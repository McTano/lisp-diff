#! /usr/bin/racket
#lang racket
(require rackunit)
(require plai/datatype)
(require)
(provide (all-defined-out))

(define (or? . preds)
  (foldr (λ (acc pred) (λ (x) (or (acc x) (pred x)))) (λ (_) false) preds))


;; Constants
(define OUTPUT-GREEN "\e[32m")
(define OUTPUT-RED "\e[31m")
(define OUTPUT-BLUE "\e[36m")
(define OUTPUT-PINK "\e[35m")
(define OUTPUT-YELLOW "\u001b[33m")
(define RESET-OUTPUT-COLOR "\e[0m")

;; s-expr is one-of:
;; the empty list: '()
;; a symbol
;; (cons atom s-expr)

(define-type diff-output
  [OK (val list?)]
  ;; diff-marker
  [HERE (left list?) (right list?)]
  #;[list-diff (diffs list?)]
  ;; diff-output can also be (listof diff-output)
  )

;; diff and compare-lists are mutually recursive.

;; s-expr s-expr -> diff-output
(define (diff left right) 
  (match `(,left . ,right)
    [`(,same . ,same) (OK `(,same))]
    [pair-of-lists #:when (and (list? left) (list? right))
                   (compare-lists left right)]
    [`((,lcar . ,lcdr) . (,rcar . ,rcdr))
     (cons (diff lcar rcar) (diff lcdr rcdr))]
    [else
     (HERE `(,left) `(,right))]))

;; (list-of any) (list-of any) -> (listof diff-output)
(define (compare-lists left right)
  (match `(,left . ,right)
    [`(() . ()) '()]
    #;[`((,head ,tail ...) . ())
       `( ,(HERE left '()))]
    #;[`(() . (,head ,tail ...))
       `(,(HERE '() right))]
    [`((,sames ..1 ,ltail ...) . (,sames2 ..1 ,rtail ...))
     #:when (equal? sames sames2)
     (cons (OK sames)
           (compare-lists ltail rtail))]
    [`((,lheads ..1 ,ltail) . (,rheads ..1 ,rtail))
     #:when (not (equal? lheads rheads))
     (append (compare-lists lheads rheads)
             (compare-lists ltail rtail))]
    [`((,lhead ,ltail ...) . (,rhead ,rtail ...))
     #:when (and (list? lhead) (list? rhead))
     (cons (compare-lists lhead rhead)
           (compare-lists ltail rtail))]
    
    
    [else
     `(,(HERE (list left)
              (list right)))]
    
    ))

(module+ test
  (check-equal? (compare-lists '([consoles [dreamcast xbox-one ps4 switch]])
                               '([consoles [dreamcast xbox-one playstation]]))
                `([,(OK '(consoles)) [,(OK '(dreamcast xbox-one))
                                      ,(HERE '(ps4 switch)
                                             '(playstation))]]))
  ;; pair handling
  (check-equal? (diff '(siskel . ebert)
                      '(ebert . roeper))
                `(,(HERE '(siskel) '(ebert)) .
                                             ,(HERE '(ebert) '(roeper))))
  (check-equal? (diff
                 '(info
                   ((players
                     [(player "brandon" 20 180) (player "kevin" 40 204) ("maxwell" 31 150)])))
                 '(thing
                   ((players
                     [(player "brandon" 20 180) (player "kevin" 400 204) ("maxwell" 31 150)]))))
                `(,(HERE '(info) '(thing))
                  ((,(OK '(players))
                    [,(OK '((player "brandon" 20 180)))
                     (,(OK '(player "kevin"))
                      ,(HERE '(40) '(400))
                      ,(OK '(204)))
                     ,(OK '(("maxwell" 31 150)))]))))
  
  (check-equal? (diff '(same (nested list)) '(same (with different contents)))
                `(,(OK '(same))
                  ,(HERE '((nested list)) '((with different contents)))))
  (check-equal? (diff '() '()) (OK '(())))
  (check-equal? (diff '() '(bless you)) (HERE '(()) '((bless you))))
  (check-equal? (diff '(right is empty) '()) (HERE '((right is empty)) '(())))
  )

;; list-of diff-output -> list-of diff-output
;; merge adjacent OKs and HEREs
(define (merge-diffs ls)
  (match ls
    ['() '()]
    [`(,(OK oks) ..1 ,tail ...)
     `(,(OK (foldr append '() oks))
       ,@(merge-diffs tail))]
    [(list (HERE `(,ls) `(,rs)) ..1 tail ... )
     `(,(HERE ls rs) ,@(merge-diffs tail))]
    ))
  

(module+ test
  (check-equal? (merge-diffs (list (OK '(there)) (OK '(are)) (OK '(four)) (HERE '(waffles) '(lights))))
                (list (OK '(there are four)) (HERE '(waffles) '(lights)))))


;; used to repeat a string n times as a new string.
;; like the * method in ruby.
(define (*str s n)
  (cond
    [(<= n 0) ""]
    [else
     (string-append s (*str s (- n 1)))]
    )
  )

(define SPACE " ")
(define INDENT-CHAR SPACE)
(define INDENT-WIDTH 2)
(define INDENT (*str INDENT-CHAR INDENT-WIDTH))

;; number string -> string
(define (indent-to indent-depth str)
  (let ([lines (string-split str "\n")])
    (string-join (map (λ (s) (string-append (*str INDENT indent-depth)
                                            s))
                      lines)
                 "\n")))

;; There's a bug in here dropping some brackets.

(define (colorize-diff tree)
  (define (colorize-diff-rec tree color-context)
    (match tree
      ;; how do lists work now?
      [(OK `(,same ...)) `(,OUTPUT-GREEN ,@same ,color-context)]
      [(HERE left right) `(,OUTPUT-BLUE ,@left
                                        ,OUTPUT-RED ,@right
                                        ,color-context)]
      [(list things ...) (append (append-map
                                  (λ (expr) (colorize-diff-rec expr color-context))
                                  things)
                                 `(,color-context))]
      [(cons head tail) (append (colorize-diff-rec head color-context)
                                (colorize-diff-rec tail color-context))]
      [else (error "don't understand how to colorize" tree)]))
  (define (colorize-diff-main tree)
    (match tree
      [(OK `(,matching-values ...)) `(,OUTPUT-GREEN ,@(string-join matching-values " "))]
      [(HERE left right) `(,OUTPUT-BLUE ,@left
                                        ,OUTPUT-RED ,@right
                                        ,RESET-OUTPUT-COLOR)]
      [(cons head tail) (string-append
                         OUTPUT-GREEN
                         (pretty-format
                          (append (colorize-diff-rec head OUTPUT-GREEN)
                                  (colorize-diff-rec tail OUTPUT-GREEN))
                          #:mode 'display)
                         RESET-OUTPUT-COLOR
                         )]
      [else (string-append OUTPUT-GREEN
                           (pretty-format tree #:mode 'display)
                           RESET-OUTPUT-COLOR)]))
  (colorize-diff-main tree))

(define show-diff (compose display colorize-diff diff))

#;(module+ test
    (show-diff  `(same
                  (same))
                `(same
                  (same '(but different))))

    (show-diff '(first (second)
                       (third something))
               '(first (second)
                       (third (something-else))))

    (show-diff '(same up to here then (list with multiple differences))
               '(same up to here then (and different length)))
    (show-diff '(when the head is different and the tail is the same)
               '(but the head is different and the tail is the same))
    (show-diff 'single-symbol 'comparison)
    (show-diff '(we . compare)'(a . pair))
    (show-diff '(second-item . matches)'(pair-cdr . matches))
    ;; complex structure with a small difference higher up, then another difference lower down
    (show-diff '(same (nested list)) '(same (with different contents)))
    (show-diff
     '(info
       ((players
         [(player "brandon" 20 180) (player "kevin" 40 204) ("maxwell" 31 150)])))
     '(thing
       ((players
         [(player "brandon" 20 180) (player "kevin" 400 204) ("maxwell" 31 150)]))))
    )

