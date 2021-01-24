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
  [SAME* (vals list?)]
  [SAME (val any/c)]
  #;[SAME-VALUES (val list?)]
  ;; diff-marker
  [DIFFERENT* (left list?) (right list?)]
  [DIFFERENT (left any/c) (right any/c)]
  ;; diff-output can also be (listof diff-output)
  [list-diff (diffs list?)]
  ;; could also be:
  ;; (cons diff-output diff-output)
  )

;; diff and compare-lists are mutually recursive.

;; s-expr s-expr -> diff-output
(define (diff left right) 
  (match `(,left . ,right)
    [`(,same . ,same) (SAME* same)]
    [pair-of-lists #:when (and (list? left) (list? right))
                   (compare-lists left right)]
    [`((,lcar . ,lcdr) . (,rcar . ,rcdr))
     (cons (diff lcar rcar) (diff lcdr rcdr))]
    [else
     (DIFFERENT left right)]))

(define (compare-lists left right)
  ;; list -> (list-of diff-output)
  (define (compare-partials left right)
    (match `(,left . ,right)
      [`(() . ()) '()]
      [`((,lheads ..1 ,ltail ...) . (,rheads ..1 ,rtail ...))
       #:when (equal? lheads rheads)
       (cons (SAME* lheads)
              (compare-partials ltail rtail))]
      [`((,lhead ,ltail ...) . (,rhead ,rtail ...))
       #:when (and (list? lhead)
                   (list? rhead))
       (cons (compare-lists lhead rhead)
             (compare-partials ltail rtail))]
      [`((,lheads oo1 ,ltail ...) . (,rheads oo1 ,rtail ...))
       #:when (and (equal? (length lheads)
                           (length rheads))
                   (andmap (λ (a b) (not (equal? a b))) lheads rheads))
       (cons (DIFFERENT* lheads rheads)
             (compare-partials ltail rtail))]
      [`((,lhead ,ltail ...) . (,rhead ,rtail ...))
       (cons (diff lhead rhead)
             (compare-partials ltail rtail))]
      [else
       (displayln (~a left right #:separator "\n"))
       `(,(DIFFERENT* left right))]
      )
    )
  ;; list -> diff-output
  (match `(,left . ,right)
    [`(() ()) `()]
    [`(,same . ,same) (SAME left)]
    [else (list-diff (compare-partials left right))]))


(module+ test
  ;; pair handling
  (check-equal? (diff '(siskel . ebert)
                      '(ebert . roeper))
                `(,(DIFFERENT 'siskel 'ebert) . ,(DIFFERENT 'ebert 'roeper)))
  (check-equal? (diff '() '()) (SAME* '()))
  (check-equal? (diff '() '(bless you)) (list-diff
                                         (list (DIFFERENT* '() '(bless you)))))
  (check-equal? (diff '(right is empty) '()) (list-diff
                                              (list (DIFFERENT* '(right is empty) '()))))

  (check-equal? (diff '[dreamcast xbox-one ps4 switch]
                      '[dreamcast xbox-one playstation])
                (list-diff
                 `[,(SAME* '(dreamcast xbox-one))
                   ,(DIFFERENT* '(ps4 switch)
                                '(playstation))]))
  (check-equal? (diff '([consoles [dreamcast xbox-one ps4 switch]])
                      '([consoles [dreamcast xbox-one playstation]]))
                (list-diff `(,(list-diff `[,(SAME* '(consoles))
                                           ,(list-diff
                                             `[,(SAME* '(dreamcast xbox-one))
                                               ,(DIFFERENT* '(ps4 switch)
                                                            '(playstation))])]))))
  (check-equal? (diff '(info
                        ((players
                          ((player "brandon" 20 180)
                           (player "kevin" 40 204)
                           ("maxwell" 31 150)))))
                      '(thing
                        ((players
                          ((player "brandon" 20 180)
                           (player "kevin" 400 204)
                           ("maxwell" 31 150))))))
                (list-diff (list (DIFFERENT*
                                   '(info) '(thing))
                                 `((players ...)))))
  (check-equal? (diff
                 '(info
                   ((players
                     [(player "brandon" 20 180) (player "kevin" 40 204) ("maxwell" 31 150)])))
                 '(thing
                   ((players
                     [(player "brandon" 20 180) (player "kevin" 400 204) ("maxwell" 31 150)]))))
                `(,(DIFFERENT* '(info) '(thing))
                  ((,(SAME* '(players))
                    [,(SAME* '((player "brandon" 20 180)))
                     (,(SAME* '(player "kevin"))
                      ,(DIFFERENT* '(40) '(400))
                      ,(SAME* '(204)))
                     ,(SAME* '(("maxwell" 31 150)))]))))
  (check-equal? (diff '(same (nested list)) '(same (with different contents)))
                `(,(SAME* '(same))
                  ,(DIFFERENT* '((nested list)) '((with different contents)))))
  )

;; list-of diff-output -> list-of diff-output
;; merge adjacent OKs and HEREs
(define (merge-diffs ls)
  (match ls
    ['() '()]
    [`(,(SAME* oks) ..1 ,tail ...)
     `(,(SAME* (foldr append '() oks))
       ,@(merge-diffs tail))]
    [(list (DIFFERENT* `(,ls) `(,rs)) ..1 tail ... )
     `(,(DIFFERENT* ls rs) ,@(merge-diffs tail))]
    ))
  

(module+ test
  (check-equal? (merge-diffs (list (SAME* '(there)) (SAME* '(are)) (SAME* '(four)) (DIFFERENT* '(waffles) '(lights))))
                (list (SAME* '(there are four)) (DIFFERENT* '(waffles) '(lights)))))


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
      [(SAME* `(,same ...)) `(,OUTPUT-GREEN ,@same ,color-context)]
      [(DIFFERENT* left right) `(,OUTPUT-BLUE ,@left
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
      [(SAME* `(,matching-values ...)) `(,OUTPUT-GREEN ,@(string-join matching-values " "))]
      [(DIFFERENT* left right) `(,OUTPUT-BLUE ,@left
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

