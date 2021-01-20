#! /usr/bin/racket
#lang racket
(require rackunit)
(require plai/datatype)
(require racket/format)
(provide (all-defined-out))

(define (or? . preds)
  (foldr (λ (acc pred) (λ (x) (or (acc x) (pred x)))) (λ (_) false) preds))


;; Constants
(define OUTPUT-GREEN "\e[32m")
(define OUTPUT-RED "\e[31m")
(define OUTPUT-BLUE "\e[36m")
(define RESET-OUTPUT-COLOR "\e[0m")

;; s-expr is one-of:
;; the empty list: '()
;; a symbol
;; (cons atom s-expr)

;; diff-output is one-of:
;; s-expr
;; (HERE s-expr s-expr)
;; complex s-expr with a HERE marker somewhere inside it.
(define-type diff-output
  [OK (val list?)]
  ;; diff-marker
  [HERE (left list?) (right list?)]
  [ONLY-LEFT (left list?)]
  [ONLY-RIGHT (right list?)]
  #;[list-diff (diffs list?)]
  ;; otherwise it's just the expression itself
  )

;; diff and compare-lists are mutually recursive.

;; s-expr s-expr -> diff-output
(define (diff left right)
  (match `(,left . ,right)
    [`(,same . ,same) (OK `(,same))]
    [pair-of-lists #:when (and (list? left) (list? right))
                   (compare-lists left right)]
    [`((,lcar . ,lcdr) . (,rcar . ,rcdr))
     `(,(diff lcar rcar) . ,(diff lcdr rcdr))]
    [else
     (HERE `(,left) `(,right))]))

;; (list-of any) (list-of any) -> list-diff
(define (compare-lists l r)
  ;; (listof any) (listof any) -> 1 listof diff-output
  (define (helper left right)
    (match `(,left . ,right)
      [`(() . ()) '()]
      [`((,head ,tail ...) . ())
      `( ,(ONLY-LEFT left))]
      [`(() . (,head ,tail ...))
       `(,(ONLY-RIGHT right))]
      [`((,sames ..1 ,ltail ...) . (,sames2 ..1 ,rtail ...))
       #:when (equal? sames sames2)
       (cons (OK sames)
             (helper ltail rtail))]
      [`((,lhead ,ltail ...) . (,rhead ,rtail ...))
       (cons (diff lhead rhead)
             (helper ltail rtail))]
      [`((,lheads ..1 ,ltail ...) . (,rheads ..1 ,rtail ...))
       #:when (not (equal? lheads rheads))
       (cons (HERE lheads rheads)
             (helper ltail rtail))]
      ))
  (helper l r))

(module+ test
  (check-equal? (compare-lists '([consoles [dreamcast xbox-one ps4 switch]])
                               '([consoles [dreamcast xbox-one playstation]]))
                `([,(OK '(consoles)) [,(OK '(dreamcast xbox-one)) ,(HERE '(ps4) '(playstation))
                                                                  ,(ONLY-LEFT '(switch))]]))
  )


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

#;(diff '(same (nested list)) '(same (with different contents)))

(module+ test
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
  (diff '(same (nested list)) '(same (with different contents)))
  )

;; string escape-sequence escape-sequence -> string
;; sets color of string to new-color, and resets to current-color after
(define (colorize new-color s current-color)
  (string-append new-color s current-color))

(define greenify ((curry string-append) OUTPUT-GREEN))
(displayln (greenify "hello in green" RESET-OUTPUT-COLOR))
(displayln "back to default color")

(define (blue s) (string-append OUTPUT-BLUE s RESET-OUTPUT-COLOR))
(define (red s) (string-append OUTPUT-RED s RESET-OUTPUT-COLOR))
#;(define (green s) (string-append OUTPUT-GREEN s RESET-OUTPUT-COLOR))

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

;; (color escape marker) s-expr number -> str
(define format-difference (curry (λ (color expr indent-depth)
                                   (string-append color
                                                  (indent-to
                                                   indent-depth
                                                   (stringify expr))
                                                  SPACE))))



(define (stringify sexpr)
  (pretty-format sexpr #:mode 'display))

;; expr -> 2 of the expression
;; return the given expression twice
(define (double expr)
  (values expr expr))

    (define format-left (format-difference OUTPUT-BLUE))
    (define format-right (format-difference OUTPUT-RED))
    (define format-same (format-difference OUTPUT-GREEN))

;; TODO fix this for new representation.
;; It should be simpler now so maybe I should just start over on the output.

(define (diff-colorize tree)
  (match tree
   ;; how do lists work now?
    [(OK same) (format-same (foldl string-append "" same))]
    [else (stringify tree)]))

;; diff-output -> string
#;
(define (diff-colorize tree)
  ;; diff-output -> string
  (define (diff-colorize-rec tree indent-depth)

    ;; (listof diff-output) -> 2 lists of strings?
    #;(define (colorize-list diffs)
        (match diffs
          ['() (double "")]
          [(list (ONLY-LEFT tail))
           (values
            (format-left tail 0)
            "")]
          [(list (ONLY-RIGHT tail))
           (values
            ""
            (format-right tail 0))]
          [`(,head ,tail ...)
           (let-values ([(left-head right-head) (colorize-item-in-list head)]
                        [(left-tail right-tail) (colorize-list tail)])
             (values
              (string-append left-head left-tail)
              (string-append right-head right-tail)
              ))]))
    ;; diff-output -> 2 strings
    #;(define (colorize-item-in-list d)
        (match d
          [(OK `(,same)) (double (format-same same 0))]
          [(HERE `(,left) `(,right))
           (values
            (format-left left 0)
            (format-right right 0)
            )]
          [(list-diff diffs)
           (colorize-list diffs)]
          ))
    ;; BODY OF diff-colorize-rec starts here
    (match tree
      ;; Alternate list behaviour
      #;[(list-diff diffs) (let-values ([(left right) (colorize-list diffs)])
                             (format-same (list left "\n" right) indent-depth))]
      [(HERE `(,left) `(,right))
       (stringify (list
                   (format-left left (add1 indent-depth))
        
                   (format-right right (add1 indent-depth))
                   OUTPUT-GREEN
                   ))]
      [`(,contents ...) (foldr (λ (x acc)
                                 (string-append (diff-colorize-rec x indent-depth) acc))
                               ""
                               contents)]
      [(OK `(,same ...)) (format-same (stringify same) indent-depth)]
      [(ONLY-RIGHT `(,right ...)) (format-right right indent-depth)]
      [(ONLY-LEFT `(,left ...)) (format-left left indent-depth)]
      ;; should be impossible
      [`(,card . ,cdrd) (stringify (cons
                                    (diff-colorize-rec card indent-depth)
                                    (diff-colorize-rec cdrd indent-depth)))]
      [else
       (error "unexpected case" tree)]))
  
  
  (diff-colorize-rec tree 0))

(define show-diff (compose displayln diff-colorize diff))

(module+ test
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

