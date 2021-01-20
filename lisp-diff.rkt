#! /usr/bin/racket
#lang racket
(require rackunit)
(require plai/datatype)
(require racket/format)
(provide (all-defined-out))

(define (or? . preds)
  (foldr (λ (acc pred) (λ (x) (or (acc x) (pred x)))) (λ (_) false) preds))


;; s-expr is one-of:
;; the empty list: '()
;; a symbol
;; (cons atom s-expr)

;; diff-output is one-of:
;; s-expr
;; (HERE s-expr s-expr)
;; complex s-expr with a HERE marker somewhere inside it.
(define-type diff-output
  [OK (val any/c)]
  ;; diff-marker
  [HERE (left any/c) (right any/c)]
  [ONLY-LEFT (left list?)]
  [ONLY-RIGHT (right list?)]
  [list-diff (diffs list?)]
  ;; otherwise it's just the expression itself
  )

;; diff and compare-lists are mutually recursive.

;; s-expr s-expr -> diff-output
(define (diff left right)
  (match `(,left ,right)
    [pair-of-lists #:when (and (list? left) (list? right))
              (compare-lists left right)]
    [`(,same ,same) (OK same)]
    #;
    [`(,(cons same-head ltail) ,(cons same-head rtail))
       (cons same-head (diff ltail rtail))]
    #;
    [`(,(cons lhead ltail) ,(cons rhead rtail))
       (cons (diff lhead rhead) (diff ltail rtail))]
    #;
    [`(,(cons lhead same-tail)
         ,(cons rhead same-tail))
       (let ([head-diff (diff lhead rhead)])
         (cons head-diff tail))
       ]
    [else
     (HERE left right)]))

;; (list-of any) (list-of any) -> list-diff
(define (compare-lists l r)
  ;; (listof any) (listof any) -> 1 listof diff-output
  (define (helper left right)
    (match `(,left . ,right)
      [`(() . ()) '()]
      [`((,head ,tail ...) . ())
       `(,(ONLY-LEFT left))]
      [`(() . (,head ,tail ...))
       `(,(ONLY-RIGHT right))]
      [`((,lhead ,ltail ...) . (,rhead ,rtail ...)) (cons (diff lhead rhead)
                                                          (helper ltail rtail))]
      ))
  (list-diff (helper l r)))

(diff '(same (nested list)) '(same (with different contents)))


(define OUTPUT-GREEN "\e[32m")
(define OUTPUT-RED "\e[31m")
(define OUTPUT-BLUE "\e[36m")
(define RESET-OUTPUT-COLOR "\e[0m")

;; string escape-sequence escape-sequence -> string
;; sets color of string to new-color, and resets to current-color after
(define (colorize new-color s current-color)
  (string-append new-color s current-color))

(define greenify ((curry string-append) OUTPUT-GREEN))
(displayln (greenify "hello in green" RESET-OUTPUT-COLOR))
(displayln "back to default color")

(define (blue s) (string-append OUTPUT-BLUE s RESET-OUTPUT-COLOR))
(define (red s) (string-append OUTPUT-RED s RESET-OUTPUT-COLOR))
(define (green s) (string-append OUTPUT-GREEN s RESET-OUTPUT-COLOR))

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


;; diff-output -> string
(define (diff-colorize tree)
  ;; diff-output -> string OR s-expr
  (define (diff-colorize-rec tree indent-depth)
    (define format-left (format-difference OUTPUT-BLUE))
    (define format-right (format-difference OUTPUT-RED))
    (define format-same (format-difference OUTPUT-GREEN))
    ;; (listof diff-output) -> 2 lists of strings?
    (define (colorize-list diffs)
      (match diffs
        ['() (double "")]
        [(list (ONLY-LEFT tail))
         (displayln tail)
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
    (define (colorize-item-in-list d)
      (match d
        [(OK same) (double (format-same same 0))]
        [(HERE left right)
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
      [(list-diff diffs) (let-values ([(left right) (colorize-list diffs)])
                           (string-append left "\n" right))]
      [(HERE left right)
       (string-append
        "\n"
        (format-left left (add1 indent-depth))
        "\n"
        (format-right right (add1 indent-depth))
        OUTPUT-GREEN
        )]
      [(OK same) (format-same same)]
      ;; should be impossible
      [else
       (error "unexpected case")]))
  
  (define (diff-colorize-main tree)
    (string-append
     OUTPUT-GREEN
     (pretty-format
      (diff-colorize-rec tree 0)
      #:mode 'display)
     RESET-OUTPUT-COLOR))
  (diff-colorize-main tree))

(define show-diff (compose displayln diff-colorize diff))
#;
(module+ test
  (check-equal? (diff '(first (second)
                              (third something))
                      '(first (second)
                              (third (something-else))))
                `(first (second)
                        (third
                         ,(HERE 'something '(something-else)))))
  (check-equal? (diff '() '()) '())
  (check-equal? (diff '(1) '(2)) `(,(HERE 1 2))
                )
  #;(check-equal? (diff-colorize '(hello my friends))
                "<span style=\"color:green\">
(hello my friends)
</span>
")
  #;(check-equal? (diff-colorize (diff `(same
                                       (same))
                                     `(same
                                       (same (but different)))))
                "<span style=\"color:green\">
(same (same . <span style=\"color:blue\">
()
(but different)
</span>))
</span>
"))

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



