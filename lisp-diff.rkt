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
  ;; diff-marker
  [HERE (left any/c) (right any/c)]
  ;; otherwise it's just the expression itself
  )

;; s-expr s-expr -> diff-output

(define (diff left right)
  (match `(,left ,right)
    [`(,same ,same) same]
    [`(,(cons same-head ltail) ,(cons same-head rtail))
     (cons same-head (diff ltail rtail))]
    [`(,(cons lhead ltail) ,(cons rhead rtail))
     (cons (diff lhead rhead) (diff ltail rtail))]
    #;[`(,(cons lhead same-tail)
         ,(cons rhead same-tail))
       (let ([head-diff (diff lhead rhead)])
         (cons head-diff tail))
       ]
    [else
     (HERE left right)]))

(define OUTPUT-GREEN "\e[32m")
(define set-red "\e[31m")
(define set-blue "\e[36m")
(define RESET-OUTPUT-COLOR "\e[0m")

;; used to repeat a string n times as a new string.
;; like the * method in ruby.
(define (*str s n)
  (cond
    [(<= n 0) ""]
    [else
     (string-append s (*str s (- n 1)))]
    )
  )

(define INDENT (*str " " 2))

;; number string -> string
(define (indent-to indent-depth str)
  (let ([lines (string-split str "\n")])
    (string-join (map (λ (s) (string-append (*str INDENT indent-depth)
                                            s))
                      lines)
                 "\n")))

;; (color escape marker) s-expr number -> str
(define (format-difference color expr indent-depth)
  (string-append color
                 (indent-to (add1 indent-depth)
                            (stringify expr))
                 "\n")
  )

(define format-left ((curry format-difference) set-blue))
(define format-right ((curry format-difference) set-red))

(define (stringify sexpr)
  (pretty-format sexpr #:mode 'display))

;; diff-output -> string
(define (diff-colorize tree)
  ;; diff-output -> string OR s-expr
  (define (diff-colorize-rec tree indent-depth)
    (match tree
      ;; special cases to handle the end of a list
      [(HERE '() right)
       (list (string-append "\n"
                            (format-left '() indent-depth)
                            (format-right right indent-depth)
                            OUTPUT-GREEN))]
      [(HERE left '())
       (list (string-append "\n"
                            (format-left left indent-depth)
                            (format-right '() indent-depth)
                            OUTPUT-GREEN))]
      [(HERE left right)
       (string-append
        "\n"
        (indent-to (add1 indent-depth) (format-left left indent-depth))
        "\n"
        (indent-to (add1 indent-depth) (format-right right indent-depth))
        "\n"
        OUTPUT-GREEN
        )]
      #;
      (match head
        [(HERE left right)
         (string-append  (diff-colorize-rec left indent-depth)
                         (diff-colorize-rec tail indent-depth))
         (string-append (diff-colorize-rec right indent-depth)
                        (diff-colorize-rec tail indent-depth))]
        [head (string-append (diff-colorize-rec head indent-depth) (diff-colorize-rec tail indent-depth))])
      ;; Cases
      ;; head and tail are one-of:
      ;;   (HERE left right)
      ;;   s-expr containing diff-marker somewhere within
      ;;   s-expr containing no diff-markers
      [(cons head tail)
       (cons (diff-colorize-rec head indent-depth)
             (diff-colorize-rec tail indent-depth))]
      ;;  should match anything
      [atom atom]
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

;; string escape-sequence escape-sequence -> string
;; sets color of string to new-color, and resets to current-color after
(define (colorize new-color s current-color)
  (string-append new-color s current-color))

(define greenify ((curry colorize) OUTPUT-GREEN))
(displayln (greenify "hello in green" RESET-OUTPUT-COLOR))
(displayln "back to default color")

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
  (check-equal? (diff-colorize '(hello my friends))
                "<span style=\"color:green\">
(hello my friends)
</span>
")
  (check-equal? (diff-colorize (diff `(same
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

(show-diff '(same up to here then (HERE different stuff))
           '(same up to here then (different stuff)))
(show-diff '(when the head is different and the tail is the same)
           '(but the head is different and the tail is the same))




