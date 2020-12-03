#lang racket
(require rackunit)
(require plai/datatype)
(provide (all-defined-out))

(define (or? . preds)
  (foldr (λ (acc pred) (λ (x) (or (acc x) (pred x)))) (λ (_) false) preds))


;; s-expr is one-of:
;; '()
;; symbol
;; (cons atom s-expr)

;; diff-output is one-of:
;; s-expr
;; (HERE)
(define-type diff-output
  [HERE (left any/c) (right any/c)]
  [OK (inner any/c)]
  )

;; s-expr s-expr -> diff-output

(define (diff left right)
  (match `(,left ,right)
    [`(,same ,same) same]
    [`(,(cons head ltail) ,(cons head rtail))
     (cons head (diff ltail rtail))]
    [`(,(cons lhead ltail) ,(cons rhead rtail))
     (cons (diff lhead rhead) (diff ltail rtail))]
    [else
     (HERE left right)]))

(define set-green "\e[32m")
(define set-red "\e[31m")
(define set-blue "\e[36m")
(define reset-color "\e[0m")


(define (diff-colorize tree)
  (define (diff-colorize-rec tree)
    (match tree
      [(HERE left right)
       (~a set-blue
           (~a left)
           set-red
           (~a right)
           set-green
           #:separator "\n")]
       
      [(cons head tail)  (cons (diff-colorize-rec head) (diff-colorize-rec tail))]
      [atom atom]
      [else
       (error "unexpected case")]))
  
   (~a
    set-green
     (diff-colorize-rec tree)
            reset-color
       
  #:separator "\n"))


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




;; 