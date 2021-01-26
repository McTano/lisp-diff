#! /usr/bin/racket
#lang racket
(require rackunit)
(require plai/datatype)
(provide (rename-out [display-diff display-lisp-diff])
                     )

;; Constants
(define OUTPUT-GREEN "\e[32m")
(define OUTPUT-RED "\e[31m")
(define OUTPUT-BLUE "\e[36m")
(define OUTPUT-PINK "\e[35m")
(define OUTPUT-YELLOW "\u001b[33m")
(define RESET-OUTPUT-COLOR "\e[0m")

;; predicate
;; checks whether a value is valid as contents of list-diff
(define (list-diff-contents? contents)
  (and (list? contents)
       (andmap (λ (el)
                 (or (list-diff? el)
                     (pair? el)
                     (partial? el)))
               contents)))

;; representation of expression with differences highlighted
(define-type diff-output
  [SAME (val any/c)]
  [DIFFERENT (left any/c) (right any/c)]
  ;; diff-output can also be (listof diff-output)
  [list-diff (diffs list-diff-contents?)]
  ;; could also be a pair:
  ;; (cons diff-output diff-output)
  )

;; represent portion of a list containing sub-expressions
;; to get same color.
;; Should only appear inside 'contents field of lisp-diff
(define-type partial
  [SAME* (vals list?)]
  [DIFFERENT* (left list?) (right list?)]
  )

;; s-expr s-expr -> diff-output
;; return diff-output representing differences
;; compare-expressions and compare-lists are mutually recursive.
(define (compare-expressions left right) 
  (match `(,left . ,right)
    [`(,same . ,same) (SAME same)]
    [pair-of-lists #:when (and (list? left) (list? right))
                   (compare-lists left right)]
    [`((,lcar . ,lcdr) . (,rcar . ,rcdr))
     (cons (compare-expressions lcar rcar) (compare-expressions lcdr rcdr))]
    [else
     (DIFFERENT left right)]))

;; list -> list-diff
(define (compare-lists left right)
  ;; list -> (list-of diff-output)
  (define (compare-partials left right)
    (match `(,left . ,right)
      [`(() . ()) '()]
      ;; pair which is not a list
      [`((,l1 . ,l2) (,r1 . r2))
       #:when (and (not (list? left))
                   (not (list? right)))
       `(,(compare-expressions l1 r1)
         . (diff l2 r2))]
      ;; lists begin with some matching values
      [`((,lheads ..1 ,ltail ...) . (,rheads ..1 ,rtail ...))
       #:when (equal? lheads rheads)
       (cons (SAME* lheads)
             (compare-partials ltail rtail))]
      ;; lists both have a list as first element
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
       #:when (not (equal? lhead rhead))
       (cons (DIFFERENT* `(,lhead) `(,rhead))
             (compare-partials ltail rtail))]
      [else
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
  (check-equal? (compare-expressions '(siskel . ebert)
                      '(ebert . roeper))
                `(,(DIFFERENT 'siskel 'ebert) . ,(DIFFERENT 'ebert 'roeper)))
  (check-equal? (compare-expressions '() '()) (SAME '()))
  (check-equal? (compare-expressions '() '(bless you)) (list-diff
                                         (list (DIFFERENT* '() '(bless you)))))
  (check-equal? (compare-expressions '(right is empty) '()) (list-diff
                                              (list (DIFFERENT* '(right is empty) '()))))

  (check-equal? (compare-expressions '[dreamcast xbox-one ps4 switch]
                      '[dreamcast xbox-one playstation])
                (list-diff
                 [list (SAME* '(dreamcast xbox-one))
                       (DIFFERENT* '(ps4) '(playstation))
                       (DIFFERENT* '(switch) '())]))
  (check-equal? (compare-expressions '([consoles [dreamcast xbox-one ps4 switch]])
                      '([consoles [dreamcast xbox-one playstation]]))
                (list-diff `(,(list-diff `[,(SAME* '(consoles))
                                           ,(list-diff
                                             [list (SAME* '(dreamcast xbox-one))
                                                   (DIFFERENT* '(ps4) '(playstation))
                                                   (DIFFERENT* '(switch) '())])]))))
  (check-equal? (compare-expressions
                 '(info
                   ((players
                     [(player "brandon" 20 180)
                      (player "kevin" 40 204)
                      ("maxwell" 31 150)])))
                 '(thing
                   ((players
                     [(player "brandon" 20 180)
                      (player "kevin" 400 204)
                      ("maxwell" 31 150)]))))
                (list-diff
                 (list
                  (DIFFERENT* '(info) '(thing))
                  (list-diff
                   (list
                    (list-diff
                     (list
                      (SAME* '(players))
                      (list-diff
                       [list
                        (SAME* '((player "brandon" 20 180)))
                        (list-diff
                         (list
                          (SAME* '(player "kevin"))
                          (DIFFERENT* '(40) '(400))
                          (SAME* '(204))))
                        (SAME* '(("maxwell" 31 150)))]))))))))
  (check-equal? (compare-expressions '(same (nested list)) '(same (with different contents)))
                (list-diff
                 (list
                  (SAME* '(same))
                  (DIFFERENT* '((nested list)) '((with different contents))))))
  )

;; diff-output -> string
(define (colorize-diff tree)
  ;; partial -> list
  (define (colorize-partial part color-context)
    (match part
      [(SAME* `(,same-values ...)) `(,OUTPUT-GREEN
                                     ,@same-values
                                     ,color-context )]
      [(DIFFERENT* left right)     `(,OUTPUT-BLUE
                                     ,@left
                                     ,OUTPUT-RED
                                     ,@right
                                     ,color-context)]
      [lsd #:when (list-diff? lsd) (list (colorize-list-diff lsd color-context))]
      ;; TODO: handle pairs
      #;[(cons fst snd) #:when (not (list? tree))
                        (error "TODO; Handle pairs like " part)
                        (list (cons (colorize-diff-rec fst color-context)
                                    (colorize-diff-rec snd color-context)))]
      [else (error "expected SAME* or DIFFERENT*, got " part)]))
  ;; list-diff -> list
  (define (colorize-list-diff lsd color-context)
    (match lsd
      [(list-diff contents) #:when (list? contents)
                            (foldr (λ (d acc) (append
                                               (colorize-partial d color-context)
                                               acc)) '() contents)]
      [else (error "expected list-diff containing list, got: " lsd)]
      ))
  ;; diff-output -> string
  (define (colorize-diff-rec tree color-context)
    (match tree
      [(SAME val) (~a OUTPUT-GREEN (~a val #:separator " ") color-context)]
      [(DIFFERENT left right)  (~a (~a (~a OUTPUT-BLUE
                                           left)
                                       (~a OUTPUT-RED
                                           right)
                                       #:separator " ")
                                   color-context)]

      [lsd #:when (list-diff? lsd) (~a (colorize-list-diff lsd color-context))]
      [(cons d1 d2) `(,(colorize-diff-rec d1 color-context) . ,(colorize-diff-rec d2 color-context))]
      [else (error "Unexpected input: " tree)]))
  (~a OUTPUT-GREEN
      (colorize-diff-rec tree OUTPUT-GREEN)
      RESET-OUTPUT-COLOR
      "\n")
  )

(define display-diff (compose displayln colorize-diff compare-expressions))
(define lisp-diff (compose colorize-diff compare-expressions))

(module+ test
  (check-equal? (colorize-diff
                 (list-diff (list
                             (DIFFERENT* '(a b c) '(1 2 3))
                             (SAME* '(end of list)))))
                (~a OUTPUT-GREEN
                    (list
                     OUTPUT-BLUE 'a 'b 'c
                     OUTPUT-RED 1 2 3
                     OUTPUT-GREEN
                     OUTPUT-GREEN 'end 'of 'list
                     OUTPUT-GREEN
                     )
                    RESET-OUTPUT-COLOR
                    "\n"))
  (display-diff  `(same
                (same))
              `(same
                (same (but different))))

  (display-diff '(first (second)
                     (third something))
             '(first (second)
                     (third (something-else))))

  (display-diff '(same up to here then (list with multiple differences))
             '(same up to here then (and different length)))
  (display-diff '(when the head is different and the tail is the same)
             '(but the head is different and the tail is the same))
  (display-diff 'single-symbol 'comparison)
  (display-diff '(we . compare)'(a . pair))
  (display-diff '(second-item . matches) '(pair-cdr . matches))
  ;; complex structure with a small difference higher up, then another difference lower down
  (display-diff '(same (nested list)) '(same (with different contents)))
  (display-diff
   '(info
     ((players
       [(player "brandon" 20 180) (player "kevin" 40 204) ("maxwell" 31 150)])))
   '(thing
     ((players
       [(player "brandon" 20 180) (player "kevin" 400 204) ("maxwell" 31 150)]))))
  )

