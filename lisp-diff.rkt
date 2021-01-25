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

(define (list-diff-contents? contents)
  (and (list? contents)
       (andmap (λ (el)
                 (or (list-diff? el)
                     (pair? el)
                     (SAME*? el)
                     (DIFFERENT*? el)))
               contents)))

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
  [list-diff (diffs list-diff-contents?)]
  ;; could also be:
  ;; (cons diff-output diff-output)
  )

;; diff and compare-lists are mutually recursive.

;; s-expr s-expr -> diff-output
(define (diff left right) 
  (match `(,left . ,right)
    [`(,same . ,same) (SAME same)]
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
      ;; pair
      [`((,l1 . ,l2) (,r1 . r2))
       #:when (and (not (list? left))
                   (not (list? right)))
       `(,(diff l1 r1)
         . (diff l2 r2))]
      ;; lists begin with some matching values
      [`((,lheads ..1 ,ltail ...) . (,rheads ..1 ,rtail ...))
       #:when (equal? lheads rheads)
       (cons (SAME* lheads)
             (compare-partials ltail rtail))]
      ;; lists starts with (unequal) lists
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
  (check-equal? (diff '(siskel . ebert)
                      '(ebert . roeper))
                `(,(DIFFERENT 'siskel 'ebert) . ,(DIFFERENT 'ebert 'roeper)))
  (check-equal? (diff '() '()) (SAME '()))
  (check-equal? (diff '() '(bless you)) (list-diff
                                         (list (DIFFERENT* '() '(bless you)))))
  (check-equal? (diff '(right is empty) '()) (list-diff
                                              (list (DIFFERENT* '(right is empty) '()))))

  (check-equal? (diff '[dreamcast xbox-one ps4 switch]
                      '[dreamcast xbox-one playstation])
                (list-diff
                 [list (SAME* '(dreamcast xbox-one))
                       (DIFFERENT* '(ps4) '(playstation))
                       (DIFFERENT* '(switch) '())]))
  (check-equal? (diff '([consoles [dreamcast xbox-one ps4 switch]])
                      '([consoles [dreamcast xbox-one playstation]]))
                (list-diff `(,(list-diff `[,(SAME* '(consoles))
                                           ,(list-diff
                                             [list (SAME* '(dreamcast xbox-one))
                                                   (DIFFERENT* '(ps4) '(playstation))
                                                   (DIFFERENT* '(switch) '())])]))))
  (check-equal? (diff
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
  (check-equal? (diff '(same (nested list)) '(same (with different contents)))
                (list-diff
                 (list
                  (SAME* '(same))
                  (DIFFERENT* '((nested list)) '((with different contents))))))
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
      [(DIFFERENT left right)  (~a ""
                                   (~a (~a OUTPUT-BLUE
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

(define show-diff (compose displayln colorize-diff diff))

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
  (show-diff  `(same
                (same))
              `(same
                (same (but different))))

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
  (show-diff '(second-item . matches) '(pair-cdr . matches))
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

