;;;; Applicability

;;; An applicability attribute is a list of lists, representing
;;; an OR of some per-argument ANDs.
(define (predicates-match? predicates args)
  (and (n:= (length predicates) (length args))
       (every (lambda (predicate arg)
                (predicate arg))
              predicates
              args)))

(define (match-args . predicates)
  (list predicates))

(define (all-args arity predicate)
  (list (make-list arity predicate)))

(define (any-arg arity predicate base-predicate)
  (if (n:= 0 arity)
      (list)
      (cdr (all-sequences-of arity base-predicate predicate))))

(define (applicability-union . applicabilities)
  (applicability-union* applicabilities))

(define (applicability-union* applicabilities)
  (apply lset-union equal? applicabilities))
