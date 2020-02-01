;;;; List utilities
(define (all-sequences-of arity zero one)
  (map (lambda (index)
         (index->choices index arity zero one))
       (iota (n:expt 2 arity))))

(define (index->choices index arity zero one)
  (let loop ((i 0) (index index) (choices '()))
    (if (n:< i arity)
        (loop (n:+ i 1)
              (quotient index 2)
              (cons (if (odd? index) one zero)
                    choices))
        choices)))

;;; MIT/GNU Scheme implementation specific:
(define microcode-type/code->name
  (access microcode-type/code->name (->environment '(runtime))))

;;; This removes those annoying hash numbers after ;Value:
(set! repl:write-result-hash-numbers? #f)
