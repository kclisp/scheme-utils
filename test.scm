(define *quiet-testing* #t)

(define (output-failed-test name answer result args)
  (format #t "Failed test: ~a~%  Expected: ~a~%  Got: ~a~%  Args: ~a~%~%"
          name answer result args))

(define (make-tester% func name args answer)
  (let ((result (apply func args)))
    (if (equal? answer result)
        (or *quiet-testing*
            (format #t "Passed test: ~a~%~%" name))
        (output-failed-test name answer result args))))

;;todo: check output (warn and pp)
;;maybe eventually define equality relation
(define (((make-tester func) name . args) answer #!optional when-error)
  (run-on-error
   (lambda ()
     (make-tester% func name args answer))
   (lambda (condition)
     (if (default-object? when-error)
         (output-failed-test name answer
                             (format #f "ERROR - ~a" (condition/report-string condition))
                             args)
         (((make-tester when-error) name condition) answer)))))

(define *tests* '())
(define (add-test! test)
  (set! *tests* (cons test *tests*))
  'added-test)

(define (run-test test)
  (test))
(define (run-tests)
  (for-each run-test (reverse *tests*)))
(define (run-top-test)
  (run-test (car *tests*)))

