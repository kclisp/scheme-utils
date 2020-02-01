(load-option 'format)

;;;fundamental
(define (atom? obj)
  (and (not (null? obj))
       (not (pair? obj))))

(define ((tagged-list? tag) obj)
  (and (pair? obj)
       (eq? (car obj) tag)))

;;files
(define (ls #!optional path%)
  (let ((path (if (default-object? path%)
                  "."
                  path%)))
    (directory-file-names path)))

;;;split
(define ((split-list list) succeed fail)
  (if (null? list)
      (fail)
      (succeed (car list) (cdr list))))

;;;map
;;map that accumulates multiple objects via continuations
;; and doesn't leave anything on the stack
;;proc takes in an object, ignore and success continuations
;;ignore doesn't add anything
;;success takes in the accumulated objects
(define ((map-conts proc l)
         num-success-args success)
  ((split-list l)
   (lambda (head tail)
     (let ((next (lambda (next-success)
                   ((map-conts proc tail)
                    num-success-args
                    next-success))))
       (proc head
             (lambda ()
               (next success))
             (lambda args
               (next (lambda tot-args
                       (apply success (map cons args tot-args))))))))
   (lambda ()
     (apply success (make-list num-success-args)))))

;;map-success only has success continuation
(define (map-success proc l)
  (map-conts (lambda (arg ignore success)
                 (proc arg success))
             l))

;;;condition/error handling
(define (run-on-conditions conditions thunk on-conditions)
  (call/cc
   (lambda (k)
     (bind-condition-handler
      conditions
      (lambda (condition)
        (k (on-conditions condition)))
      thunk))))

(define (run-on-error thunk on-error)
  (run-on-conditions
   (list condition-type:error)
   thunk
   on-error))
