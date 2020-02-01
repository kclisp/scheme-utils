(define (at dir proc)
  (let ((here (pwd)))
    (dynamic-wind
        (lambda () (cd dir))
        proc
        (lambda () (cd here)))))
(define (load-at dir pathname . args)
  (at dir
      (lambda () (apply load pathname args))))
(at "~/coding/scheme/utils"
    (lambda ()
      (load-at "generic-procs" "load")
      (load '("utils" "macro-utils" "test"))))
