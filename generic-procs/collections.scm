;;;; Collections
(define (make-hash-table-store make-table)
  (let ((table (make-table)))

    (define (get-keys)
      (hash-table-keys table))

    (define (has? key)
      (hash-table-exists? table key))

    (define (get key)
      (hash-table-ref table key))

    (define (put! key metadata)
      (hash-table-set! table key metadata))

    (lambda (operator)
      (case operator
        ((get-keys) get-keys)
        ((has?) has?)
        ((get) get)
        ((put!) put!)
        (else (error "Unknown operator:" operator))))))

(define (make-metadata-association)
  (let* ((store
          (make-hash-table-store make-key-weak-eqv-hash-table))
         (base-has? (store 'has?))
         (base-get (store 'get))
         (base-put! (store 'put!)))

    (define (put! key metadata)
      (if (base-has? key)
          (let ((metadata* (base-get key)))
            (if (not (eqv? metadata* metadata))
                (error "Can't change metadata for:"
                       key metadata metadata*))))
      (base-put! key metadata))

    (lambda (operator)
      (case operator
        ((put!) put!)
        (else (store operator))))))
