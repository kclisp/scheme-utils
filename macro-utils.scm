(define gensym generate-uninterned-symbol)

;;maps macro name to its lambda
(define macros (make-strong-eq-hash-table))
(define (macro-lambda sym) (hash-table-ref macros sym))
(define (add-macro! sym f) (hash-table-set! macros sym f))

(define (macroexpand form)
  ((macro-lambda (car form)) form))

;;define unhygienic macros a la common lisp macros
(define (defmacro%% name injected-var body)
  `(begin
     (add-macro! ',name (lambda (,injected-var) ,@body))
     (define-syntax ,name
       (er-macro-transformer
        (lambda (,injected-var ,(gensym) ,(gensym))
          ,@body)))))

(define-syntax defmacro%
  (er-macro-transformer
   (lambda (form rename cmp)
     (let ((name (cadr form))
           (body (cddr form)))
       (defmacro%% name 'form body)))))                ;injects form into the macro exp

(defmacro% defmacro
  (let ((name (caadr form))
        (bvl (cdadr form))
        (body% (cddr form))
        (var (gensym)))
    (let ((body `((apply (lambda ,bvl ,@body%)
                          (cdr ,var)))))
      (defmacro%% name var body))))

;;macros
(defmacro (dispatch . fs)
  (let ((sym (gensym)))
    (let ((mfs (map (lambda (f) `((,f) ,f))
                    fs)))
      `(lambda (sym)
         (case sym
           ,@mfs)))))

(defmacro (incf sym #!optional opt-incr)
  (let ((incr (if (default-object? opt-incr)
                  1
                  opt-incr)))
    `(set! ,sym (+ ,sym ,incr))))

(defmacro (decf sym #!optional opt-decr)
  (let ((decr (if (default-object? opt-decr)
                  1
                  opt-decr)))
    `(incf ,sym ,(- decr))))

