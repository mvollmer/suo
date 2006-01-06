;;; Toplevel definers

(define-macro (define-suo-macro args . body)
  `(register-macro ',(car args)
		   (lambda (form)
		     (apply (lambda ,(cdr args) ,@body) (cdr form)))))

(define-macro (define-suo-variable name value)
  `(set-global-variable ',name ,value))

(define-macro (define-suo head . body)
  (if (pair? head)
      `(define-suo ,(car head) (lambda ,(cdr head) ,@body))
      `(define-suo-variable ,head (cps-compile ',(car body)))))

;;; Basic Scheme syntax

(define-suo-macro (quote val)
  `(:quote ,val))

(define-suo-macro (lambda args . body)
  `(:lambda ,args ,@body))

(define-suo-macro (let bindings . body)
  (let ((vars (map car bindings))
	(inits (map cadr bindings)))
    `((lambda ,vars ,@body) ,@inits)))

(define-suo-macro (if cond then . else)
  (if (null? else)
      `(if ,cond ,then (begin))
      `(:primif (if-eq? ,cond #f) ,(car else) ,then)))

(define-suo-macro (begin . body)
  `(:begin ,@body))

(define-suo-macro (set! var val)
  `(:set ,var ,val))

(define-suo-macro (letrec bindings . body)
  (let ((vars (map car bindings))
	(inits (map cadr bindings))
	(unspec (if #f #f)))
    `(let ,(map (lambda (v) `(,v ',unspec)) vars)
       ,@(map (lambda (v i)
		`(set! ,v ,i))
	      vars inits)
       (let () ,@body))))

(define-suo-macro (do specs term . body)
  (let* ((vars (map car specs))
	 (inits (map cadr specs))
	 (steps (map (lambda (s v) (if (null? (cddr s)) v (caddr s)))
		     specs vars))
	 (test (car term))
	 (term-body (cdr term))
	 (loop (gensym)))
    (pk `(letrec ((,loop (lambda ,vars
			   (if ,test
			       (begin ,@term-body)
			       (begin ,@body
				      (,loop ,@steps))))))
	   (,loop ,@inits)))))

(define-suo-variable error:not-a-closure #f)
(define-suo-variable error:wrong-num-args #f)

(define-suo (error:not-a-closure thing)
  (:primop syscall 1 2 thing)
  (:primop syscall))

(define-suo (error:wrong-num-args)
  (:primop syscall 1 3)
  (:primop syscall))

;;; Debugging

(define-suo (pk x)
  (:primop syscall 0 x)
  x)

;;; Error handling

(define-suo (error code)
  (:primop syscall 1 code)
  (:primop syscall))

;;; Testing and booleans

(define-suo (eq? a b)
  (:primif (if-eq? a b) #t #f))

(define-suo (not x)
  (if x #f #t))

(define-suo-macro (and . args)
  (cond ((null? args)
	 #t)
	((null? (cdr args))
	 (car args))
	(else
	 `(if ,(car args)
	      (and ,@(cdr args))
	      #f))))

(define-suo-macro (or . args)
  (cond ((null? args)
	 #f)
	((null? (cdr args))
	 (car args))
	(else
	 (let ((tmp (gensym)))
	   `(let ((,tmp ,(car args)))
	      (if ,tmp
		  ,tmp
		  (or ,@(cdr args))))))))
;;; Pairs

(define-suo (cons a b)
  (:primop cons a b))

(define-suo (pair? a)
  (:primif (if-pair? a) #t #f))

(define-suo (car a)
  (if (pair? a)
      (:primop car a)
      (error 0)))

(define-suo (cdr a)
  (if (pair? a)
      (:primop cdr a)
      (error 0)))

;;; Fixnums

(define-suo (fixnum? obj)
  (:primif (if-fixnum? obj) #t #f))

(define-suo (+ a b)
  (if (and (fixnum? a)
	   (fixnum? b))
      (or (:primop add-fixnum a b)
	  (error 1))               ; overflow
      (error 0)))                  ; non-fixnums

(define-suo (- a b)
  (if (and (fixnum? a)
	   (fixnum? b))
      (or (:primop sub-fixnum a b)
	  (error 1))               ; overflow
      (error 0)))                  ; non-fixnums

(define-suo (* a b)
  (if (and (fixnum? a)
	   (fixnum? b))
      (or (:primop mul-fixnum a b)
	  (error 1))               ; overflow
      (error 0)))                  ; non-fixnums

(define-suo (= a b)
  (if (and (fixnum? a)
	   (fixnum? b))
      (eq? a b)
      (error 0)))

(define-suo (zero? a)
  (= a 0))

(define-suo (< a b)
  (if (and (fixnum? a)
	   (fixnum? b))
      (:primif (if-< a b) #t #f)
      (error 0)))

(define-suo (<= a b)
  (or (< a b) (= a b)))
