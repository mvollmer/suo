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

(define-suo-variable else #t)

(define-suo-macro (cond . clauses)
  (if (null? clauses)
      `(quote ,(if #f #f))
      (let* ((clause (car clauses))
	     (test (car clause)))
	(if (eq? (cadr clause) '=>)
	    `(and=> ,test ,(caddr clause))
	    `(if ,test
		 (begin ,@(cdr clause))
		 (cond ,@(cdr clauses)))))))

(define-suo-macro (and=> test proc)
  (let ((tmp (gentemp)))
    `(let ((,tmp ,test))
       (and ,tmp (proc ,tmp)))))

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
    `(letrec ((,loop (lambda ,vars
		       (if ,test
			   (begin ,@term-body)
			   (begin ,@body
				  (,loop ,@steps))))))
       (,loop ,@inits))))

;;; Error handling

(define-suo-variable error:not-a-closure #f)
(define-suo-variable error:wrong-num-args #f)

(define-suo (error:not-a-closure thing)
  (error "not a closure: " thing))

(define-suo (error:wrong-num-args)
  (error "wrong number of arguments"))

(define-suo (error:wrong-type val)
  (error "wrong type: " val))

(define-suo (error:out-of-range val)
  (error "out of range: " val))

(define-suo (error:overflow)
  (error "overflow"))

(define-suo-variable handling-error #f)

(define-suo (error msg . rest)
  (if handling-error
      (sys:halt)
      (begin
	(set! handling-error #t)
	(display "ERROR: ")
	(display msg)
	(for-each display rest)
	(newline)
	(sys:halt))))

;;; Debugging

(define-suo (pk . args)
  (display ";;; ")
  (for-each (lambda (a) (display a) (display " ")) args)
  (newline)
  (car (last-pair args)))

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
      (error:wrong-type a)))

(define-suo (cdr a)
  (if (pair? a)
      (:primop cdr a)
      (error:wrong-type a)))

(define-suo (set-car! a val)
  (if (pair? a)
      (:primop set-car a val)
      (error:wrong-type a)))

(define-suo (set-cdr! a val)
  (if (pair? a)
      (:primop set-cdr a val)
      (error:wrong-type a)))

(define-suo (cadr a)
  (car (cdr a)))

(define-suo (caddr a)
  (car (cdr (cdr a))))

(define-suo (cadddr a)
  (car (cdr (cdr (cdr a)))))

;;; Lists

(define-suo (list . elts)
  elts)

(define-suo (length lst)
  (if (null? lst)
      0
      (+ 1 (length (cdr lst)))))

(define-suo (null? lst)
  (eq? lst '()))

(define-suo (reduce f i l)
  (if (pair? l)
      (f (car l) (reduce f i (cdr l)))
      i))

(define-suo (map func lst)
  (if (null? lst)
      '()
      (cons (func (car lst)) (map func (cdr lst)))))

(define-suo (for-each func lst)
  (if (not (null? lst))
      (begin
	(func (car lst))
	(for-each func (cdr lst)))))

(define-suo (reverse-with-tail list tail)
  (if (null? list)
      tail
      (reverse-with-tail (cdr list) (cons (car list) tail))))

(define-suo (reverse list)
  (reverse-with-tail list '()))

(define-suo (last-pair list)
  (if (null? (cdr list))
      list
      (last-pair (cdr list))))

;;; Association lists

(define-suo (acons key val alist)
  (cons (cons key val) alist))

(define-suo (assq key alist)
  (cond ((null? alist)
	 #f)
	((eq? key (car (car alist)))
	 (car alist))
	(else
	 (assq key (cdr alist)))))
      
;;; Characters

(define-suo (char? val)
  (:primif (if-char? val) #t #f))

(define-suo (integer->char n)
  (if (and (<= 0 n) (< n #x1000000))
      (:primop integer->char n)
      (error:out-of-range n)))

(define-suo (char->integer c)
  (if (char? c)
      (:primop char->integer c)
      (error:wrong-type c)))

;;; Strings (just bytevecs for now)

(define-suo (string? val)
  (:primif (if-bytevec? val) #t #f))

;; (define-suo (make-string n . opt-fill)
;;   (let ((fill (if (null? opt-fill) #\ (car opt-fill))))
;;     (:primop make-bytevec n (char->integer fill))))

(define-suo (string-length str)
  (if (string? str)
      (:primop bytevec-length str)
      (error:wrong-type str)))

(define-suo (string-ref str idx)
  (if (and (<= 0 idx) (< idx (string-length str)))
      (integer->char (:primop bytevec-ref-u8 str idx))
      (error:out-of-range idx)))

(define-suo (string-set! str idx chr)
  (if (and (<= 0 idx) (< idx (string-length str)))
      (:primop bytevec-set-u8 str idx (char->integer chr))
      (error:out-of-range idx)))

;;; Functions

(define-suo (flatten-for-apply arg1 args+rest)
  (if (null? args+rest)
      arg1
      (cons arg1 (flatten-for-apply (car args+rest) (cdr args+rest)))))

(define-suo (apply func arg1 . args+rest)
  (:apply func (flatten-for-apply arg1 args+rest)))

;;; Dynamic environment, continuations and multiple values

(define-suo-variable -dynamic-env- '())

(define-suo (call/cc func)
  (let ((old-env -dynamic-env-))
    (:call/cc (lambda (k)
		(func (lambda results
			(set! -dynamic-env- old-env)
			(apply k results)))))))

(define-suo (call/v producer consumer)
  (:call/v producer consumer))

(define-suo (values . results)
  (call/cc (lambda (k) (apply k results))))

(define-suo (call/de env func)
  (call/cc (lambda (k)
	     (set! -dynamic-env- env)
	     (call/v func k))))

(define-suo (dynamic-environment)
  -dynamic-env-)

;;; Parameters

(define-suo (make-parameter init)
  (let ((global-cell (cons #f init)))
    (letrec ((parameter (lambda new-val
			  (let ((cell (or (assq parameter
						(dynamic-environment))
					  global-cell)))
			    (cond ((null? new-val)
				   (cdr cell))
				  (else
				   (set-cdr! cell (car new-val))))))))
      parameter)))

(define-suo (call/p parameter value func)
  (call/de (acons parameter value (dynamic-environment)) func))

;;; Fixnums

(define-suo (fixnum? obj)
  (:primif (if-fixnum? obj) #t #f))

(define-suo (2+ a b)
  (if (fixnum? a)
      (if (fixnum? b)
	  (or (:primop add-fixnum a b)
	      (error:overflow))
	  (error:wrong-type b))
      (error:wrong-type a)))

(define-suo (+ . args)
  (reduce 2+ 0 args))

(define-suo (2- a b)
  (if (fixnum? a)
      (if (fixnum? b)
	  (or (:primop sub-fixnum a b)
	      (error:overflow))
	  (error:wrong-type b))
      (error:wrong-type a)))

(define-suo (- arg . rest)
  (if (null? rest)
      (2- 0 arg)
      (2- arg (apply + rest))))

(define-suo (2* a b)
  (if (fixnum? a)
      (if (fixnum? b)
	  (or (:primop mul-fixnum a b)
	      (error:overflow))
	  (error:wrong-type b))
      (error:wrong-type a)))

(define-suo (* . args)
  (reduce 2* 1 args))

(define-suo (quotient a b)
  (if (fixnum? a)
      (if (fixnum? b)
	  (:primop quotient-fixnum a b)
	  (error:wrong-type b))
      (error:wrong-type a)))

(define-suo (remainder a b)
  (if (fixnum? a)
      (if (fixnum? b)
	  (:primop remainder-fixnum a b)
	  (error:wrong-type b))
      (error:wrong-type a)))

(define-suo (= a b)
  (if (fixnum? a)
      (if (fixnum? b)
	  (eq? a b)
	  (error:wrong-type b))
      (error:wrong-type a)))

(define-suo (zero? a)
  (= a 0))

(define-suo (< a b)
  (if (fixnum? a)
      (if (fixnum? b)
	  (:primif (if-< a b) #t #f)
	  (error:wrong-type b))
      (error:wrong-type a)))

(define-suo (<= a b)
  (or (< a b) (= a b)))

;;; Syscalls

(define-suo (sys:halt)
  (:primop syscall))

(define-suo (sys:peek . vals)
  (let ((n (length vals)))
    (cond ((= n 0)
	   (:primop syscall 0))
	  ((= n 1)
	   (:primop syscall 0 (car vals)))
	  ((= n 2)
	   (:primop syscall 0 (car vals) (cadr vals)))
	  (else
	   (:primop syscall 0 (car vals) (cadr vals) (caddr vals))))))

(define-suo (sys:write fd buf start end)
  (if (string? buf)
      (if (and (<= 0 start)
	       (<= start end))
	  (if (<= end (string-length buf))
	      (:primop syscall 2 fd buf start end)
	      (error:out-of-range end))
	  (error:out-of-range start))
      (error:wrong-type buf)))

(define-suo (sys:read fd buf start end)
  (if (string? buf)
      (if (and (<= 0 start)
	       (<= start end))
	  (if (<= end (string-length buf))
	      (:primop syscall 3 fd buf start end)
	      (error:out-of-range end))
	  (error:out-of-range start))
      (error:wrong-type buf)))

;;; Ports

(define-suo (output-char port ch)
  (port 0 ch))

(define-suo (input-char port)
  (port 1 #f))

(define-suo (make-sys-output-port fd)
  (lambda (op arg)
    (cond ((= op 0)
	   (let ((buf "."))
	     (string-set! buf 0 arg)
	     (sys:write fd buf 0 1)))
	  ((= op 1)
	   (error "can't read from output port"))
	  (else
	   (error "not supported")))))

(define-suo (make-sys-input-port fd)
  (lambda (op arg)
    (cond ((= op 1)
	   (let ((buf "."))
	     (let ((res (sys:read fd buf 0 1)))
	       (cond ((= res 1)
		      (string-ref buf 0))
		     ((= res 0)
		      arg)
		     (else
		      (error "read error"))))))
	  ((= op 0)
	   (error "can't write to input port"))
	  (else
	   (error "not supported")))))

(define-suo-variable current-input-port #f)
(define-suo-variable current-output-port #f)

(define-suo (init-ports)
  (set! current-input-port (make-parameter (make-sys-input-port 0)))
  (set! current-output-port (make-parameter (make-sys-output-port 1))))

;;; Input/Output

(define-suo (display-string str)
  (let ((port (current-output-port))
	(len (string-length str)))
    (do ((i 0 (+ i 1)))
	((= i len))
      (output-char port (string-ref str i)))))

(define-suo (display-character chr)
  (output-char (current-output-port) chr))

(define-suo (display-positive-number n)
  (let ((q (quotient n 10))
	(r (remainder n 10)))
    (if (not (zero? q))
	(display-positive-number q))
    (display-character (integer->char (+ (char->integer #\0) r)))))

(define-suo (display-number n)
  (if (zero? n)
      (display-string "0")
      (if (< n 0)
	  (begin
	    (display-string "-")
	    (display-positive-number (- n)))
	  (display-positive-number n))))

(define-suo (display-list-tail p)
  (if (pair? p)
      (begin
	(display-character #\space)
	(display (car p))
	(display-list-tail (cdr p)))
      (begin
	(if (not (null? p))
	    (begin
	      (display-string " . ")
	      (display p)))
	(display-character #\)))))
      
(define-suo (display-list p)
  (display-character #\()
  (display (car p))
  (display-list-tail (cdr p)))

(define-suo (display-record r)
  (let ((n (record-length r)))
    (display-string "#<")
    (display (record-type-name (record-type r)))
    (do ((i 0 (+ i 1)))
	((= i n))
      (display-string " ")
      (display (record-ref r i)))
    (display-string ">")))

(define-suo (display-vector v)
  (let ((n (vector-length v)))
    (display-string "#(")
    (do ((i 0 (+ i 1))
	 (sp #f #t))
	((= i n))
      (if sp
	  (display-string " "))
      (display (vector-ref v i)))
    (display-string ")")))

(define-suo (display-code val)
  (display-string "#<code>"))

(define-suo (display val)
  (cond ((eq? #t val)
	 (display-string "#t"))
	((eq? #f val)
	 (display-string "#f"))
	((eq? '() val)
	 (display-string "()"))
	((eq? (begin) val)
	 (display-string "#<unspecified>"))
	((char? val)
	 (display-character val))
	((fixnum? val)
	 (display-number val))
	((string? val)
	 (display-string val))
	((pair? val)
	 (display-list val))
	((record? val)
	 (display-record val))
	((vector? val)
	 (display-vector val))
	((code? val)
	 (display-code val))
	(else
	 (display-string "#<...>"))))
  
(define-suo (newline)
  (display-character #\nl))

;;; Records

(define-suo-variable record-descriptor-descriptor suo:descriptor-descriptor)

(define-suo (record? val)
  (:primif (if-record? val #t) #t #f))

(define-suo (record-with-type? val type)
  (:primif (if-record? val type) #t #f))

(define-suo (record-type rec)
  (if (record? rec)
      (:primop record-desc rec)
      (error:wrong-type rec)))

(define-suo (record-length rec)
  (:primop record-ref (record-type rec) 0))

(define-suo (record-ref rec idx)
  (if (and (<= 0 idx) (< idx (record-length rec)))
      (:primop record-ref rec idx)
      (error:out-of-range idx)))

(define-suo (record-set! rec idx val)
  (if (and (<= 0 idx) (< idx (record-length rec)))
      (:primop record-set rec idx val)
      (error:out-of-range idx)))

(define-suo (make-record type init)
  (:primop make-record type (record-type-n-fields type) init))
  
(define-suo (record type . values)
  (if (record-with-type? type record-descriptor-descriptor)
      (let ((n (record-ref type 0))
	    (rec (make-record type #f)))
	(if (= (length values) n)
	    (begin
	      (do ((i 0 (+ i 1))
		   (v values (cdr v)))
		  ((= i n))
		(record-set! rec i (car v)))
	      rec)
	    (error:wrong-num-args)))
      (error:wrong-type type)))

(define-suo (make-record-type n-fields name)
  (if (fixnum? n-fields)
      (record record-descriptor-descriptor n-fields name)
      (error:wrong-type n-fields)))

(define-suo (record-type-n-fields rec)
  (if (record-with-type? rec record-descriptor-descriptor)
      (record-ref rec 0)
      (error:wrong-type rec)))

(define-suo (record-type-name rec)
  (if (record-with-type? rec record-descriptor-descriptor)
      (record-ref rec 1)
      (error:wrong-type rec)))

;;; Vectors

(define-suo (vector? val)
  (:primif (if-vector? val) #t #f))

(define-suo (vector-length vec)
  (if (vector? vec)
      (:primop vector-length vec)
      (error:wrong-type vec)))

(define-suo (vector-ref vec idx)
  (if (and (<= 0 idx) (< idx (vector-length vec)))
      (:primop vector-ref vec idx)
      (error:out-of-range idx)))

(define-suo (vector-set! vec idx val)
  (if (and (<= 0 idx) (< idx (vector-length vec)))
      (:primop vector-set vec idx val)
      (error:out-of-range idx)))

(define-suo (make-vector n init)
  (if (fixnum? n)
      (:primop make-vector n init)
      (error:out-of-range n)))

(define-suo (vector . values)
  (let ((n (length values)))
    (let ((vec (make-vector n #f)))
      (do ((i 0 (+ i 1))
	   (v values (cdr v)))
	  ((= i n))
	(vector-set! vec i (car v)))
      vec)))

;;; Code

(define-suo (code? val)
  (:primif (if-code? val) #t #f))
