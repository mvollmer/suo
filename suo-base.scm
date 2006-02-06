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

;;; Some basic record types that are needed by the compiler.  We need
;;; to define them early.

(define (suo:make-record-type n-fields name)
  (suo:record suo:record-type-type n-fields name))

(define suo:box-type (suo:make-record-type 1 'box))

(define suo:variable-type (suo:make-record-type 2 'variable))

(define suo:macro-type (suo:make-record-type 2 'macro))

(define suo:closure-type (suo:make-record-type 2 'closure))

;;; Basic Scheme syntax

(define (suo:macroexpand1 form)
  (if (and (pair? form) (symbol? (car form)))
      (let ((def (lookup-global-macro (car form))))
	(if def
	    (expand-macro def form)
	    form))
      form))

(define (suo:macroexpand form)
  (let ((exp (suo:macroexpand1 form)))
    (if (eq? exp form)
	form
	(suo:macroexpand exp))))

(define (suo:expand-body body)
  (let loop ((defs '())
	     (rest body))
    (let ((first (suo:macroexpand (car rest))))
      (cond ((and (pair? first) (eq? (car first) :define))
	     (loop (cons first defs) (cdr rest)))
	    ((and (pair? first) (eq? (car first) :begin))
	     (loop defs (append (cdr first) (cdr rest))))
	    ((null? defs)
	     (cons first (cdr rest)))
	    (else
	     (let ((vars (map cadr defs))
		   (vals (map caddr defs)))
	       (pk body '->
		   (list `(letrec ,(map list vars vals)
			    ,@(cons first (cdr rest)))))))))))

(define-suo-macro (define head . body)
  (if (pair? head)
      `(define ,(car head) (lambda ,(cdr head) ,@body))
      `(:define ,head ,(car body))))

(define-suo-macro (quote val)
  `(:quote ,val))

(define-suo-macro (lambda args . body)
  `(:lambda ,args ,@(suo:expand-body body)))

(define-suo-macro (let first . rest)
  (if (symbol? first)
      (let ((name first)
	    (bindings (car rest))
	    (body (cdr rest)))
	(let ((vars (map car bindings))
	      (inits (map cadr bindings)))
	  (pk `(letrec ((,name (lambda ,vars ,@body)))
		 (,name ,@inits)))))
      (let ((vars (map car first))
	    (inits (map cadr first)))
	`((lambda ,vars ,@rest) ,@inits))))

(define-suo-macro (let* bindings . body)
  (if (null? bindings)
      `(let ()
	 ,@body)
      `(let (,(car bindings))
	 (let* ,(cdr bindings)
	   ,@body))))

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
  (let ((tmp (gensym)))
    `(let ((,tmp ,test))
       (and ,tmp (,proc ,tmp)))))

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

;;; Toplevel

(define-suo-variable toplevel '())

;;; Debugging

(define-suo (pk . args)
  (display ";;; ")
  (for-each (lambda (a) (write a) (display " ")) args)
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

(define-suo (make-string n)
  (if (fixnum? n)
      (:primop make-bytevec n)))

(define-suo (string . chars)
  (let ((str (make-string (length chars))))
    (do ((i 0 (+ i 1))
	 (c chars (cdr c)))
	((null? c))
      (string-set! str i (car c)))
    str))

(define-suo (list->string list)
  (apply string list))

(define-suo (string->list str)
  (let ((len (string-length str)))
    (do ((i (- len 1) (- i 1))
	 (l '() (cons (string-ref str i) l)))
	((< i 0) l))))

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

(define-suo (number->string n)
  (list->string
   (if (< n 0)
       (cons #\- (reverse (positive-number->char-list (- n))))
       (reverse (positive-number->char-list n)))))

(define-suo (positive-number->char-list n)
  (let ((q (quotient n 10))
	(r (remainder n 10)))
    (cons (integer->char (+ (char->integer #\0) r))
	  (if (zero? q)
	      '()
	      (positive-number->char-list q)))))

(define-suo (digit? ch)
  (and (<= (char->integer #\0) (char->integer ch))
       (<= (char->integer ch) (char->integer #\9))))

(define-suo (string->number str)
  (let loop ((chars (string->list str))
	     (num 0)
	     (start? #t)
	     (valid? #f)
	     (sign 1))
    (cond ((null? chars)
	   (if valid?
	       (* num sign)
	       #f))
	  ((and start? (whitespace? (car chars)))
	   (loop (cdr chars) num #t #f sign))
	  ((and start? (eq? (car chars) #\-))
	   (loop (cdr chars) num #f #f -1))
	  ((and start? (eq? (car chars) #\+))
	   (loop (cdr chars) num #f #f 1))
	  ((digit? (car chars))
	   (loop (cdr chars) (+ (* 10 num) (- (char->integer (car chars))
					      (char->integer #\0)))
		 #f #t sign))
	  (else
	   #f))))

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

(define-suo (output-string port str)
  (let ((len (string-length str)))
    (do ((i 0 (+ i 1)))
	((= i len))
      (output-char port (string-ref str i)))))

(define-suo (input-char port)
  (port 1 #f))

(define-suo (putback-char port ch)
  (port 2 ch))

(define-suo (peek-char port)
  (let ((ch (input-char port)))
    (putback-char port ch)
    ch))

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
  (let ((ch #f))
    (lambda (op arg)
      (cond ((= op 1)
	     (if ch
		 (let ((r ch))
		   (set! ch #f)
		   r)
		 (let ((buf "."))
		   (let ((res (sys:read fd buf 0 1)))
		     (cond ((= res 1)
			    (string-ref buf 0))
			   ((= res 0)
			    arg)
			   (else
			    (error "read error")))))))
	    ((= op 2)
	     (set! ch arg))
	    ((= op 0)
	     (error "can't write to input port"))
	    (else
	     (error "not supported"))))))

(define-suo-variable current-input-port #f)
(define-suo-variable current-output-port #f)

(define-suo (init-ports)
  (set! current-input-port (make-parameter (make-sys-input-port 0)))
  (set! current-output-port (make-parameter (make-sys-output-port 1))))

;;; Input/Output

(define-suo (make-print-state port writing?)
  (cons port writing?))

(define-suo (print-state-port state)
  (car state))

(define-suo (print-state-writing? state)
  (cdr state))

(define-suo (print-string str state)
  (let ((port (print-state-port state))
	(len (string-length str)))
    (if (print-state-writing? state)
	(begin
	  (output-char port #\")
	  (do ((i 0 (+ i 1)))
	      ((= i len))
	    (let ((ch (string-ref str i)))
	      (if (eq? ch #\")
		  (output-char port #\\))
	      (output-char port ch)))
	  (output-char port #\"))
	(output-string port str))))

(define-suo (print-character ch state)
  (let ((port (print-state-port state)))
    (if (print-state-writing? state)
	(begin
	  (output-string port "#\\")
	  (output-char port ch))
	(output-char port ch))))

(define-suo (print-number n state)
  (output-string (print-state-port state) (number->string n)))

(define-suo (print-list-tail p state)
  (if (pair? p)
      (begin
	(output-char (print-state-port state) #\space)
	(print (car p) state)
	(print-list-tail (cdr p) state))
      (begin
	(if (not (null? p))
	    (begin
	      (output-string (print-state-port state) " . ")
	      (print p state)))
	(output-char (print-state-port state) #\)))))
      
(define-suo (print-list p state)
  (let ((port (print-state-port state)))
    (output-char port #\()
    (print (car p) state)
    (print-list-tail (cdr p) state)))

(define-suo (print-record r state)
  (let ((port (print-state-port state))
	(n (record-length r)))
    (output-string port "#<")
    (print (record-type-name (record-type r)) state)
    (do ((i 0 (+ i 1)))
	((= i n))
      (output-string port " ")
      (print (record-ref r i) state))
    (output-string port ">")))

(define-suo (print-vector v state)
  (let ((port (print-state-port state))
	(n (vector-length v)))
    (output-string port "#(")
    (do ((i 0 (+ i 1))
	 (sp #f #t))
	((= i n))
      (if sp
	  (output-string port " "))
      (print (vector-ref v i) state))
    (output-char port #\))))

(define-suo (print-weird-symbol s state)
  (output-char (print-state-port state) #\|)
  (output-string (print-state-port state) (symbol->string s))
  (output-char (print-state-port state) #\|))

(define-suo (print-symbol s state)
  (if (string->number (symbol->string s))
      (print-weird-symbol s state)
      (output-string (print-state-port state) (symbol->string s))))

(define-suo (print-code val state)
  (output-string (print-state-port state) "#<code>"))

(define-suo-variable print-depth #f)

(define-suo (init-print)
  (set! print-depth (make-parameter 0)))

(define-suo (print val state)
  (if (< (print-depth) 10)
      (call/p print-depth (+ 1 (print-depth))
	      (lambda ()
		(let ((port (print-state-port state)))
		  (cond ((eq? #t val)
			 (output-string port "#t"))
			((eq? #f val)
			 (output-string port "#f"))
			((eq? '() val)
			 (output-string port "()"))
			((eq? (begin) val)
			 (output-string port "#<unspecified>"))
			((char? val)
			 (print-character val state))
			((fixnum? val)
			 (print-number val state))
			((string? val)
			 (print-string val state))
			((symbol? val)
			 (print-symbol val state))
			((pair? val)
			 (print-list val state))
			((vector? val)
			 (print-vector val state))
			((code? val)
			 (print-code val state))
			((record? val)
			 (print-record val state))
			(else
			 (output-string port "#<...>"))))))
      (output-string (print-state-port state) "#")))

(define-suo (display val)
  (print val (make-print-state (current-output-port) #f)))

(define-suo (write val)
  (print val (make-print-state (current-output-port) #t)))

(define-suo (newline)
  (display #\nl))

(define-suo (make-parse-state port)
  port)

(define-suo (parse-state-port state)
  state)

(define-suo (parse-reduce p f i state)
  (let ((ch (input-char (parse-state-port state))))
    (if (p ch)
	(parse-reduce p f (f ch i) state)
	(begin
	  (putback-char (parse-state-port state) ch)
	  i))))

(define-suo (parse-gather p state)
  (reverse (parse-reduce p cons '() state)))

(define-suo (parse-skip p state)
  (parse-reduce p (lambda (ch v) #t) #f state))

(define-suo (parse-comment state)
  (let ((ch (input-char (parse-state-port state))))
    (if (not (eq? ch #\newline))
	(parse-comment state))))

(define-suo (whitespace? ch)
  (or (eq? ch #\space)
      (eq? ch #\tab)
      (eq? ch #\newline)))

(define-suo (delimiter? ch)
  (or (whitespace? ch)
      (eq? ch #\()
      (eq? ch #\))
      (eq? ch #\;)))
      
(define-suo (parse-whitespace state)
  (let ((ch (input-char (parse-state-port state))))
    (cond ((whitespace? ch)
	   (parse-whitespace state))
	  ((eq? ch #\;)
	   (parse-comment state)
	   (parse-whitespace state))
	  (else
	   (putback-char (parse-state-port state) ch)))))

(define-suo (parse-token state)
  (let ((str (list->string
	      (parse-gather (lambda (ch) 
			      (not (delimiter? ch)))
			    state))))
    (or (string->number str)
	(string->symbol str))))

(define-suo-variable dot (string->symbol "."))

(define-suo (must-parse ch state)
  (parse-whitespace state)
  (if (not (eq? ch (input-char (parse-state-port state))))
      (error "missing " ch)))

(define-suo (parse-list state)
  (parse-whitespace state)
  (let ((ch (input-char (parse-state-port state))))
    (if (eq? ch #\))
	'()
	(begin
	  (putback-char (parse-state-port state) ch)
	  (let ((elt (parse state)))
	    (if (eq? dot elt)
		(let ((elt (parse state)))
		  (must-parse #\) state)
		  elt)
		(cons elt (parse-list state))))))))

(define-suo (parse-string state)
  (let ((port (parse-state-port state)))
    (let loop ((ch (input-char port))
	       (chars '()))
      (cond ((eq? ch #\")
	     (list->string (reverse chars)))
	    ((eq? ch #\\)
	     (let ((ch (input-char port)))
	       (loop (input-char port) (cons ch chars))))
	    (else
	     (loop (input-char port) (cons ch chars)))))))

(define-suo (parse-sharp state)
  (let ((ch (input-char (parse-state-port state))))
    (cond ((eq? ch #\()
	   (list->vector (parse-list state)))
	  (else
	   (error "unsupported # construct: #" ch)))))

(define-suo (parse state)
  (parse-whitespace state)
  (let ((ch (input-char (parse-state-port state))))
    (cond ((eq? ch #\()
	   (parse-list state))
	  ((eq? ch #\")
	   (parse-string state))
	  ((eq? ch #\#)
	   (parse-sharp state))
	  (else
	   (putback-char (parse-state-port state) ch)
	   (parse-token state)))))

(define-suo (read)
  (parse (make-parse-state (current-input-port))))

;;; Records

(define-suo-variable record-type-type suo:record-type-type)

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
  (if (record-with-type? type record-type-type)
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
      (record record-type-type n-fields name)
      (error:wrong-type n-fields)))

(define-suo (record-type-n-fields rec)
  (if (record-with-type? rec record-type-type)
      (record-ref rec 0)
      (error:wrong-type rec)))

(define-suo (record-type-name rec)
  (if (record-with-type? rec record-type-type)
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

(define-suo (list->vector lst)
  (apply vector lst))

;;; Code

(define-suo (code? val)
  (:primif (if-code? val) #t #f))

;;; equal?

(define-suo (equal? a b)
  (or (eq? a b)
      (and (string? a) (string? b)
	   (string-equal? a b))
      (and (symbol? a) (symbol? b)
	   (symbol-equal? a b))
      (and (pair? a) (pair? b)
	   (pair-equal? a b))
      (and (vector? a) (vector? b)
	   (pair-equal? a b))
      (and (record? a) (record-with-type? b (record-type a))
	   (record-equal? a b))))

(define-suo (string-equal? a b)
  (let ((len (string-length a)))
    (and (= len (string-length b))
	 (do ((i 0 (+ i 1)))
	     ((or (= i len)
		  (not (equal? (string-ref a i) (string-ref b i))))
	      (= i len))))))

(define-suo (vector-equal? a b)
  (let ((len (vector-length a)))
    (and (= len (vector-length b))
	 (do ((i 0 (+ i 1)))
	     ((or (= i len)
		  (not (equal? (vector-ref a i) (vector-ref b i))))
	      (= i len))))))

(define-suo (record-equal? a b)
  (let ((len (record-length a)))
    (do ((i 0 (+ i 1)))
	((or (= i len)
	     (not (equal? (record-ref a i) (record-ref b i))))
	 (= i len)))))

(define-suo (symbol-equal? a b)
  (eq? a b))

(define-suo (pair-equal? a b)
  (and (equal? (car a) (car b))
       (equal? (cdr a) (cdr b))))

;;; Symbols

(define suo:symbol-type (suo:make-record-type 1 'symbol))
(define-suo-variable symbol-type suo:symbol-type)

(define-suo-variable all-symbols '())

(define-suo (init-symbols syms)
  (set! all-symbols syms))

(define-suo (symbol? val)
  (record-with-type? val symbol-type))

(define-suo (find-symbol str syms)
  (cond ((null? syms)
	 #f)
	((equal? (symbol->string (car syms)) str)
	 (car syms))
	(else
	 (find-symbol str (cdr syms)))))

(define-suo (string->symbol str)
  (if (string? str)
      (or (find-symbol str all-symbols)
	  (let ((sym (record symbol-type str)))
	    (set! all-symbols (cons sym all-symbols))
	    sym))
      (error:wrong-type str)))

(define-suo (symbol->string sym)
  (if (record-with-type? sym symbol-type)
      (record-ref sym 0)
      (error:wrong-type sym)))
