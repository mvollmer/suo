;;; Basic Scheme syntax

(define (macroexpand1 form)
  (if (and (pair? form) (symbol? (car form)))
      (let ((transformer (lookup-toplevel-macro-transformer (car form))))
	(if transformer
	    (apply transformer (cdr form))
	    form))
      form))

(define (macroexpand form)
  (let ((exp (macroexpand1 form)))
    (if (eq? exp form)
	form
	(macroexpand exp))))

(define (expand-body body)
  (let loop ((defs '())
	     (rest body))
    (let ((first (macroexpand (car rest))))
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

;; Traditional quaisquote is wierd, mostly when being nested.  This is
;; from Alan Bawden, "Quasiquotation in Lisp", Appendix B.

(define (qq-expand x depth)
  (if (pair? x)
      (case (car x)
	((quasiquote)
	 ;; Guile can't seem to handle a 'quasiquote' symbol in a
	 ;; quasiquote expression; thus we use long hand here.
	 (list 'cons ''quasiquote
	       (qq-expand (cdr x) (+ depth 1))))
	((unquote unquote-splicing)
	 (cond ((> depth 0)
		`(cons ',(car x)
		       ,(qq-expand (cdr x) (- depth 1))))
	       ((and (eq? 'unquote (car x))
		     (not (null? (cdr x)))
		     (null? (cddr x)))
		(cadr x))
	       (else
		(error "illegal"))))
	(else
	 `(append ,(qq-expand-list (car x) depth)
		  ,(qq-expand (cdr x) depth))))
      `',x))

(define (qq-expand-list x depth)
  (if (pair? x)
      (case (car x)
	((quasiquote)
	 ;; Guile can't seem to handle a 'quasiquote' symbol in a
	 ;; quasiquote expression; thus we use long hand here.
	 (list 'list (list 'cons ''quasiquote
			   (qq-expand (cdr x) (+ depth 1)))))
	((unquote unquote-splicing)
	 (cond ((> depth 0)
		`(list (cons ',(car x)
			     ,(qq-expand (cdr x) (- depth 1)))))
	       ((eq? 'unquote (car x))
		`(list . ,(cdr x)))
	       (else
		`(append . ,(cdr x)))))
	(else
	 `(list (append ,(qq-expand-list (car x) depth)
			,(qq-expand (cdr x) depth)))))
      `'(,x)))

(define-macro (lambda args . body)
  (cons* :lambda args (expand-body body)))

(define-macro (quote val)
  (list :quote val))

(define-macro (quasiquote form)
  (qq-expand form 0))

(define-macro (if cond then . else)
  (if (null? else)
      `(if ,cond ,then (begin))
      `(:primitive if-eq? () (,cond #f) (,(car else) ,then))))

(define-macro (begin . body)
  `(:begin ,@body))

(define-macro (define head . body)
  (if (pair? head)
      `(define ,(car head) (lambda ,(cdr head) 
;;			     (primop syscall 0 ,(host-string-chars
;;						 (symbol->string (car head))))
;;			     (let ()
			       ,@body))
;;	 )
      `(:define ,head ,(car body))))

(define-macro (define-macro head . body)
  (if (pair? head)
      `(define-macro ,(car head) (lambda ,(cdr head) ,@body))
      `(:define-macro ,head ,(car body))))

(define-macro (let first . rest)
  (if (symbol? first)
      (let ((name first)
	    (bindings (car rest))
	    (body (cdr rest)))
	`(letrec ((,name (lambda ,(map car bindings) ,@body)))
	   (,name ,@(map cadr bindings))))
      `((lambda ,(map car first) ,@rest) ,@(map cadr first))))

(define-macro (let* bindings . body)
  (if (null? bindings)
      `(let ()
	 ,@body)
      `(let (,(car bindings))
	 (let* ,(cdr bindings)
	   ,@body))))

(define-macro (cond . clauses)
  (if (null? clauses)
      `(quote ,(if #f #f))
      (let* ((clause (car clauses))
	     (test (car clause)))
	(if (eq? (cadr clause) '=>)
	    (let ((tmp (gensym)))
	      `(let ((,tmp ,test))
		 (if ,tmp
		     (,(caddr clause) ,tmp)
		     (cond ,@(cdr clauses)))))
	    (if (eq? test 'else)
		`(begin ,@(cdr clause))
		`(if ,test
		     (begin ,@(cdr clause))
		     (cond ,@(cdr clauses))))))))

(define-macro (case key . clauses)
  (let ((tmp (gensym)))
    `(let ((,tmp ,key))
       (cond ,@(map (lambda (clause)
		      (if (eq? (car clause) 'else)
			  clause
			  `((memv ,tmp ',(car clause)) ,@(cdr clause))))
		    clauses)))))

(define-macro (and=> test proc)
  (let ((tmp (gensym)))
    `(let ((,tmp ,test))
       (and ,tmp (,proc ,tmp)))))

(define-macro (set! var val)
  `(:set ,var ,val))

(define-macro (letrec bindings . body)
  (let ((vars (map car bindings))
	(inits (map cadr bindings))
	(unspec (if #f #f)))
    `(let ,(map (lambda (v) `(,v ',unspec)) vars)
       ,@(map (lambda (v i)
		`(set! ,v ,i))
	      vars inits)
       (let () ,@body))))

(define-macro (do specs term . body)
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

(define-macro (and . args)
  (cond ((null? args)
	 #t)
	((null? (cdr args))
	 (car args))
	(else
	 `(if ,(car args)
	      (and ,@(cdr args))
	      #f))))

(define-macro (or . args)
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

(define-macro (primif cond then else)
  `(:primitive ,(car cond) () ,(cdr cond) (,then ,else)))

(define-macro (primop op . args)
  `(:primitive ,op (res) ,args (res)))

;;; Error handling

(define (error:not-a-closure thing)
  (error "not a closure: " thing))

(define (error:wrong-num-args)
  (error "wrong number of arguments"))

(define (set-wrong-num-args-hook)
  (primop syscall 9 -6 error:wrong-num-args))

(define (error:wrong-type val)
  (error "wrong type: " val))

(define (error:out-of-range val)
  (error "out of range: " val))

(define (error:overflow)
  (error "overflow"))

(define (display-error msg rest)
  (display "ERROR: ")
  (display msg)
  (for-each (lambda (r)
	      (display r)
	      (display " "))
	    rest)
  (newline))
  
(define handling-error #f)

(define (last-straw-error-handler msg rest)
  (if handling-error
      (sys:panic)
      (begin
	(set! handling-error #t)
	(display-error msg rest)
	(sys:panic))))

(define error-handlers (make-parameter '()))

(define (error msg . rest)
  (if (null? (error-handlers))
      (last-straw-error-handler msg rest)
      (let ((handler (car (error-handlers))))
	(error-handlers (cdr (error-handlers)))
	(handler msg rest))))

(define (with-error-handler handler thunk)
  (call/p error-handlers (cons handler (error-handlers)) thunk))

;;; Debugging

(define (pk . args)
  (display ";;; ")
  (for-each (lambda (a) (write a) (display " ")) args)
  (newline)
  (car (last-pair args)))

;;; Testing and booleans

(define (eq? a b)
  (primif (if-eq? a b) #t #f))

(define (eqv? a b)
  (eq? a b))

(define (not x)
  (if x #f #t))

;;; Pairs

(define (cons a b)
  (primop cons a b))

(define (pair? a)
  (primif (if-pair? a) #t #f))

(define (car a)
  (if (pair? a)
      (primop car a)
      (error:wrong-type a)))

(define (cdr a)
  (if (pair? a)
      (primop cdr a)
      (error:wrong-type a)))

(define (set-car! a val)
  (if (pair? a)
      (primop set-car a val)
      (error:wrong-type a)))

(define (set-cdr! a val)
  (if (pair? a)
      (primop set-cdr a val)
      (error:wrong-type a)))

(define (cadr a)
  (car (cdr a)))

(define (cddr a)
  (cdr (cdr a)))

(define (caar a)
  (car (car a)))

(define (cdar a)
  (cdr (car a)))

(define (caddr a)
  (car (cdr (cdr a))))

(define (cadddr a)
  (car (cdr (cdr (cdr a)))))

;;; Lists

(define (list . elts)
  elts)

(define (cons* first . rest)
  (if (null? rest)
      first
      (cons first (apply cons* rest))))

(define (length lst)
  (if (null? lst)
      0
      (+ 1 (length (cdr lst)))))

(define (null? lst)
  (eq? lst '()))

(define (reduce f i l)
  (if (pair? l)
      (f (car l) (reduce f i (cdr l)))
      i))

(define (any pred lst)
  (if (null? lst)
      #f
      (or (pred (car lst))
	  (any pred (cdr lst)))))

(define (map1 func lst)
  (if (null? lst)
      '()
      (cons (func (car lst)) (map1 func (cdr lst)))))

(define (map func . lsts)
  (if (any null? lsts)
      '()
      (cons (apply func (map1 car lsts)) (apply map func (map1 cdr lsts)))))

(define (for-each func . lists)
  (if (not (any null? lists))
      (begin
	(apply func (map1 car lists))
	(apply for-each func (map1 cdr lists)))))

(define (reverse-with-tail list tail)
  (if (null? list)
      tail
      (reverse-with-tail (cdr list) (cons (car list) tail))))

(define (reverse list)
  (reverse-with-tail list '()))

(define (last-pair list)
  (if (null? (cdr list))
      list
      (last-pair (cdr list))))

(define (append . lists)
  (cond ((null? lists)
	 '())
	((null? (cdr lists))
	 (car lists))
	(else
	 (list-copy-with-tail (car lists)
			      (apply append (cdr lists))))))

(define (memv elt lst)
  (cond ((null? lst)
	 #f)
	((eqv? elt (car lst))
	 #t)
	(else
	 (memv elt (cdr lst)))))

(define (memq elt lst)
  (cond ((null? lst)
	 #f)
	((eq? elt (car lst))
	 #t)
	(else
	 (memq elt (cdr lst)))))

(define (list-copy lst)
  (list-copy-with-tail lst '()))

(define (list-copy-with-tail lst tail)
  (if (null? lst)
      tail
      (cons (car lst) (list-copy-with-tail (cdr lst) tail))))

(define (dotted-list? lst)
  (cond ((pair? lst)
	 (dotted-list? (cdr lst)))
	(else
	 (not (null? lst)))))

(define (dotted-list-copy lst)
  (cond ((pair? lst)
	 (cons (car lst) (dotted-list-copy (cdr lst))))
	(else
	 lst)))

(define (dotted-list-length lst)
  (cond ((pair? lst)
	 (1+ (dotted-list-length (cdr lst))))
	(else
	 0)))

(define (flatten-dotted-list lst)
  (cond ((pair? lst)
	 (cons (car lst) (flatten-dotted-list (cdr lst))))
	((null? lst)
	 '())
	(else
	 (list lst))))

(define (list-index lst obj)
  (let loop ((lst lst)
	     (i 0))
    (cond ((null? lst)
	   #f)
	  ((eq? (car lst) obj)
	   i)
	  (else
	   (loop (cdr lst) (1+ i))))))

(define (list-ref lst i)
  (if (zero? i)
      (car lst)
      (list-ref (cdr lst) (1- i))))

(define (list-head lst i)
  (if (zero? i)
      '()
      (cons (car lst) (list-head (cdr lst) (1- i)))))

;;; Association lists

(define (acons key val alist)
  (cons (cons key val) alist))

(define (assq key alist)
  (cond ((null? alist)
	 #f)
	((eq? key (car (car alist)))
	 (car alist))
	(else
	 (assq key (cdr alist)))))

(define (assq-ref alist key)
  (and=> (assq key alist) cdr))

(define (assoc key alist)
  (cond ((null? alist)
	 #f)
	((equal? key (car (car alist)))
	 (car alist))
	(else
	 (assoc key (cdr alist)))))

(define (assoc-ref alist key)
  (and=> (assoc key alist) cdr))

;;; Lists as sets

(define (set-difference l1 l2)
  "Return elements from list L1 that are not in list L2."
  (let loop ((l1 l1) (result '()))
    (cond ((null? l1) (reverse result))
	  ((memv (car l1) l2) (loop (cdr l1) result))
	  (else (loop (cdr l1) (cons (car l1) result))))))

(define (adjoin e l)
  "Return list L, possibly with element E added if it is not already in L."
  (if (memq e l) l (cons e l)))

(define (union l1 l2)
  "Return a new list that is the union of L1 and L2.
Elements that occur in both lists occur only once in
the result list."
  (cond ((null? l1) l2)
	((null? l2) l1)
	(else (union (cdr l1) (adjoin (car l1) l2)))))

;;; Byte vectors

(define (bytevec? val)
  (primif (if-bytevec? val) #t #f))

(define (make-bytevec n)
  (if (fixnum? n)
      (primop make-bytevec n)
      (error:wrong-type n)))

(define (bytevec-subvector v start end)
  (let ((s (make-bytevec (- end start))))
    (do ((i start (1+ i)))
	((= i end))
      (bytevec-set-u8! s (- i start) (bytevec-ref-u8 v i)))
    s))

(define (bytevec-length-8 bv)
  (if (bytevec? bv)
      (primop bytevec-length-8 bv)
      (error:wrong-type bv)))

(define (bytevec-ref-u8 bv i)
  (if (and (<= 0 i) (< i (bytevec-length-8 bv)))
      (primop bytevec-ref-u8 bv i)
      (error:out-of-range i)))

(define (bytevec-set-u8! bv i val)
  (if (and (<= 0 i) (< i (bytevec-length-8 bv)))
      (if (and (<= 0 val) (< val #x100))
	  (primop bytevec-set-u8 bv i val)
	  (error:out-of-range val))
      (error:out-of-range i)))

(define (bytevec-length-16 bv)
  (if (bytevec? bv)
      (primop bytevec-length-16 bv)
      (error:wrong-type bv)))

(define (bytevec-ref-u16 bv i)
  (if (and (<= 0 i) (< i (bytevec-length-16 bv)))
      (primop bytevec-ref-u16 bv i)
      (error:out-of-range i)))

(define (bytevec-set-u16! bv i val)
  (if (and (<= 0 i) (< i (bytevec-length-16 bv)))
      (if (and (<= 0 val) (< val #x10000))
	  (primop bytevec-set-u16 bv i val)
	  (error:out-of-range val))
      (error:out-of-range i)))

(define (bytevec-u16 . vals)
  (let* ((n (length vals))
	 (v (make-bytevec (* 2 n))))
    (do ((i 0 (1+ i))
	 (vals vals (cdr vals)))
	((= i n) v)
      (bytevec-set-u16! v i (car vals)))))

(define (bytevec-subvector-u16 v start end)
  (bytevec-subvector v (* 2 start) (* 2 end)))

(define (bytevec-length-32 bv)
  (quotient (bytevec-length-16 bv) 2))

(define (bytevec-ref-u32 bv i)
  (+ (* (bytevec-ref-u16 bv (* 2 i)) #x10000)
     (bytevec-ref-u16 bv (1+ (* 2 i)))))

(define (bytevec-set-u32! bv i val)
  (bytevec-set-u16! bv (* 2 i) (quotient val #x10000))
  (bytevec-set-u16! bv (1+ (* 2 i)) (remainder val #x10000)))

(define (bytevec-subvector-u32 v start end)
  (bytevec-subvector v (* 4 start) (* 4 end)))

;;; Characters

(define (char? val)
  (primif (if-char? val) #t #f))

(define (integer->char n)
  (if (and (<= 0 n) (< n #x1000000))
      (primop integer->char n)
      (error:out-of-range n)))

(define (char->integer c)
  (if (char? c)
      (primop char->integer c)
      (error:wrong-type c)))

(define (char=? a b)
  (eq? a b))

;;; Strings

(define (string? val)
  (record-with-type? val string-type))

(define (make-string n)
  (record string-type (make-bytevec n)))

(define (string-chars str)
  (if (string? str)
      (record-ref str 0)
      (error:wrong-type str)))

(define (string . chars)
  (let ((str (make-string (length chars))))
    (do ((i 0 (+ i 1))
	 (c chars (cdr c)))
	((null? c))
      (string-set! str i (car c)))
    str))

(define (list->string list)
  (apply string list))

(define (string->list str)
  (let ((len (string-length str)))
    (do ((i (- len 1) (- i 1))
	 (l '() (cons (string-ref str i) l)))
	((< i 0) l))))

(define (string-length str)
  (bytevec-length-8 (string-chars str)))

(define (string-ref str idx)
  (integer->char (bytevec-ref-u8 (string-chars str) idx)))

(define (string-set! str idx chr)
  (bytevec-set-u8! (string-chars str) idx (char->integer chr)))

(define (number->string n . opt-base)
  (let ((base (if (null? opt-base) 10 (car opt-base))))
    (list->string
     (if (< n 0)
	 (cons #\- (reverse (positive-number->char-list (- n) base)))
	 (reverse (positive-number->char-list n base))))))

(define (integer->digit d)
  (integer->char (if (< d 10)
		     (+ (char->integer #\0) d)
		     (+ (char->integer #\A) (- d 10)))))
  
(define (positive-number->char-list n b)
  (let ((q (quotient n b))
	(r (remainder n b)))
    (cons (integer->digit r)
	  (if (zero? q)
	      '()
	      (positive-number->char-list q b)))))

(define (digit? ch)
  (and (<= (char->integer #\0) (char->integer ch))
       (<= (char->integer ch) (char->integer #\9))))

(define (string->number str)
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

(define (string-set-substring! str idx sub)
  (let ((n (string-length sub)))
    (do ((i 0 (+ i 1)))
	((= i n))
      (string-set! str (+ idx i) (string-ref sub i)))))

(define (string-append . strings)
  (let ((res (make-string (apply + (map string-length strings)))))
    (let loop ((s strings)
	       (i 0))
      (if (null? s)
	  res
	  (begin
	    (string-set-substring! res i (car s))
	    (loop (cdr s) (+ i (string-length (car s)))))))))

;;; Functions

(define (flatten-for-apply arg1 args+rest)
  (if (null? args+rest)
      arg1
      (cons arg1 (flatten-for-apply (car args+rest) (cdr args+rest)))))

(define (apply func arg1 . args+rest)
  (:apply func (flatten-for-apply arg1 args+rest)))

;;; Dynamic environment, continuations and multiple values

(define -dynamic-env- '())

(define (call/cc func)
  (let ((old-env -dynamic-env-))
    (:call/cc (lambda (k)
		(func (lambda results
			(set! -dynamic-env- old-env)
			(apply k results)))))))

(define (call/v producer consumer)
  (:call/v producer consumer))

(define (values . results)
  (call/cc (lambda (k) (apply k results))))

(define-macro (let-values1 bindings . body)
  ;; bindings -> (sym1 sym2 ... producer)
  (let* ((n (length bindings))
	 (producer (list-ref bindings (1- n)))
	 (vals (list-head bindings (1- n))))
    (pk `(call/v (lambda () ,producer) (lambda ,vals ,@body)))))

(define (call/de env func)
  (call/cc (lambda (k)
	     (set! -dynamic-env- env)
	     (call/v func k))))

(define (dynamic-environment)
  -dynamic-env-)

;;; Parameters

(define (make-parameter init)
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

(define (call/p parameter value func)
  (call/de (acons parameter value (dynamic-environment)) func))

;;; Numbers

(define fixnum-max  536870911)
(define fixnum-min -536870912)

(define (fixnum? obj)
  (primif (if-fixnum? obj) #t #f))

(define (bignum? x)
  (record-with-type? x bignum-type))

(define (bignum-limbs b)
  (record-ref b 0))

(define (bignum-length b)
  (bytevec-length-16 (bignum-limbs b)))

(define (bignum-ref b i)
  (let* ((l (bignum-limbs b))
	 (n (bytevec-length-16 l)))
    (if (>= i n)
	(if (>= (bytevec-ref-u16 l (1- n)) #x8000)
	    #xFFFF #x0000)
	(bytevec-ref-u16 l i))))

(define (bignum-sref b i)
  (let ((v (bignum-ref b i)))
    (if (>= v #x8000)
	(- v #x10000)
	v)))

(define (bignum-negative? b)
  (let* ((l (bignum-limbs b))
	 (n (bytevec-length-16 l)))
    (>= (bytevec-ref-u16 l (1- n)) #x8000)))

(define (fixnum->bignum n)
  (:primitive split-fixnum (hi lo) (n)
	      ((record bignum-type (bytevec-u16 lo hi)))))

(define (make-bignum-limbs n)
  (make-bytevec (* 2 n)))

(define (make-bignum-limbs-zero n)
  (let ((l (make-bignum-limbs n)))
    (do ((i 0 (1+ i)))
	((= i n))
      (bytevec-set-u16! l i 0))
    l))

(define (limbs->bignum x)
  (let loop ((i (1- (bytevec-length-16 x))))
    (let ((k (bytevec-ref-u16 x i)))
      (cond ((= i 0)
	     (if (>= k #x8000)
		 (- k #x10000)
		 k))
	    ((and (= k 0) (< (bytevec-ref-u16 x (1- i)) #x8000))
	     (loop (1- i)))
	    ((and (= k #xFFFF) (>= (bytevec-ref-u16 x (1- i)) #x8000))
	     (loop (1- i)))
	    ((and (= i 1) (< k #x2000))
	     (+ (bytevec-ref-u16 x 0) (* k #x10000)))
	    ((and (= i 1) (>= k #xe000))
	     (+ (bytevec-ref-u16 x 0) (* (- k #x10000) #x10000)))
	    (else
	     (pk-dbg 'trunc-to (1+ i))
	     (record bignum-type (bytevec-subvector-u16 x 0 (1+ i))))))))

(define (normalize-bignum b)
  (limbs->bignum (bignum-limbs b)))

(define-macro (pk-dbg . args)
  (car (last-pair args)))
  ;;`(pk ,@args))

(define (->bignum n)
  (if (fixnum? n)
      (fixnum->bignum n)
      (if (bignum? n)
	  n
	  (error:wrong-type n))))

(define (bignum-add a b)
  (pk-dbg 'add a b)
  (let* ((a-n (bignum-length a))
	 (b-n (bignum-length b))
	 (n (1+ (if (> a-n b-n) a-n b-n)))
	 (z (make-bignum-limbs n)))
    (pk-dbg a-n b-n n)
    (let loop ((i 0)
	       (k 0))
      (pk-dbg i)
      (if (< i n)
	  (:primitive add-fixnum2 (hi lo) ((pk-dbg 'a (bignum-ref a i))
					   (pk-dbg 'b (bignum-ref b i))
					   (pk-dbg 'k k))
		      ((begin
			 (pk-dbg 'res hi lo)
			 (bytevec-set-u16! z i lo)
			 (loop (1+ i) hi))))
	  (limbs->bignum (pk-dbg 'result z))))))

(define (bignum-sub a b)
  (pk-dbg 'sub a b)
  (let* ((a-n (bignum-length a))
	 (b-n (bignum-length b))
	 (n (1+ (if (> a-n b-n) a-n b-n)))
	 (z (make-bignum-limbs n)))
    (pk-dbg a-n b-n n)
    (let loop ((i 0)
	       (k 0))
      (pk-dbg i)
      (if (< i n)
	  (:primitive sub-fixnum2 (hi lo) ((pk-dbg 'a (bignum-ref a i))
					   (pk-dbg 'b (bignum-ref b i))
					   (pk-dbg 'k k))
		      ((begin
			 (pk-dbg 'res hi lo)
			 (bytevec-set-u16! z i lo)
			 (loop (1+ i) hi))))
	  (limbs->bignum (pk-dbg 'result z))))))

(define (bignum-mul a b)

  (define (mul a b)
    (pk-dbg 'mul a b)
    (let* ((a-n (bignum-length a))
	   (b-n (bignum-length b))
	   (n (+ a-n b-n))
	   (z (make-bignum-limbs-zero n)))
      (let loop-j ((j 0))
	(pk-dbg 'j j)
	(if (< j a-n)
	    (let loop-i ((i 0)
			 (k 0))
	      (pk-dbg 'i i)
	      (if (< i b-n)
		  (:primitive mul-fixnum2 (hi lo) ((pk-dbg 'a (bignum-ref a j))
						   (pk-dbg 'b (bignum-ref b i))
						   (pk-dbg 'c
						       (bytevec-ref-u16
							z (+ i j)))
						   (pk-dbg 'k k))
			      ((begin
				 (pk-dbg 'res hi lo)
				 (bytevec-set-u16! z (+ i j) lo)
				 (loop-i (1+ i) hi))))
		  (begin
		    (bytevec-set-u16! z (+ i j) k)
		    (loop-j (1+ j)))))
	    (limbs->bignum (pk-dbg 'result z))))))

  (if (bignum-negative? a)
      (if (bignum-negative? b)
	  (* (- a) (- b))
	  (- (* (- a) b)))
      (if (bignum-negative? b)
	  (- (* a (- b)))
	  (mul a b))))

(define (bignum-equal a b)
  (let* ((a-n (bignum-length a))
	 (b-n (bignum-length b))
	 (n (if (> a-n b-n) a-n b-n)))
    (let loop ((i 0))
      (if (< i n)
	  (if (= (bignum-ref a i) (bignum-ref b i))
	      (loop (1+ i))
	      #f)
	  #t))))

(define (bignum-less-than a b)
  (pk-dbg '< a b)
  (let* ((a-n (bignum-length a))
	 (b-n (bignum-length b))
	 (n (if (> a-n b-n) a-n b-n)))
    (let loop ((i (1- n)))
      (pk-dbg 'i i)
      (if (< i 0)
	  #f
	  (let ((v-a (if (= i (1- n)) (bignum-sref a i) (bignum-ref a i)))
		(v-b (if (= i (1- n)) (bignum-sref b i) (bignum-ref b i))))
	    (pk-dbg 'a v-a 'b v-b)
	    (cond ((< v-a v-b)
		   #t)
		  ((= v-a v-b)
		   (loop (1- i)))
		  (else
		   #f)))))))

(define (bignum-shift-limbs a n)
  (let* ((a-v (bignum-limbs a))
	 (a-n (bytevec-length-16 a-v))
	 (v (make-bignum-limbs (+ a-n n))))
    (do ((i 0 (1+ i)))
	((= i a-n))
      (bytevec-set-u16! v (+ i n) (bytevec-ref-u16 a-v i)))
    (do ((i 0 (1+ i)))
	((= i n))
      (bytevec-set-u16! v i 0))
    (limbs->bignum v)))

(define (quot-fixnum2 q1 q2 v)
  (:primitive quotrem-fixnum2 (ww rr) (q1 q2 v) (ww)))

(define (bignum-quotrem q v)
  
  (define (quot1 q q-n v v-n)
    (pk-dbg 'quot1 q q-n v v-n)
    ;; compute one limb of q/v.
    ;; (q[n-1] q[n-2]) / v[n-1] is less than b.
    (let* ((ww (quot-fixnum2 (bignum-ref q (- q-n 1))
			     (bignum-ref q (- q-n 2))
			     (bignum-ref v (- v-n 1))))
	   (v-shift (bignum-shift-limbs v (- q-n v-n 1)))
	   (r (->bignum (* ww v-shift))))
      (let loop ((ww ww)
		 (r r))
	(pk-dbg 'ww? ww)
	(if (> r q)
	    (loop (- ww 1)
		  (->bignum (- r v-shift)))
	    (values ww (->bignum (- q r)))))))

  (define (non-zero-bignum-length v)
    (let loop ((n (bignum-length v)))
      (if (or (zero? n)
	      (not (zero? (bignum-ref v (1- n)))))
	  n
	  (loop (1- n)))))

  (define (quotrem q v)
    (pk-dbg 'q q)
    (pk-dbg 'v v)
    (let* ((v-n (non-zero-bignum-length v))
	   (d (quotient #x10000 (1+ (bignum-ref v (1- v-n)))))
	   (v (->bignum (* d v)))
	   (q (->bignum (* d q)))
	   (q-n (+ (bignum-length q) 1))
	   (w (make-bignum-limbs (max 0 (- q-n v-n)))))
      (let loop ((q-n q-n)
		 (q q))
	(pk-dbg 'q q 'w w)
	(if (<= q-n v-n)
	    (values (limbs->bignum w)
		    (if (zero? q) (normalize-bignum q) (quotient q d)))
	    (let-values1 (ww q (quot1 q q-n v v-n))
	      (pk-dbg 'ww ww)
	      (bytevec-set-u16! w (- q-n v-n 1) ww)
	      (loop (1- q-n) q))))))

  (if (< q 0)
      (let-values1 (w r (bignum-quotrem (->bignum (- q)) v))
        (values (- w) (- r)))
      (if (< v 0)
	  (let-values1 (w r (quotrem q (->bignum (- v))))
            (values (- w) r))
	  (quotrem q v))))

(define (bignum-quotient q v)
  (let-values1 (w r (bignum-quotrem q v))
    w))

(define (bignum-remainder q v)
  (let-values1 (w r (bignum-quotrem q v))
    r))

(define (+:2 a b)
  (if (and (fixnum? a) (fixnum? b))
      (:primitive add-fixnum (res) (a b)
		  (res
		   (bignum-add (fixnum->bignum a) (fixnum->bignum b))))
      (bignum-add (->bignum a) (->bignum b))))

(define (+ . args)
  (reduce +:2 0 args))

(define (-:2 a b)
  (if (and (fixnum? a) (fixnum? b))
      (:primitive sub-fixnum (res) (a b)
		  (res
		   (bignum-sub (fixnum->bignum a) (fixnum->bignum b))))
      (bignum-sub (->bignum a) (->bignum b))))

(define (- arg . rest)
  (if (null? rest)
      (-:2 0 arg)
      (-:2 arg (apply + rest))))

(define (*:2 a b)
  (if (and (fixnum? a) (fixnum? b))
      (:primitive mul-fixnum (res) (a b)
		  (res
		   (bignum-mul (fixnum->bignum a) (fixnum->bignum b))))
      (bignum-mul (->bignum a) (->bignum b))))

(define (* . args)
  (reduce *:2 1 args))

(define (quotient a b)
  (if (and (fixnum? a) (fixnum? b))
      (primop quotient-fixnum a b)
      (bignum-quotient (->bignum a) (->bignum b))))

(define (remainder a b)
  (if (and (fixnum? a) (fixnum? b))
      (primop remainder-fixnum a b)
      (bignum-remainder (->bignum a) (->bignum b))))

(define (= a b)
  (if (and (fixnum? a) (fixnum? b))
      (eq? a b)
      (bignum-equal (->bignum a) (->bignum b))))

(define (zero? a)
  (= a 0))

(define (< a b)
  (if (and (fixnum? a) (fixnum? b))
      (primif (if-< a b) #t #f)
      (bignum-less-than (->bignum a) (->bignum b))))

(define (<= a b)
  (or (< a b) (= a b)))

(define (> a b)
  (< b a))

(define (>= a b)
  (<= b a))

(define (1+ a)
  (+ a 1))

(define (1- a)
  (- a 1))

(define (max:2 a b)
  (if (> a b) a b))

(define (max a . rest)
  (reduce max:2 a rest))

(define (min:2 a b)
  (if (< a b) a b))

(define (min a . rest)
  (reduce min:2 a rest))

(define (iota n)
  (let loop ((res '())
	     (i 0))
    (if (>= i n)
	(reverse res)
	(loop (cons i res) (1+ i)))))

(define (even? a)
  (zero? (remainder a 2)))

(define (odd? a)
  (not (even? a)))

(define (square x)
  (* x x))

(define (expt a b)
  (cond ((< b 0)
	 (error:out-of-range b))
	((zero? b)
	 1)
	((even? b)
	 (square (expt a (quotient b 2))))
	(else
	 (* a (expt a (1- b))))))

(define (number? x)
  ;;(fixnum? x))
  (or (fixnum? x) (bignum? x)))

;;; Syscalls

(define (sys:panic)
  (primop syscall))

(define (sys:peek . vals)
  (let ((n (length vals)))
    (cond ((= n 0)
	   (primop syscall 0))
	  ((= n 1)
	   (primop syscall 0 (car vals)))
	  ((= n 2)
	   (primop syscall 0 (car vals) (cadr vals)))
	  (else
	   (primop syscall 0 (car vals) (cadr vals) (caddr vals))))))

(define (sys:write fd buf start end)
  (if (bytevec? buf)
      (if (and (<= 0 start)
	       (<= start end))
	  (if (<= end (bytevec-length-8 buf))
	      (primop syscall 2 fd buf start end)
	      (error:out-of-range end))
	  (error:out-of-range start))
      (error:wrong-type buf)))

(define (sys:read fd buf start end)
  (if (bytevec? buf)
      (if (and (<= 0 start)
	       (<= start end))
	  (if (<= end (bytevec-length-8 buf))
	      (primop syscall 3 fd buf start end)
	      (error:out-of-range end))
	  (error:out-of-range start))
      (error:wrong-type buf)))

;;; Ports

(define (output-char port ch)
  (port 0 ch))

(define (output-string port str)
  (let ((len (string-length str)))
    (do ((i 0 (+ i 1)))
	((= i len))
      (output-char port (string-ref str i)))))

(define (input-char port)
  (port 1 #f))

(define (putback-char port ch)
  (port 2 ch))

(define (peek-char port)
  (let ((ch (input-char port)))
    (putback-char port ch)
    ch))

(define (make-sys-output-port fd)
  (lambda (op arg)
    (cond ((= op 0)
	   (let ((buf (make-bytevec 1)))
	     (bytevec-set-u8! buf 0 (char->integer arg))
	     (sys:write fd buf 0 1)))
	  ((= op 1)
	   (error "can't read from output port"))
	  (else
	   (error "not supported")))))

(define (make-sys-input-port fd)
  (let ((ch #f))
    (lambda (op arg)
      (cond ((= op 1)
	     (if ch
		 (let ((r ch))
		   (set! ch #f)
		   r)
		 (let ((buf (make-bytevec 1)))
		   (let ((res (sys:read fd buf 0 1)))
		     (cond ((= res 1)
			    (integer->char (bytevec-ref-u8 buf 0)))
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

(define current-input-port (make-parameter (make-sys-input-port 0)))
(define current-output-port (make-parameter (make-sys-output-port 1)))

;;; Input/Output

(define (make-print-state port writing?)
  (cons port writing?))

(define (print-state-port state)
  (car state))

(define (print-state-writing? state)
  (cdr state))

(define (print-string str state)
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

(define (print-character ch state)
  (let ((port (print-state-port state)))
    (if (print-state-writing? state)
	(begin
	  (output-string port "#\\")
	  (output-char port ch))
	(output-char port ch))))

(define (print-number n state)
  (output-string (print-state-port state) (number->string n)))

(define (print-list-tail p state)
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
      
(define (print-list p state)
  (let ((port (print-state-port state)))
    (output-char port #\()
    (print (car p) state)
    (print-list-tail (cdr p) state)))

(define (print-record r state)
  (let ((port (print-state-port state))
	(n (record-length r)))
    (output-string port "#<")
    (print (record-type-name (record-type r)) state)
    (do ((i 0 (+ i 1)))
	((= i n))
      (output-string port " ")
      (print (record-ref r i) state))
    (output-string port ">")))

(define (print-vector v state)
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

(define (print-bytevec v state)
  (let ((port (print-state-port state))
	(n (bytevec-length-8 v)))
    (output-string port "/")
    (do ((i 0 (+ i 1)))
	((= i n))
      (if (and (> i 0)
	       (zero? (remainder i 2)))
	  (output-string port "/"))
      (output-char port (integer->digit (quotient (bytevec-ref-u8 v i) 16)))
      (output-char port (integer->digit (remainder (bytevec-ref-u8 v i) 16))))
    (output-string port "/")))

(define (print-weird-symbol s state)
  (output-char (print-state-port state) #\|)
  (output-string (print-state-port state) (symbol->string s))
  (output-char (print-state-port state) #\|))

(define (print-symbol s state)
  (if (string->number (symbol->string s))
      (print-weird-symbol s state)
      (output-string (print-state-port state) (symbol->string s))))

(define (print-code val state)
  (output-string (print-state-port state) "#<code ")
  (print (code-insn-length val) state)
  (output-string (print-state-port state) " ")
  (print (code-lit-length val) state)
  (output-string (print-state-port state) ">"))

(define print-depth (make-parameter 0))

(define (print val state)
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
			((number? val)
			 (print-number val state))
			((string? val)
			 (print-string val state))
			((symbol? val)
			 (print-symbol val state))
			((keyword? val)
			 (output-string port ":")
			 (print-symbol (keyword->symbol val) state))
			((pair? val)
			 (print-list val state))
			((vector? val)
			 (print-vector val state))
			((bytevec? val)
			 (print-bytevec val state))
			((code? val)
			 (print-code val state))
			((record? val)
			 (print-record val state))
			(else
			 (output-string port "#<...>"))))))
      (output-string (print-state-port state) "#")))

(define (pretty-print val)
  ;; beauty is in the eye of the beholder
  (display val))

(define (display val)
  (print val (make-print-state (current-output-port) #f)))

(define (write val)
  (print val (make-print-state (current-output-port) #t)))

(define (newline)
  (display #\nl))

(define (make-parse-state port)
  port)

(define (parse-state-port state)
  state)

(define (parse-reduce p f i state)
  (let ((ch (input-char (parse-state-port state))))
    (if (p ch)
	(parse-reduce p f (f ch i) state)
	(begin
	  (putback-char (parse-state-port state) ch)
	  i))))

(define (parse-gather p state)
  (reverse (parse-reduce p cons '() state)))

(define (parse-skip p state)
  (parse-reduce p (lambda (ch v) #t) #f state))

(define (parse-comment state)
  (let ((ch (input-char (parse-state-port state))))
    (if (not (eq? ch #\newline))
	(parse-comment state))))

(define (whitespace? ch)
  (or (eq? ch #\space)
      (eq? ch #\tab)
      (eq? ch #\newline)))

(define (delimiter? ch)
  (or (whitespace? ch)
      (eq? ch #\()
      (eq? ch #\))
      (eq? ch #\;)))
      
(define (parse-whitespace state)
  (let ((ch (input-char (parse-state-port state))))
    (cond ((whitespace? ch)
	   (parse-whitespace state))
	  ((eq? ch #\;)
	   (parse-comment state)
	   (parse-whitespace state))
	  (else
	   (putback-char (parse-state-port state) ch)))))

(define (parse-token state)
  (let ((str (list->string
	      (parse-gather (lambda (ch) 
			      (not (delimiter? ch)))
			    state))))
    (or (string->number str)
	(string->symbol str))))

(define (must-parse ch state)
  (parse-whitespace state)
  (if (not (eq? ch (input-char (parse-state-port state))))
      (error "missing " ch)))

(define (parse-list state)
  (parse-whitespace state)
  (let ((ch (input-char (parse-state-port state))))
    (if (eq? ch #\))
	'()
	(begin
	  (putback-char (parse-state-port state) ch)
	  (let ((elt (parse state)))
	    (if (eq? dot-symbol elt)
		(let ((elt (parse state)))
		  (must-parse #\) state)
		  elt)
		(cons elt (parse-list state))))))))

(define (parse-string state)
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

(define (parse-sharp state)
  (let ((ch (input-char (parse-state-port state))))
    (cond ((eq? ch #\()
	   (list->vector (parse-list state)))
	  ((eq? ch #\t)
	   #t)
	  ((eq? ch #\f)
	   #f)
	  (else
	   (error "unsupported # construct: #" ch)))))

(define (parse state)
  (parse-whitespace state)
  (let ((ch (input-char (parse-state-port state))))
    (cond ((eq? ch #\()
	   (parse-list state))
	  ((eq? ch #\")
	   (parse-string state))
	  ((eq? ch #\#)
	   (parse-sharp state))
	  ((eq? ch #\')
	   (list 'quote (parse state)))
	  (else
	   (putback-char (parse-state-port state) ch)
	   (parse-token state)))))

(define (read)
  (parse (make-parse-state (current-input-port))))

;;; Records

(define (record? val)
  (primif (if-record? val #t) #t #f))

(define (record-with-type? val type)
  (primif (if-record? val type) #t #f))

(define (record-type rec)
  (if (record? rec)
      (primop record-desc rec)
      (error:wrong-type rec)))

(define (record-length rec)
  (primop record-ref (record-type rec) 0))

(define (record-ref rec idx)
  (if (and (<= 0 idx) (< idx (record-length rec)))
      (primop record-ref rec idx)
      (error:out-of-range idx)))

(define (record-set! rec idx val)
  (if (and (<= 0 idx) (< idx (record-length rec)))
      (primop record-set rec idx val)
      (error:out-of-range idx)))

(define (make-record type init)
  (primop make-record type (record-type-n-fields type) init))
  
(define (record type . values)
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

(define (make-record-type n-fields name)
  (if (fixnum? n-fields)
      (record record-type-type n-fields name)
      (error:wrong-type n-fields)))

(define (record-type-n-fields rec)
  (if (record-with-type? rec record-type-type)
      (record-ref rec 0)
      (error:wrong-type rec)))

(define (record-type-name rec)
  (if (record-with-type? rec record-type-type)
      (record-ref rec 1)
      (error:wrong-type rec)))

(define (record->list rec)
  (do ((i (record-length rec) (1- i))
       (l '() (cons (record-ref rec (1- i)) l)))
      ((zero? i) l)))

;;; Vectors

(define (vector? val)
  (primif (if-vector? val) #t #f))

(define (vector-length vec)
  (if (vector? vec)
      (primop vector-length vec)
      (error:wrong-type vec)))

(define (vector-ref vec idx)
  (if (and (<= 0 idx) (< idx (vector-length vec)))
      (primop vector-ref vec idx)
      (error:out-of-range idx)))

(define (vector-set! vec idx val)
  (if (and (<= 0 idx) (< idx (vector-length vec)))
      (primop vector-set vec idx val)
      (error:out-of-range idx)))

(define (make-vector n init)
  (if (fixnum? n)
      (primop make-vector n init)
      (error:out-of-range n)))

(define (vector . values)
  (let ((n (length values)))
    (let ((vec (make-vector n #f)))
      (do ((i 0 (+ i 1))
	   (v values (cdr v)))
	  ((= i n))
	(vector-set! vec i (car v)))
      vec)))

(define (list->vector lst)
  (apply vector lst))

(define (vector->list vec)
  (do ((i 0 (1+ i))
       (r '() (cons (vector-ref vec i) r)))
      ((= i (vector-length vec)) (reverse r))))

(define (subvector v start end)
  (let ((s (make-vector (- end start) #f)))
    (do ((i start (1+ i)))
	((= i end))
      (vector-set! s (- i start) (vector-ref v i)))
    s))

;;; Code

(define (code? val)
  (primif (if-code? val) #t #f))

(define (code-insn-length code)
  (if (code? code)
      (primop code-insn-length code)
      (error:wrong-type)))

(define (code-lit-length code)
  (if (code? code)
      (primop code-lit-length code)
      (error:wrong-type)))

(define (make-code insn-length lit-length)
  (pk 'make-code insn-length lit-length)
  (if (fixnum? insn-length)
      (if (fixnum? lit-length)
	  (primop make-code insn-length lit-length)
	  (error:wrong-type lit-length))
      (error:wrong-type insn-length)))

(define (code-insn-set-u16! code idx val)
  (if (code? code)
      (if (and (<= 0 idx) (< idx (* 2 (code-insn-length code))))
	  (if (and (<= 0 val) (< val #x10000))
	      (primop bytevec-set-u16 code idx val)
	      (error:out-of-range val))
	  (error:out-of-range idx))
      (error:wrong-type code)))

(define (code-lit-set! code idx val)
  (if (code? code)
      (if (and (<= 0 idx) (< idx (code-lit-length code)))
	  (primop vector-set code (+ (code-insn-length code) idx) val)
	  (error:out-of-range idx))
      (error:wrong-type code)))

(define (code insns lits)
  (let* ((insn-length (bytevec-length-32 insns))
	 (insn-length-16 (* 2 insn-length))
	 (lit-length (vector-length lits))
	 (code (make-code insn-length lit-length)))
    (do ((i 0 (1+ i)))
	((= i insn-length-16))
      (code-insn-set-u16! code i (bytevec-ref-u16 insns i)))
    (do ((i 0 (1+ i)))
	((= i lit-length))
      (code-lit-set! code i (vector-ref lits i)))
    code))

;;; equal?

(define (equal? a b)
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

(define (string-equal? a b)
  (let ((len (string-length a)))
    (and (= len (string-length b))
	 (do ((i 0 (+ i 1)))
	     ((or (= i len)
		  (not (equal? (string-ref a i) (string-ref b i))))
	      (= i len))))))

(define (vector-equal? a b)
  (let ((len (vector-length a)))
    (and (= len (vector-length b))
	 (do ((i 0 (+ i 1)))
	     ((or (= i len)
		  (not (equal? (vector-ref a i) (vector-ref b i))))
	      (= i len))))))

(define (record-equal? a b)
  (let ((len (record-length a)))
    (do ((i 0 (+ i 1)))
	((or (= i len)
	     (not (equal? (record-ref a i) (record-ref b i))))
	 (= i len)))))

(define (symbol-equal? a b)
  (eq? a b))

(define (pair-equal? a b)
  (and (equal? (car a) (car b))
       (equal? (cdr a) (cdr b))))

;;; Bootinfo

(define-macro (bootinfo)
  `(:bootinfo))

;;; Hash tables

(define (mix-hash n . vals)
  (let loop ((h 0)
	     (vals vals))
    (if (null? vals)
	h
	(loop (remainder (+ (* h 37) (car vals)) n)
	      (cdr vals)))))

(define (string-hash n str)
  (apply mix-hash n (map char->integer (string->list str))))

(define (symbol-hash n sym)
  (string-hash n (symbol->string sym)))

(define (pair-hash n p)
  (mix-hash n (hash (car p) n) (hash (cdr p) n)))

(define (vector-hash n vec)
  (apply mix-hash n (map (lambda (e) (hash e n)) (vector->list vec))))

(define (record-hash n rec)
  (apply mix-hash n (map (lambda (e) (hash e n)) (record->list rec))))

(define (hash obj n)
  (cond
   ((string? obj)
    (string-hash n obj))
   ((symbol? obj)
    (symbol-hash n obj))
   ((pair? obj)
    (pair-hash n obj))
   ((vector? obj)
    (vector-hash n obj))
   ((record? obj)
    (record-hash n obj))
   ((number? obj)
    (remainder obj n))
   ((char? obj)
    (hash (char->integer obj) n))
   ((eq? #t obj)
    (remainder 10 n))
   ((eq? #f obj)
    (remainder 18 n))
   ((eq? '() obj)
    (remainder 2 n))
   (else
    (error:wrong-type obj))))

(define (make-hash-table n)
  (make-vector n '()))

(define (hash-ref tab key)
  (assoc-ref (vector-ref tab (hash key (vector-length tab))) key))

(define (hash-set! tab key val)
  (let* ((h (hash key (vector-length tab)))
	 (c (assoc key (vector-ref tab h))))
    (if c
	(set-cdr! c val)
	(vector-set! tab h (acons key val (vector-ref tab h))))))

(define (make-hashq-table n)
  (let ((v (make-vector n '())))
    (set-hashq-vectors (cons v (get-hashq-vectors)))
    v))

(define (hashq-ref vec key)
  (if (vector? vec)
      (and=> (primop syscall 4 vec key #f) cdr)
      (error:wrong-type vec)))

(define (hashq-set! vec key val)
  (if (vector? vec)
      (let* ((new-pair (acons key val '()))
	     (c (primop syscall 4 vec key new-pair)))
	(if c
	    (set-cdr! c val)))
      (error:wrong-type vec)))

(define (hashq-del! vec key)
  (if (vector? vec)
      (primop syscall 5 vec key)
      (error:wrong-type vec)))

(define (hashq->alist! vec)
  (if (vector? vec)
      (primop syscall 6 vec)
      (error:wrong-type vec)))

(define (alist->hashq! alist vec)
  (if (vector? vec)
      (primop syscall 7 alist vec)
      (error:wrong-type vec)))

(define (get-hashq-vectors)
  (primop syscall 8 -3))

(define (set-hashq-vectors v)
  (primop syscall 9 -3 v))

;;; Symbols

(define symbols (let ((t (make-hash-table 213)))
		  (for-each (lambda (sym)
			      (hash-set! t (symbol->string sym) sym))
			    (car (bootinfo)))
		  t))

(define (symbol? val)
  (record-with-type? val symbol-type))

(define (string->symbol str)
  (if (string? str)
      (or (hash-ref symbols str)
	  (let ((sym (record symbol-type str)))
	    (hash-set! symbols str sym)
	    sym))
      (error:wrong-type str)))

(define (symbol->string sym)
  (if (record-with-type? sym symbol-type)
      (record-ref sym 0)
      (error:wrong-type sym)))

(define (symbol-append . syms)
  (string->symbol (apply string-append (map symbol->string syms))))

(define -gensym-counter- 0)

(define (gensym)
  (set! -gensym-counter- (+ -gensym-counter- 1))
  (string->symbol (string-append "G" (number->string -gensym-counter-))))

(define dot-symbol (string->symbol "."))

;;; Keywords

(define keywords (let ((t (make-hashq-table 213)))
		   (for-each (lambda (key)
			       (hashq-set! t (keyword->symbol key) key))
			     (cadr (bootinfo)))
		   t))

(define (keyword? val)
  (record-with-type? val keyword-type))

(define (keyword->symbol key)
  (if (record-with-type? key keyword-type)
      (record-ref key 0)
      (error:wrong-type key)))

;;; Variables, macros and name spaces

(define (variable? obj)
  (record-with-type? obj variable-type))

(define (variable init name)
  (record variable-type init name))

(define (variable-ref obj)
  (if (variable? obj)
      (record-ref obj 0)
      (error:wrong-type obj)))

(define (variable-set! obj val)
  (if (variable? obj)
      (record-set! obj 0 val)
      (error:wrong-type obj)))

(define (variable-name obj)
  (if (variable? obj)
      (record-ref obj 1)
      (error:wrong-type obj)))

(define (macro? obj)
  (record-with-type? obj macro-type))

(define (macro-transformer obj)
  (if (macro? obj)
      (record-ref obj 0)
      (error:wrong-type obj)))

(define toplevel (caddr (bootinfo)))

(define (register-toplevel-variable sym)
  (let ((var (variable (if #f #f) sym)))
    (set! toplevel (acons sym var toplevel))
    var))

(define (lookup-toplevel-variable sym)
  (let ((thing (assq-ref toplevel sym)))
    (if thing
	(if (variable? thing)
	    thing
	    (error "not a variable: " sym))
	(register-toplevel-variable sym))))

(define (lookup-toplevel-macro-transformer sym)
  (let ((thing (assq-ref toplevel sym)))
    (and (macro? thing) (macro-transformer thing))))
