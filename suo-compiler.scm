;;; The compiler

;; The compiler performs the following passes
;;
;; - conversion to continuation-passing-style
;; - closure and argument conversion
;; - code generation

;;; CPS data structures

;; The CPS data structure consists of 'values' and 'instructions'.  A
;; 'value' represents a storage location with a constant contents.  An
;; 'instruction' represents some action, such as creating a new
;; location and initializing it.
;;
;; Both instructions and values are represented as Scheme objects that
;; carry a cache of properties and maybe additional information.

;; Values
;;
;; - (var SYMBOL)
;;
;;   The variable named SYMBOL.
;;
;; - (quote OBJECT)
;;
;;   An arbitrary literal object, such as a number or a string.
;;
;; - (reg IDX)
;;
;;   The register with number IDX.  These only appear after register
;;   allocation.

;; Instructions
;;
;; - (app VALUE1 VALUES...)
;;
;;   Jumps to the function indicated by VALUE1, passing it VALUES as
;;   the arguments.
;;
;; - (set VAR VALUE)
;;
;;   Set VAR to VALUE.  This only appears before boxifying.
;;
;; - (fix ((SYMBOL1 (SYMBOLS) BODY)... ) CONT)
;;
;;   Introduces functions named SYMBOL1, expecting SYMBOLS as their
;;   arguments, with BODY as their body.
;;
;; - (fun (SYMBOL (SYMBOLS...) BODY) CONT)
;;
;;   Introduces a single function named SYMBOL.  SYMBOL is only
;;   visible in CONT.
;;
;; - (primop OP (RESULTS...) (ARGUMENTS...) (CONTS...))
;;
;;   Represents the primitive operation indicated by OP, binding
;;   RESULTS (symbols) to values computed from the ARGUMENTS
;;   (cps-values) and continuing with one of the CONTS
;;   (cps-instructions).
;;
;; CPS structures are constant; when applying transformations, a new
;; structure is constructed.

(define-record cps-var name id)

(define-record cps-quote value)

(define-record cps-reg idx)

(define-record cps-app func args restp)

(define-record cps-func name args restp source parent-cont body)

(define-record cps-fix funcs cont)

(define-record cps-fun func cont)

(define-record cps-set var value cont)

(define-record cps-primop type results args conts)

;;; Printing a CPS structure

(define (cps-render obj)
  (cond ((pair? obj)
	 (cons (cps-render (car obj))
	       (cps-render (cdr obj))))

	((record? obj)
	 (record-case obj
	  
	  ((cps-var name)
	   name)
	  
	  ((cps-quote value)
	   `(quote ,value))
	  
	  ((cps-reg idx)
	   idx)
	  
	  ((cps-app func args restp)
	   `(app ,(cps-render func)
		 ,@(map cps-render args)
		 ,restp))
	  
	  ((cps-fix funcs cont)
	   `(fix ,(map cps-render funcs)
		 ,(cps-render cont)))
	  
	  ((cps-fun func cont)
	   `(fun ,(cps-render func)
		 ,(cps-render cont)))
	  
	  ((cps-func name args restp source parent-cont body)
	   `(,(cps-render name)
	     ,(map cps-render args)
	     ,restp
	     ,(cps-render parent-cont)
	     ,(cps-render body)))
	  
	  ((cps-set var value cont)
	   `(set ,(cps-render var) ,(cps-render value)
		 ,(cps-render cont)))

	  ((cps-primop type results args conts)
	   `(primop ,type
		    ,(map cps-render results)
		    ,(map cps-render args)
		    ,(map cps-render conts)))
	  (else
	   obj)))

	(else
	 obj)))

(define (cps-print-val obj)
  (record-case obj
    ((cps-var name)
     (display name))
    ((cps-quote value)
     (display "'")
     (display value))
    ((cps-reg idx)
     (display idx))
    (else
     (display (cps-render obj)))))

(define (cps-print-stat obj indent)

  (define (nl)
    (newline)
    (do ((i 0 (1+ i)))
	((= i indent))
      (display " ")))

  (define (print-val-list vals restp)
    (display "(")
    (let loop ((v vals)
	       (first #t))
      (cond ((not (null? v))
	     (if (not first)
		 (display " "))
	     (if (and (null? (cdr v)) restp)
		 (display ". "))
	     (cps-print-val (car v))
	     (loop (cdr v) #f))))
    (display ")"))

  (record-case obj

    ((cps-app func args restp)
     (nl)
     (if restp
	 (display "(app* ")
	 (display "(app "))
     (cps-print-val func)
     (for-each (lambda (a)
		 (display " ")
		 (cps-print-val a))
	       args)
     (display ")"))
	  
    ((cps-fun func cont)
     (nl)
     (display "(fun (")
     (cps-print-val (cps-func-name func))
     (display " ")
     (print-val-list (cps-func-args func) (cps-func-restp func))
     (cps-print-stat (cps-func-body func) (+ indent 6))
     (display ")")
     (cps-print-stat cont (+ indent 2))
     (display ")"))

    ((cps-set var value cont)
     (nl)
     (display "(set ")
     (cps-print-val var)
     (display " ")
     (cps-print-val value)
     (display ")")
     (cps-print-stat cont indent))

    ((cps-primop type results args conts)
     (nl)
     (display "(prim ")
     (display type)
     (display " ")
     (print-val-list results #f)
     (display " ")
     (print-val-list args #f)
     (if (= (length conts) 1)
	 (begin
	   (display ")")
	   (cps-print-stat (car conts) indent))
	 (begin
	   (for-each (lambda (c)
		       (nl)
		       (display " (begin")
		       (cps-print-stat c (+ indent 3))
		       (display ")"))
		     conts)
	   (display ")"))))
    
    (else
     (nl)
     (display (cps-render obj)))))

(define (pk-cps . args)
  (apply pk (map cps-render args))
  (car (last-pair args)))

(define genvar-counter 0)

(define (genvar)
  (set! genvar-counter (1+ genvar-counter))
  (cps-var (string->symbol (string-append "v" (number->string genvar-counter)))
	   genvar-counter))

;;; Code generation hooks for the compiler

;; Bind a box to RESULT, put VALUE into it and continue with CONT
;;
(define (cps-gen-box result value cont)
  (cps-primop 'record
	      (list result)
	      (list (cps-quote box-type) value)
	      (list cont)))

;; Bind the value in BOX to RESULT and continue with CONT
;;
(define (cps-gen-box-ref result box cont)
  (cps-primop 'record-ref
	      (list result)
	      (list box (cps-quote 0))
	      (list cont)))

;; Put VALUE into BOX and continue with CONT
;;
(define (cps-gen-box-set box value cont)
  (cps-primop 'record-set
	      (list (genvar))
	      (list box (cps-quote 0) value)
	      (list cont)))

;; If VALUE is true, continue with THEN, else with ELSE
;;
(define (cps-gen-if-eq a b then else)
  (cps-primop 'if-eq
	      '()
	      (list a b)
	      (list then else)))

;; Bind the value in VARIABLE to RESULT and continue with CONT
;;
(define (cps-gen-variable-ref result variable cont)
  (cps-primop 'record-ref
	      (list result)
	      (list variable (cps-quote 0))
	      (list cont)))

;; Put VALUE into VARIABLE and continue with CONT
;;
(define (cps-gen-variable-set variable value cont)
  (cps-primop 'record-set
	      (list (genvar))
	      (list variable (cps-quote 0) value)
	      (list cont)))

;;; Parameters

(define-macro (define-param name)
  `(define ,name (make-parameter #f)))

(define-macro (with-param param-val . body)
  (let* ((param-exp (car param-val))
	 (value-exp (cadr param-val)))
    `(call/p ,param-exp ,value-exp (lambda () ,@body))))

;;; Conversion into CPS

;; Mini-Scheme is a subset of Scheme that is straightforward to
;; translate into CPS.  Translation from Scheme into Mini-Scheme is
;; done via Scheme macros.
;;
;; Mini-Scheme has the following forms:
;;
;; - SYMBOL
;;
;; - LITERAL
;;
;; - (:quote OBJECT)
;;
;; - (:lambda ARGS BODY)
;;
;; - (:begin EXP...)
;;
;; - (:primitive TYPE RESULTS ARGS CONTS)
;;
;; - (:set SYMBOL EXP)
;;
;; - (FUNC ARGS)

(define non-rest-sigs (make-vector 30 0))
(define rest-sigs (make-vector 30 0))

(define non-rest-calls (make-vector 30 0))
(define rest-calls (make-vector 30 0))

(define (log-signature n restp)
  (if restp
      (vector-set! rest-sigs n (1+ (vector-ref rest-sigs n)))
      (vector-set! non-rest-sigs n (1+ (vector-ref non-rest-sigs n)))))

(define (log-call n restp)
  (if restp
      (vector-set! rest-calls n (1+ (vector-ref rest-calls n)))
      (vector-set! non-rest-calls n (1+ (vector-ref non-rest-calls n)))))

(define (dump-sigs-n-calls)
  (pk 'rest-sigs rest-sigs)
  (pk 'nonr-sigs non-rest-sigs)
  (let ((all-sigs (map +
		       (vector->list non-rest-sigs)
		       (vector->list rest-sigs))))
    (pk 'all--sigs (list->vector all-sigs))
    (pk 'all--clos (apply + all-sigs)))
  (pk 'rest-calls rest-calls)
  (pk 'nonr-calls non-rest-calls)
  (let ((all-calls (map +
		       (vector->list non-rest-calls)
		       (vector->list rest-calls))))
    (pk 'all--calls (list->vector all-calls))
    (pk 'all--clos (apply + all-calls))))

(define (cps-func* label args restp source parent-cont body)
  (log-signature (length args) restp)
  (cps-func label args restp source parent-cont body))

(define (cps-app* func args restp)
  (log-call (length args) restp)
  (cps-app func args restp))

(define cps-source (make-parameter #f))
(define cps-parent-cont (make-parameter #f))

(define (cps-convert exp)

  (define (make-env) '())
  (define (extend-env sym value env) (acons sym value env))
  (define (lookup-sym sym env) (assq-ref env sym))
  (define (extend-env* syms vals env)
    (if (null? syms)
	env
	(extend-env* (cdr syms) (cdr vals)
		     (extend-env (car syms) (car vals) env))))

  (define (bound-variable? val) (cps-var? val))

  (define (lookup-macro-transformer sym env)
    (and (not (lookup-sym sym env))
	 (lookup-toplevel-macro-transformer sym)))

  (define (lookup-variable sym env)
    (or (lookup-sym sym env)
	(lookup-toplevel-variable sym)))

  (define (conv-func func-label args body env)
    (let* ((cont-arg (genvar))
	   (restp (dotted-list? args))
	   (flat-args (flatten-dotted-list args))
	   (arg-vars (map (lambda (a) (genvar)) flat-args))
	   (body-env (extend-env* flat-args arg-vars env)))
      (cps-func* func-label
		(cons* cont-arg arg-vars)
		restp
		(cps-source) (cps-quote #f)
		(with-param (cps-parent-cont cont-arg)
		  (conv (cons :begin body)
			body-env
			(lambda (z)
			  (cps-app* cont-arg (list z) #f)))))))

  (define (is-tail-call? obj result-var)
    ;; must be of the form (app K (result-var) #f)
    (and ;; #f
	 (cps-app? obj)
	 (= 1 (length (cps-app-args obj)))
	 (eq? result-var
	      (car (cps-app-args obj)))
	 (not (cps-app-restp obj))))
  
  (define (tail-call-cont obj)
    (cps-app-func obj))
    
  (define (make-cont source env c z)
    (let* ((cont-label (genvar))
	   (result-var (genvar))
	   (cont-body (c result-var)))
      (if (is-tail-call? cont-body result-var)
	  (z (tail-call-cont cont-body))
	  (cps-fun (cps-func* cont-label
			     (list result-var)
			     #f
			     source
			     (cps-parent-cont)
			     cont-body)
		   (z cont-label)))))
	  
  (define (conv-apply func args restp env c)
    (let ((source (cps-source)))
      (conv* args env
	     (lambda (z-args)
	       (conv func env
		     (lambda (z-func)
		       (make-cont source env c
				  (lambda (z-cont)
				    (cps-app* z-func
					      (cons* z-cont z-args)
					      restp)))))))))

  (define (conv-call/v producer consumer env c)
    (let ((source (cps-source)))
      (conv producer env
	    (lambda (z-producer)
	      (conv consumer env
		    (lambda (z-consumer)
		      (make-cont
		       source env c
		       (lambda (z-cont)
			 (let ((producer-cont (genvar))
			       (results (genvar)))
			   (cps-fun (cps-func* producer-cont
					       (list results)
					       #t
					       'call/v z-cont
					       (cps-app* z-consumer
							 (list z-cont
							       results)
							 #t))
				    (cps-app* z-producer
					      (list producer-cont)
					      #f)))))))))))

  (define (conv-call/cc func env c)
    (let ((source (cps-source))
	  (cont-func-label (genvar))
	  (cont-func-cont (genvar))
	  (cont-func-args (genvar)))
      (conv func env
	    (lambda (z-func)
	      (make-cont source env c
			 (lambda (z-cont)
			   (cps-fun (cps-func* cont-func-label
					      (list cont-func-cont
						    cont-func-args)
					      #t
					      'call/cc z-cont
					      (cps-app* z-cont
						       (list cont-func-args)
						       #t))
				    (cps-app* z-func
					     (list z-cont cont-func-label)
					     #f))))))))

  (define (conv-primitive type results args conts env c)
    (let ((source (cps-source)))
      (let* ((result-vars (map (lambda (a) (genvar))
			       results))
	     (cont-env (extend-env* results result-vars env)))
	(conv* args env
	       (lambda (args-z)
		 (if (= (length conts) 1)
		     (cps-primop type
				 result-vars
				 args-z
				 (list (conv (car conts) cont-env c)))
		     (make-cont source env c
				(lambda (z-cont)
				  (let ((app-c (lambda (z)
						 (cps-app* z-cont
							   (list z)
							   #f))))
				    (cps-primop 
				     type
				     result-vars
				     args-z
				     (map (lambda (cont)
					    (conv cont cont-env app-c))
					  conts)))))))))))

  ;; Apply C to a cps-value representing the result of evaluating EXP.
  ;; C should return a cps-instruction that consumes the cps-value.
  ;;
  (define (conv exp env c)

    (with-param (cps-source exp)

      (pattern-case exp

	  ((:bootinfo)
	   (c (cps-quote bootinfo-marker)))

	  ((:define ?var ?val)
	   (error "wrong 'define' placement: " exp))
	  
	  ((:quote ?val)
	   (c (cps-quote ?val)))
	  
	  ((:lambda ?args . ?body)
	   (let ((func (genvar)))
	     (cps-fun (conv-func func ?args ?body env)
		      (c func))))

	  ((:begin . ?body)
	   ;; XXX - allow multiple return values
	   (conv* ?body env
		  (lambda (zs)
		    (if (null? zs)
			(c (cps-quote (if #f #f)))
			(c (car (last-pair zs)))))))

	  ((:primitive ?type ?results ?args ?conts)
	   (conv-primitive ?type ?results ?args ?conts env c))

	  ((:set ?var ?value)
	   (conv ?value
		 env
		 (lambda (z)
		   (let ((var (lookup-variable ?var env))
			 (cont (c (cps-quote (if #f #f)))))
		     (if (cps-var? var)
			 (cps-set var z cont)
			 (cps-gen-variable-set (cps-quote var) z cont))))))

	  ((:call/cc ?func)
	   (conv-call/cc ?func env c))
	  
	  ((:call/v ?producer ?consumer)
	   (conv-call/v ?producer ?consumer env c))
	
	  ((:apply ?func . ?args)
	   (conv-apply ?func ?args #t env c))
	
	  ((?func . ?args)
	   (let ((transformer (and (symbol? ?func)
				   (lookup-macro-transformer ?func env))))
	     (if transformer
		 (conv (apply transformer ?args) env c)
		 (conv-apply ?func ?args #f env c))))

	  (else
	   (if (symbol? exp)
	       (let ((var (lookup-variable exp env)))
		 (if (cps-var? var)
		     (c var)
		     (let* ((result-var (genvar))
			    (cont (c result-var)))
		       (cps-gen-variable-ref result-var (cps-quote var)
					     cont))))
	       (c (cps-quote exp)))))))

  ;; Apply C to a list cps-values that represent the results of
  ;; evaluating EXPS, a list of expressions.  C should return a
  ;; cps-instruction that consumes the cps-values.
  ;; 
  ;; The generated cps-instructions will compute the EXPS in order.
  ;;
  (define (conv* exps env c)
    (if (null? exps)
	(c '())
	(conv (car exps) env
	      (lambda (car-z)
		(conv* (cdr exps) env
		       (lambda (cdr-z)
			 (c (cons car-z cdr-z))))))))

  (define (topfun cps)
    (if (and (cps-fun? cps)
	     (cps-primop? (cps-fun-cont cps))
	     (eq? 'bottom (cps-primop-type (cps-fun-cont cps))))
	cps
	(error "Can only compile lambdas")))

  (topfun (conv exp (make-env)
		(lambda (z)
		  (cps-primop 'bottom '() (list z) '())))))

;;; Used, bound, free, and modified variables

(define (union* sets)
  (reduce union '() sets))

(define (map-union func list)
  (union* (map func list)))

(define used-vars-calls 0)

(define used-vars (make-hash-table 103100))

(define (cps-used-vars cps)
  (set! used-vars-calls (1+ used-vars-calls))
  (record-case cps

    ((cps-var)
     (list cps))

    ((cps-quote val)
     '())

    ((cps-app func args restp)
     (union (cps-used-vars func)
	    (map-union cps-used-vars args)))

    ((cps-func label args restp source parent-cont body)
     (let ((v (hashq-ref used-vars cps)))
       (or v
	   (begin
	     (let ((v (union (cps-used-vars parent-cont)
			     (cps-used-vars body))))
	       (hashq-set! used-vars cps v)
	       v)))))

    ((cps-fun func cont)
     (let ((used
	    (union (cps-used-vars cont)
		   (cps-used-vars func))))
       (pk 'used (cps-render (cps-func-name func)) (length used))
       used))

    ((cps-set var value cont)
     (union (cps-used-vars value)
	    (cps-used-vars cont)))
    
    ((cps-primop type results args conts)
     (union (map-union cps-used-vars args)
	    (map-union cps-used-vars conts)))))

(define bound-vars (make-hash-table 103100))

(define (cps-bound-vars cps)
  (record-case cps

    ((cps-var)
     '())

    ((cps-quote val)
     '())

    ((cps-app func args restp)
     '())

    ((cps-func label args restp source parent-cont body)
     (union (list label)
	    (union args
		   (cps-bound-vars body))))

    ((cps-fun func cont)
     (let ((v (hashq-ref bound-vars cps)))
       (or v
	   (begin
	     (let ((v (union (cps-bound-vars cont)
			     (cps-bound-vars func))))
	       (hashq-set! bound-vars cps v)
	       v)))))

    ((cps-set var value cont)
     (cps-bound-vars cont))

    ((cps-primop type results args conts)
     (union results
	    (map-union cps-bound-vars conts)))))

(define (cps-free-vars cps)
  (record-case cps

    ((cps-var)
     (list cps))

    ((cps-quote val)
     '())

    ((cps-app func args restp)
     (union (cps-used-vars func)
	    (map-union cps-used-vars args)))

    ((cps-func label args restp source parent-cont body)
     (set-difference (union (cps-free-vars parent-cont)
			    (cps-free-vars body))
		     args))

    ((cps-fun func cont)
     (set-difference (union (cps-free-vars cont)
			    (cps-free-vars func))
		     (list (cps-func-name func))))

    ((cps-set var value cont)
     (cps-free-vars cont))

    ((cps-primop type results args conts)
     (set-difference (union (map-union cps-free-vars args)
			    (map-union cps-free-vars conts))
		     results))))
  
(define (cps-free-vars-2 cps)
  (set-difference (cps-used-vars cps) (cps-bound-vars cps)))

(define (cps-modified-vars cps)
  (record-case cps

    ((cps-var)
     '())

    ((cps-quote val)
     '())

    ((cps-app func args restp)
     '())

    ((cps-func label args restp source parent-cont body)
     (cps-modified-vars body))

    ((cps-fun func cont)
     (union (cps-modified-vars func)
	    (cps-modified-vars cont)))

    ((cps-set var value cont)
     (adjoin var (cps-modified-vars cont)))

    ((cps-primop type results args conts)
     (map-union cps-modified-vars conts))))

(define (cps-count-nodes cps)
  (record-case cps

    ((cps-var)
     1)

    ((cps-quote val)
     1)

    ((cps-app func args restp)
     (+ 1 (cps-count-nodes func)
	(apply + (map cps-count-nodes args))))

    ((cps-func label args restp source parent-cont body)
     (+ 1 (cps-count-nodes parent-cont) (cps-count-nodes body)))

    ((cps-fun func cont)
     (+ 1 (cps-count-nodes cont)
	(cps-count-nodes func)))

    ((cps-set var value cont)
     (+ 1 (cps-count-nodes var)
	(cps-count-nodes value)
	(cps-count-nodes cont)))

    ((cps-primop type results args conts)
     (+ 1
	(length results)
	(apply + (map cps-count-nodes args))
	(apply + (map cps-count-nodes conts))))))

;;; Dynamic attributes

(define-macro (define-dynamic-attr name)
  (let ((parm-name (symbol-append name '/parm)))
    `(begin (define ,parm-name (make-parameter '()))
 	    (define (,name obj)
 	      (assq-ref (,parm-name) obj)))))

(define-macro (with-attr attr-obj-val . body)
  ;; attr-obj-val -> (attr obj val)
  (let* ((attr-name (car attr-obj-val))
	 (parm-name (symbol-append attr-name '/parm))
	 (obj-exp (cadr attr-obj-val))
	 (val-exp (caddr attr-obj-val)))
    `(call/p ,parm-name (acons ,obj-exp ,val-exp (,parm-name))
	     (lambda () ,@body))))

(define (acons* keys vals lst)
  (cond ((null? keys)
	 lst)
	(else
	 (acons* (cdr keys) (cdr vals)
		 (acons (car keys) (car vals)
			lst)))))

(define-macro (with-attr* attr-objs-vals . body)
  ;; attr-objs-vals -> (attr objs vals)
  (let* ((attr-name (car attr-objs-vals))
	 (parm-name (symbol-append attr-name '/parm))
	 (objs-exp (cadr attr-objs-vals))
	 (vals-exp (caddr attr-objs-vals)))
    `(call/p ,parm-name (acons* ,objs-exp ,vals-exp (,parm-name))
	     (lambda () ,@body))))

(define-dynamic-attr cps-replacement)

;;; Boxification

;; Variables that are 'set' will be converted to boxes so that cps
;; variables are guaranteed to be constants.
;;
;; The strategy is to insert code at the start of a 'func' that
;; creates boxes for the arguments of that func that are modified by
;; the body and fetches values out of boxed free variables.

(define-dynamic-attr cps-box-of)

(define (cps-boxify cps)
  (record-case cps

    ((cps-var)
     (or (cps-replacement cps) cps))

    ((cps-quote)
     cps)

    ((cps-app func args restp)
     (cps-app (cps-boxify func)
	      (map cps-boxify args)
	      restp))

    ((cps-func label args restp source parent-cont body)

     (define (make-boxing-body c)
       (let loop ((modified-args (intersection args
					       (cps-modified-vars cps))))
	 (if (null? modified-args)
	     (c)
	     (let ((box-var (genvar)))
	       (cps-gen-box box-var 
			    (car modified-args)
			    (with-attr
			       (cps-box-of (car modified-args) box-var)
			      (loop (cdr modified-args))))))))
     
     (define (make-unboxing-body c)
       (let loop ((free (cps-free-vars cps)))
	 (if (null? free)
	     (c)
	     (let ((box (cps-box-of (car free))))
	       (if (not box)
		   (loop (cdr free))
		   (let ((val-var (genvar)))
		     (cps-gen-box-ref val-var box
				      (with-attr (cps-replacement (car free)
								  val-var)
				        (loop (cdr free))))))))))

     (cps-func label args restp
	       source (cps-boxify parent-cont)
	       (make-boxing-body 
		(lambda ()
		  (make-unboxing-body
		   (lambda ()
		     (cps-boxify body)))))))

    ((cps-fun func cont)
     (cps-fun (cps-boxify func)
	      (cps-boxify cont)))

    ((cps-set var value cont)
     (let ((box (cps-box-of var)))
       (cps-gen-box-set box value
			(with-attr (cps-replacement var value)
			  (cps-boxify cont)))))

    ((cps-primop type results args conts)
     (cps-primop type 
		 results
		 (map cps-boxify args)
		 (map cps-boxify conts)))))


;;; Closure conversion and calling convention implementation of FUNs

;; Calling convention: all arguments are passed in registers, rest
;; arguments are handled during code generation.

;; Create a closure record for FUNC and VALUES, bind it to RESULT and
;; continue with CONT.  No type checking.
;;
;; FUNC can be among the VALUES.  In that case, the freshly created
;; closure record is used instead of FUNC.
;;
(define (cps-gen-make-closure result func values parent-cont cont)
  (let ((vector-var (genvar))
	(func-index (list-index values func)))
    (cps-primop 'vector
		(list vector-var)
		(map (lambda (v) (if (eq? v func) (cps-quote #f) v)) values)
		(list (cps-primop 'record
				  (list result)
				  (list (cps-quote closure-type)
					func vector-var parent-cont)
				  (list (if func-index
					    (cps-primop
					     'vector-set
					     (list (genvar))
					     (list vector-var
						   (cps-quote func-index)
						   result)
					     (list cont))
					    cont)))))))

;; Get the code object out of the closure record CLOSURE and continue
;; with CONT.
;;
(define (cps-gen-closure-ref-code result closure cont)
  (let* ((error-closure (record-ref 
			 (lookup-toplevel-variable 'error:not-a-closure)
			 0))
	 (error-code (if (eq? error-closure (if #f #f))
			 #f
			 (record-ref error-closure 0))))
    (cps-primop 'if-record?
		'()
		(list closure (cps-quote closure-type))
		(list (cps-primop 'record-ref
				  (list result)
				  (list closure (cps-quote 0))
				  (list cont))
		      (if error-code
			  (cps-app (cps-quote error-code)
				   (list (cps-quote error-closure)
					 (cps-quote #f)
					 closure)
				   #f)
			  (cps-app (cps-quote #f) '() #f))))))

;; Put all values in the closure record CLOSURE into the VARIABLES and
;; continue with CONT.  No type checking.
;;
(define (cps-gen-open-closure variables closure cont)
  (let ((vector-var (genvar)))
    (define (open-them idx vars)
      (cond ((null? vars)
	     cont)
	    (else
	     (cps-primop 'vector-ref
			 (list (car vars))
			 (list vector-var (cps-quote idx))
			 (list (open-them (1+ idx) (cdr vars)))))))
    (cps-primop 'record-ref
		(list vector-var)
		(list closure (cps-quote 1))
		(list (open-them 0 variables)))))

(define (cps-closure-convert cps)
  (record-case cps
	       
    ((cps-var)
     (or (cps-replacement cps) cps))

    ((cps-quote)
     cps)

    ((cps-app func args restp)
     (let ((closure (cps-closure-convert func))
	   (converted-args (map cps-closure-convert args))
	   (code-var (genvar)))
       (cps-gen-closure-ref-code
	code-var
	closure
	(cps-app code-var (cons closure converted-args) restp))))

    ((cps-fun func cont)
     (let* ((closure-arg (genvar))
	    (closure-var (genvar))
	    (free-vars (cps-free-vars func))
	    (closure-vars (map (lambda (v) (genvar)) free-vars)))

       (define (do-closure cont)
	 (cps-gen-open-closure closure-vars closure-arg
			       (with-attr* (cps-replacement free-vars
							    closure-vars)
			         (cont))))

       ;;(pk-cps 'free (cps-func-name func) free-vars)

       (cps-fun (cps-func (cps-func-name func)
			  (cons closure-arg (cps-func-args func))
			  (cps-func-restp func)
			  (cps-func-source func)
			  #f
			  (do-closure
			   (lambda ()
			     (cps-closure-convert (cps-func-body func)))))
		(cps-gen-make-closure closure-var
				      (cps-func-name func)
				      (map cps-closure-convert free-vars)
				      (cps-closure-convert
				       (cps-func-parent-cont func))
				      (with-attr (cps-replacement
						  (cps-func-name func)
						  closure-var)
				        (cps-closure-convert cont))))))

    ((cps-primop type results args conts)
     (cps-primop type 
		 (map cps-closure-convert results)
		 (map cps-closure-convert args)
		 (map cps-closure-convert conts)))))

;;; Register allocation

;; Callig convention: all arguments as a list.

(define-param cps-next-register)

(define (cps-register-allocate cps)

  (define (allocate-regs vars cont)
    (let loop ((idx (cps-next-register))
	       (regs '())
	       (rest vars))
      (cond ((null? rest)
	     (with-param (cps-next-register idx)
	       (with-attr* (cps-replacement vars (reverse regs))
		 (cont))))
	    (else
	     (loop (1+ idx)
		   (cons (cps-reg idx) regs)
		   (cdr rest))))))

  (record-case cps
   
    ((cps-var)
     (or (cps-replacement cps) cps))
    
    ((cps-quote)
     cps)
    
    ((cps-app func args restp)
     (cps-app (cps-register-allocate func)
	      (map cps-register-allocate args)
	      restp))
    
    ((cps-func label args restp source parent-cont body)
     (with-param (cps-next-register 1)
       (allocate-regs args
		      (lambda ()
			(cps-func label
				  (map cps-register-allocate args)
				  restp
				  source parent-cont
				  (cps-register-allocate body))))))
    
    ((cps-fun func cont)
     (cps-fun (cps-register-allocate func)
	      (cps-register-allocate cont)))
    
    ((cps-primop type results args conts)
     (allocate-regs results
		    (lambda ()
		      (cps-primop type 
				  (map cps-register-allocate results)
				  (map cps-register-allocate args)
				  (map cps-register-allocate conts)))))))

;;; Code generation

;; Primops are directly turned into instructions, apps are compiled
;; into shuffle and go instructions and funs are done by recursing.
;; Very simple.

(define-param cps-asm-context)

(define (cps-code-generate cps)

  (define (call-signature args restp)
    (+ (* 2 (length args)) (if restp -1 0)))

  (record-case cps

    ((cps-var)
     ;; refers to a function and its replacement is (cps-quote code)
     (cps-replacement cps))

    ((cps-reg)
     cps)

    ((cps-quote)
     cps)

    ((cps-app func args restp)
     (if (and (cps-quote? func) (not (cps-quote-value func)))
	 (cps-asm-panic (cps-asm-context))
	 (begin
	   (cps-asm-shuffle (cps-asm-context)
			    (append (list 
				     (cps-quote (call-signature args restp)))
				    (map cps-code-generate args)
				    (list (cps-code-generate func)))
			    (map cps-reg (iota (+ (length args) 2))))
	   (cps-asm-go (cps-asm-context) (cps-reg (+ (length args) 1))))))

    ((cps-func label args restp source parent-cont body)
     (let ((ctxt (cps-asm-make-context source)))
       (cps-asm-prologue ctxt (call-signature args restp))
       (with-param (cps-asm-context ctxt)
         (cps-code-generate body))
       (cps-asm-finish ctxt)))

    ((cps-fun func cont)
     (let ((code (cps-code-generate func)))
       (with-attr (cps-replacement (cps-func-name func) (cps-quote code))
         (cps-code-generate cont))))

    ((cps-primop type results args conts)
     (let* ((ctxt (cps-asm-context))
	    (results (map cps-code-generate results))
	    (args (map cps-code-generate args))
	    (extra-cont-labels (map (lambda (cont) (cps-asm-make-label ctxt))
				    (cdr conts))))
       (cps-asm-primop ctxt type results args extra-cont-labels)
       (cps-code-generate (car conts))
       (for-each (lambda (cont label)
		   (cps-asm-def-label ctxt label)
		   (cps-code-generate cont))
		 (cdr conts) extra-cont-labels)))))

;;; Putting it together

(define cps-verbose #f)

(define (cps-dbg tag cps)
  (cond (cps-verbose
	 (pk tag)
	 (cps-print-stat cps 0)
	 (newline))))

(define (cps-compile cps)
  (cps-dbg 'cps cps)
  (set! used-vars-calls 0)
  (let ((boxed (cps-boxify cps)))
    (cps-dbg 'boxed boxed)
    (let ((clos (cps-closure-convert boxed)))
      ;;(pk 'used-vars-calls used-vars-calls (cps-count-nodes cps))
      (cps-dbg 'clos clos)
      (let ((regs (cps-register-allocate (cps-fun-func clos))))
	(cps-dbg 'regs regs)
	(record closure-type
		(cps-code-generate regs)
		#()
		#f)))))

;; Takes EXP and returns a new expression that has the same effect as
;; EXP when evaluated.

(define (compile exp)
  (set! genvar-counter 0)
  (let ((exp (macroexpand exp)))
    (if (and (pair? exp) (eq? (car exp) :lambda))
	`(:quote ,(cps-compile (cps-convert exp)))
	exp)))
