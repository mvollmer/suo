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

(define-record cps-var name id boxed?)

(define-record cps-quote value)

(define-record cps-reg idx)

(define-record cps-app func args restp)

(define-record cps-func name args restp body)

(define-record cps-fix funcs cont)

(define-record cps-fun func cont)

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
	  
	  ((cps-func name args restp body)
	   `(,(cps-render name)
	     ,(map cps-render args)
	     ,restp
	     ,(cps-render body)))
	  
	  ((cps-primop type results args conts)
	   `(primop ,type
		    ,(map cps-render results)
		    ,(map cps-render args)
		    ,(map cps-render conts)))
	  (else
	   obj)))

	(else
	 obj)))

(define (cps-print obj)
  (pretty-print (cps-render obj))
  (newline))

(define (pk-cps . args)
  (apply pk (map cps-render args))
  (car (last-pair args)))

(define genvar
  (let ((counter 0))
    (lambda args
      (set! counter (1+ counter))
      (cps-var (string->symbol (string-append "v" (number->string counter)))
	       counter
	       (or (null? args)
		   (car args))))))

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

  (define (lookup-macro sym env)
    (and (not (lookup-sym sym env)) (lookup-toplevel-macro sym)))

  (define (lookup-variable sym env)
    (or (lookup-sym sym env)
	(lookup-toplevel-variable sym)))

  (define (conv-func func-label args body env)
    (let* ((cont-arg (genvar))
	   (restp (dotted-list? args))
	   (flat-args (flatten-dotted-list args))
	   (arg-vars (map (lambda (a) (genvar)) flat-args))
	   (box-vars (map (lambda (a) (genvar)) flat-args))
	   (body-env (extend-env* flat-args box-vars env)))
      (define (make-body arg-vars box-vars)
	(if (null? arg-vars)
	    (conv (cons :begin body)
		  body-env
		  (lambda (z)
		    (cps-app cont-arg (list z) #f)))
	    (cps-gen-box (car box-vars) 
			 (car arg-vars)
			 (make-body (cdr arg-vars) (cdr box-vars)))))
      (cps-func func-label
		(cons* cont-arg arg-vars)
		restp
		(make-body arg-vars box-vars))))

  (define (is-tail-call? obj result-var)
    ;; must be of the form (app K (result-var) #f)
    (and (cps-app? obj)
	 (= 1 (length (cps-app-args obj)))
	 (eq? result-var
	      (car (cps-app-args obj)))
	 (not (cps-app-restp obj))))
  
  (define (tail-call-cont obj)
    (cps-app-func obj))
    
  (define (make-cont env c z)
    (let* ((cont-label (genvar))
	   (result-var (genvar))
	   (cont-body (c result-var)))
      (if (is-tail-call? cont-body result-var)
	  (z (tail-call-cont cont-body))
	  (cps-fun (cps-func cont-label
			     (list result-var)
			     #f
			     cont-body)
		   (z cont-label)))))
	  
  (define (conv-apply func args restp env c)
    (conv* args env
	   (lambda (z-args)
	     (conv func env
		   (lambda (z-func)
		     (make-cont env c
				(lambda (z-cont)
				  (cps-app z-func
					   (cons* z-cont z-args)
					   restp))))))))

  (define (conv-call/v producer consumer env c)
    (conv producer env
	   (lambda (z-producer)
	     (conv consumer env
		   (lambda (z-consumer)
		     (make-cont
		      env c
		      (lambda (z-cont)
			(let ((producer-cont (genvar))
			      (results (genvar)))
			  (cps-fun (cps-func producer-cont
					     (list results)
					     #t
					     (cps-app z-consumer
						      (list z-cont
							    results)
						      #t))
				   (cps-app z-producer
					    (list producer-cont)
					    #f))))))))))

  (define (conv-call/cc func env c)
    (let ((cont-func-label (genvar))
	  (cont-func-cont (genvar))
	  (cont-func-args (genvar)))
      (conv func env
	    (lambda (z-func)
	      (make-cont env c
			 (lambda (z-cont)
			   (cps-fun (cps-func cont-func-label
					      (list cont-func-cont
						    cont-func-args)
					      #t
					      (cps-app z-cont
						       (list cont-func-args)
						       #t))
				    (cps-app z-func
					     (list z-cont cont-func-label)
					     #f))))))))

  (define (conv-primitive type results args conts env c)
    (conv* args env
	   (lambda (args-z)
	     (make-cont env c
			(lambda (z-cont)
			  (let ((app-c (lambda (z)
					 (cps-app z-cont
						  (list z)
						  #f))))
			    (let* ((result-vars (map (lambda (a) (genvar #f))
						     results))
				   (cont-env (extend-env* results result-vars
							  env)))
			      (cps-primop type 
					  result-vars
					  args-z
					  (map (lambda (cont)
						 (conv cont cont-env app-c))
					       conts)))))))))

  ;; Apply C to a cps-value representing the result of evaluating EXP.
  ;; C should return a cps-instruction that consumes the cps-value.
  ;;
  (define (conv exp env c)

    (pattern-case exp

       ((:bootinfo)
	(c (cps-quote suo-bootinfo-marker)))

       ((:define ?var ?val)
	(error "wrong 'define' placement"))

       ((:quote ?val)
	(c (cps-quote ?val)))

       ((:lambda ?args . ?body)
	(let ((func (genvar)))
	  (cps-fun (conv-func func ?args ?body env)
		   (c func))))

       ((:begin . ?body)
	;; XXX - allow multiple return values
	(conv* ?body
	       env
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
		      (if (cps-var-boxed? var)
			  (cps-gen-box-set var z cont)
			  (error "unmutable: " ?var))
		      (cps-gen-variable-set (cps-quote var) z cont))))))

       ((:call/cc ?func)
	(conv-call/cc ?func env c))

       ((:call/v ?producer ?consumer)
	(conv-call/v ?producer ?consumer env c))
	
       ((:apply ?func . ?args)
	(conv-apply ?func ?args #t env c))
	
       ((?func . ?args)
	(let ((macro (and (symbol? ?func)
			  (lookup-macro ?func env))))
	  (if macro
	      (conv (expand-macro macro exp) env c)
	      (conv-apply ?func ?args #f env c))))

       (else
	(if (symbol? exp)
	    (let ((var (lookup-variable exp env)))
	      (if (and (cps-var? var) (not (cps-var-boxed? var)))
		  (c var)
		  (let* ((result-var (genvar))
			 (cont (c result-var)))
		    (if (cps-var? var)
			(cps-gen-box-ref result-var var cont)
			(cps-gen-variable-ref result-var (cps-quote var)
					      cont)))))
	    (c (cps-quote exp))))))
  
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

;;; Used, bound, and free variables

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

    ((cps-func label args restp body)
     (let ((v (hashq-ref used-vars cps)))
       (or v
	   (begin
	     (let ((v (cps-used-vars body)))
	       (hashq-set! used-vars cps v)
	       v)))))

    ((cps-fun func cont)
     (let ((used
	    (union (cps-used-vars cont)
		   (cps-used-vars func))))
       (pk 'used (cps-render (cps-func-name func)) (length used))
       used))

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

    ((cps-func label args restp body)
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

    ((cps-func label args restp body)
     (set-difference (cps-free-vars body) args))

    ((cps-fun func cont)
     (set-difference (union (cps-free-vars cont)
			    (cps-free-vars func))
		     (list (cps-func-name func))))

    ((cps-primop type results args conts)
     (set-difference (union (map-union cps-free-vars args)
			    (map-union cps-free-vars conts))
		     results))))
  
(define (cps-free-vars-2 cps)
  (set-difference (cps-used-vars cps) (cps-bound-vars cps)))

(define (cps-count-nodes cps)
  (record-case cps

    ((cps-var)
     1)

    ((cps-quote val)
     1)

    ((cps-app func args restp)
     (+ 1 (cps-count-nodes func)
	(apply + (map cps-count-nodes args))))

    ((cps-func label args restp body)
     (+ 1 (cps-count-nodes body)))

    ((cps-fun func cont)
     (+ 1 (cps-count-nodes cont)
	(cps-count-nodes func)))

    ((cps-primop type results args conts)
     (+ 1
	(length results)
	(apply + (map cps-count-nodes args))
	(apply + (map cps-count-nodes conts))))))

;;; Parameters

(define-macro (define-param name)
  (let ((param-name (symbol-append name '/state)))
    `(begin (define ,param-name #f)
	    (define (,name) ,param-name))))

(define-macro (with-param param-val . body)
  (let* ((tmp (gensym))
	 (swap (gensym))
	 (param (car param-val))
	 (param-name (symbol-append param '/state)))
    `(let* ((,tmp ,(cadr param-val))
	    (,swap (lambda () (let ((old ,param-name))
				(set! ,param-name ,tmp)
				(set! ,tmp old)))))
       (dynamic-wind ,swap (lambda () ,@body) ,swap)))) 

;;; Dynamic attributes

(define-macro (define-dynamic-attr name)
  (let ((attr-name (symbol-append name '/state)))
    `(begin (define ,attr-name '())
	    (define (,name obj)
	      (assq-ref ,attr-name obj)))))
     
(define-macro (with-attr attr-obj-val . body)
  ;; attr-ob-val -> (attr obj val)
  (let* ((tmp (gensym))
	 (swap (gensym))
	 (attr (car attr-obj-val))
	 (attr-name (symbol-append attr '/state)))
    `(let* ((,tmp (acons ,(cadr attr-obj-val) 
			 ,(caddr attr-obj-val)
			 ,attr-name))
	    (,swap (lambda () (let ((old ,attr-name))
				(set! ,attr-name ,tmp)
				(set! ,tmp old)))))
       (dynamic-wind ,swap (lambda () ,@body) ,swap))))

(define (acons* keys vals lst)
  (cond ((null? keys)
	 lst)
	(else
	 (acons* (cdr keys) (cdr vals)
		 (acons (car keys) (car vals)
			lst)))))

(define-macro (with-attr* attr-objs-vals . body)
  ;; attr-ob-val -> (attr objs vals)
  (let* ((tmp (gensym))
	 (swap (gensym))
	 (attr (car attr-objs-vals))
	 (attr-name (symbol-append attr '/state)))
    `(let* ((,tmp (acons* ,(cadr attr-objs-vals) 
			  ,(caddr attr-objs-vals)
			  ,attr-name))
	    (,swap (lambda () (let ((old ,attr-name))
				(set! ,attr-name ,tmp)
				(set! ,tmp old)))))
       (dynamic-wind ,swap (lambda () ,@body) ,swap))))

;;; Closure conversion and calling convention implementation of FUNs

;; Calling convention: all arguments are passed as a list.

;; Create a closure record for CODE and VALUES, bind it to RESULT and
;; continue with CONT.  No type checking.
;;
(define (cps-gen-make-closure result code values cont)
  (let ((vector-var (genvar)))
    (cps-primop 'vector
		(list vector-var)
		values
		(list (cps-primop 'record
				  (list result)
				  (list (cps-quote closure-type)
					code vector-var)
				  (list cont))))))

;; Get the code object out of the closure record CLOSURE and continue
;; with CONT.
;;
(define (cps-gen-closure-ref-code result closure cont)
  (let* ((error-closure (record-ref 
			 (lookup-toplevel-variable 'error:not-a-closure)
			 0)))
    (cps-primop 'if-record?
		'()
		(list closure (cps-quote closure-type))
		(list (cps-primop 'record-ref
				  (list result)
				  (list closure (cps-quote 0))
				  (list cont))
		      (if (not (eq? error-closure (if #f #f)))
			  (let ((p1 (genvar))
				(p2 (genvar)))
			    (cps-primop 
			     'cons
			     (list p1)
			     (list closure (cps-quote '()))
			     (list (cps-primop
				    'cons
				    (list p2)
				    (list (cps-quote #f) p1)
				    (list (cps-app (cps-quote
						    (record-ref
						     error-closure 0))
						   (list (cps-quote
							  error-closure)
							 p2)
						   #f))))))
			  (cps-primop 'syscall
				      (list result)
				      '()
				      (list cont)))))))

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

;; Return cps-instructions to create a list containing VALUES and call
;; C with the cps-value that represents it.
;;
(define (cps-gen-arglist values restp c)
  (cond ((and restp (null? (cdr values)))
	 (c (car values)))
	((null? values)
	 (c (cps-quote '())))
	(else
	 (cps-gen-arglist (cdr values)
			  restp
			  (lambda (z)
			    (let ((var (genvar)))
			      (cps-primop 'cons
					  (list var)
					  (list (car values) z)
					  (list (c var)))))))))

;; Put the elements of LST into VARS, continuing with CONT.
;;
(define (cps-gen-unarglist vars restp lst cont)
  (let* ((error-closure (record-ref 
			 (lookup-toplevel-variable 'error:wrong-num-args)
			 0))
	 (error-cps (and (not (eq? error-closure (if #f #f)))
			 (cps-app (cps-quote
				   (record-ref error-closure 0))
				  (list (cps-quote error-closure)
					(cps-quote (list #f)))
				  #f))))

    (define (gen vars lst)
      (cond ((null? vars)
	     (if error-cps
		 (cps-primop 'if-eq?
			     '()
			     (list lst (cps-quote '()))
			     (list cont error-cps))
		 cont))
	    ((and restp (null? (cdr vars)))
	     (cps-primop 'identity
			 (list (car vars))
			 (list lst)
			 (list cont)))
	    (else
	     (let* ((cdr-var (genvar))
		    (cont (cps-primop 
			   'car
			   (list (car vars))
			   (list lst)
			   (list (cps-primop 
				  'cdr
				  (list cdr-var)
				  (list lst)
				  (list (gen (cdr vars) cdr-var)))))))
	       (if error-cps
		   (cps-primop 'if-pair?
			       '()
			       (list lst)
			       (list cont error-cps))
		   cont)))))

    (gen vars lst)))

(define-dynamic-attr cps-replacement)

(define (cps-closure-convert cps)
  (record-case cps
	       
    ((cps-var)
     (or (cps-replacement cps) cps))

    ((cps-quote)
     cps)

    ((cps-app func args restp)
     (let ((closure (cps-closure-convert func))
	   (code-var (genvar)))
       (cps-gen-arglist (map cps-closure-convert args)
			restp
			(lambda (arglist)
			  (cps-gen-closure-ref-code
			   code-var
			   closure
			   (cps-app code-var (list closure arglist) #f))))))

    ((cps-func label args restp body)
     (cps-func label
	       (map cps-closure-convert args)
	       restp
	       (cps-closure-convert body)))

    ((cps-fun func cont)
     (let* (;; closure
	    (closure-arg (genvar))
	    (closure-var (genvar))
	    (free-vars (cps-free-vars func))
	    (closure-vars (map (lambda (v) (genvar)) free-vars))
	    ;; aguments
	    (arglist-arg (genvar))
	    (func-args (cps-func-args func))
	    (func-args-vars (map (lambda (v) (genvar)) func-args))
	    (func-name (cps-func-name func)))

       (define (do-closure cont)
	 (cps-gen-open-closure closure-vars closure-arg
			       (with-attr* (cps-replacement free-vars
							    closure-vars)
			         (cont))))

       (define (do-args cont)
	 (cps-gen-unarglist func-args-vars (cps-func-restp func) 
			    arglist-arg
			    (with-attr* (cps-replacement func-args
							 func-args-vars)
			      (cont))))
	 
       ;;(pk-cps 'free (cps-func-name func) free-vars)

       (cps-fun (cps-func func-name
			  (list closure-arg arglist-arg)
			  #f
			  (do-args 
			   (lambda ()
			     (do-closure
			      (lambda ()
				(cps-closure-convert (cps-func-body func)))))))
		(cps-gen-make-closure closure-var
				      func-name
				      (map cps-closure-convert free-vars)
				      (with-attr (cps-replacement func-name
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
    
    ((cps-func label args restp body)
     (with-param (cps-next-register 0)
       (allocate-regs args
		      (lambda ()
			(cps-func label
				  (map cps-register-allocate args)
				  restp
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
  
  (record-case cps

    ((cps-var)
     ;; refers to a function and its replacement is (cps-quote code)
     (cps-replacement cps))

    ((cps-reg)
     cps)

    ((cps-quote)
     cps)

    ((cps-app func args restp)
     (if restp
	 (error "can't generate code for rest arguments"))
     (cps-asm-shuffle (cps-asm-context)
		      (map cps-code-generate args)
		      (map cps-reg (iota (length args))))
     (cps-asm-go (cps-asm-context) (cps-code-generate func)))

    ((cps-func label args restp body)
     (if restp
	 (error "can't generate code for rest arguments"))
     (let ((ctxt (cps-asm-make-context)))
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
	 (cps-print cps))))

(define (cps-compile cps)
  (cps-dbg 'cps cps)
  (set! used-vars-calls 0)
  (let ((clos (cps-closure-convert cps)))
    ;;(pk 'used-vars-calls used-vars-calls (cps-count-nodes cps))
    (cps-dbg 'clos clos)
    (let ((regs (cps-register-allocate (cps-fun-func clos))))
      (cps-dbg 'regs regs)
      (record closure-type
	      (cps-code-generate regs)
	      #()))))

;; Takes EXP and returns a new expression that has the same effect as
;; EXP when evaluated.

(define (compile exp)
  (let ((exp (macroexpand exp)))
    (if (and (pair? exp) (eq? (car exp) :lambda))
	`(:quote ,(cps-compile (cps-convert exp)))
	exp)))
