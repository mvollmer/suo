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

(define-struct cps-object ())

(define-struct cps-value (cps-object))

(define-struct cps-var (cps-value)
  name id)

(define-struct cps-quote (cps-value)
  value)

(define-struct cps-reg (cps-value)
  idx)

(define-struct cps-instruction (cps-object))

(define-struct cps-app (cps-instruction)
  func args)

(define-struct cps-func (cps-object)
  name args body)

(define-struct cps-fix (cps-instruction)
  funcs cont)

(define-struct cps-fun (cps-instruction)
  func cont)

(define-struct cps-primop (cps-instruction)
  type results args conts)

;;; Printing a CPS structure

(define-generic cps-render)

(define-method (cps-render obj)
  obj)

(define-method (cps-render (p <pair>))
  (cons (cps-render (car p)) (cps-render (cdr p))))

(define-method (cps-render (obj cps-var/class))
  (cps-var-name obj))

(define-method (cps-render (obj cps-quote/class))
  `(quote ,(cps-quote-value obj)))

(define-method (cps-render (obj cps-reg/class))
  (cps-reg-idx obj))

(define-method (cps-render (obj cps-app/class))
  `(app ,(cps-render (cps-app-func obj))
	,@(map cps-render (cps-app-args obj))))

(define-method (cps-render (obj cps-fix/class))
  `(fix ,(map cps-render (cps-fix-funcs obj))
	,(cps-render (cps-fix-cont obj))))

(define-method (cps-render (obj cps-fun/class))
  `(fun ,(cps-render (cps-fun-func obj))
	,(cps-render (cps-fun-cont obj))))

(define-method (cps-render (obj cps-func/class))
  `(,(cps-render (cps-func-name obj))
    ,(map cps-render (cps-func-args obj))
    ,(cps-render (cps-func-body obj))))

(define-method (cps-render (obj cps-primop/class))
  `(primop ,(cps-primop-type obj)
	   ,(map cps-render (cps-primop-results obj))
	   ,(map cps-render (cps-primop-args obj))
	   ,(map cps-render (cps-primop-conts obj))))

(define (cps-print obj)
  (pretty-print (cps-render obj)))

(define (pk-cps . args)
  (apply pk (map cps-render args))
  (car (last-pair args)))

;;; A simple toplevel

(define toplevel '())

(define-struct suo-macro ()
  expander)

(define-struct suo-variable ()
  value)

(define (register-thing name thing)
  (set! toplevel (acons name thing toplevel)))

(define (register-macro name expander)
  (register-thing name (suo-macro expander)))

(define-macro (define-suo-macro args . body)
  `(register-macro ',(car args)
		   (lambda (form)
		     (apply (lambda ,(cdr args) ,@body) (cdr form)))))

(define (register-variable name value)
  (register-thing name (suo-variable value)))

(define-macro (define-suo-variable name value)
  `(register-variable ',name ,value))

(define (lookup-global sym)
  (and=> (assq sym toplevel) cdr))

(define (lookup-global-macro sym)
  (let ((thing (lookup-global sym)))
    (if (and thing (suo-macro? thing))
	thing
	#f)))

(define (lookup-global-variable sym)
  (let ((thing (lookup-global sym)))
    (if (and thing (suo-variable? thing))
	thing
	#f)))

(define (expand-macro macro form)
  ((suo-macro-expander macro) form))


(define genvar
  (let ((counter 0))
    (lambda ()
      (set! counter (1+ counter))
      (cps-var (string->symbol (string-append "v" (number->string counter)))
	       counter))))


;;; Code generation hooks for the compiler

(define suo:descriptor-descriptor (suo:record #f 1))
(suo:record-set-desc! suo:descriptor-descriptor
		      suo:descriptor-descriptor)

(define suo:box-descriptor (suo:record suo:descriptor-descriptor 1))

(define suo:variable-descriptor (suo:record suo:descriptor-descriptor 1))

(define suo:closure-descriptor (suo:record suo:descriptor-descriptor 2))

;; Bind a box to RESULT, put VALUE into it and continue with CONT
;;
(define (cps-gen-box result value cont)
  (cps-primop 'record
	      (list result)
	      (list (cps-quote suo:box-descriptor) value)
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
	      '()
	      (list box (cps-quote 0) value)
	      (list cont)))

;; If VALUE is true, continue with THEN, else with ELSE
;;
(define (cps-gen-if-true value then else)
  (cps-primop 'if-eq
	      '()
	      (list value (cps-quote #t))
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
	      '()
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
;; - (:primitive OP ARG...)
;;
;; - (:if COND THEN ELSE)
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
    (and (not (lookup-sym sym env)) (lookup-global-macro sym)))

  (define (conv-func func-label args body env)
    (let* ((cont-arg (genvar))
	   (arg-vars (map (lambda (a) (genvar)) args))
	   (box-vars (map (lambda (a) (genvar)) args))
	   (body-env (extend-env* args box-vars env)))
      (define (make-body arg-vars box-vars)
	(if (null? arg-vars)
	    (conv (cons :begin body)
		  body-env
		  (lambda (z)
		    (cps-app cont-arg (list z))))
	    (cps-gen-box (car box-vars) 
			 (car arg-vars)
			 (make-body (cdr arg-vars) (cdr box-vars)))))
      (cps-func func-label
		(cons* cont-arg arg-vars)
		(make-body arg-vars box-vars))))

  (define (is-tail-call? obj result-var)
    ;; must be of the form (app K result-var)
    (and (cps-app? obj)
	 (= 1 (length (cps-app-args obj)))
	 (eq? result-var
	      (car (cps-app-args obj)))))
  
  (define (tail-call-cont obj)
    (cps-app-func obj))
    
  (define (conv-apply func args env c)
    (let* ((cont-label (genvar))
	   (result-var (genvar))
	   (cont-body (c result-var)))

      (define (gen-call make-cont)
	(conv* args env
	       (lambda (z-args)
		 (conv func env
		       (lambda (z-func)
			 (make-cont (lambda (z-cont)
				      (cps-app z-func
					       (cons* z-cont z-args)))))))))
      
      (if (is-tail-call? cont-body result-var)
	  (gen-call (lambda (c) (c (tail-call-cont cont-body))))
	  (cps-fun (cps-func cont-label
			     (list result-var)
			     cont-body)
		   (gen-call (lambda (c)
			       (c cont-label)))))))

  (define (conv-if cond then else env c)
    (let* ((cont-label (genvar))
	   (result-var (genvar))
	   (cont-body (c result-var)))

      (define (gen-if cont-label)
	(conv cond env
	      (lambda (z)
		(let ((app-c (lambda (z)
			       (cps-app cont-label
					(list z)))))
		  (cps-gen-if-true z
				   (conv then env app-c)
				   (conv else env app-c))))))

      (if (is-tail-call? cont-body result-var)
	  (gen-if (tail-call-cont cont-body))
	  (cps-fun (cps-func cont-label
			     (list result-var)
			     cont-body)
		   (gen-if cont-label)))))

  ;; Apply C to a cps-value representing the result of evaluating EXP.
  ;; C should return a cps-instruction that consumes the cps-value.
  ;;
  (define (conv exp env c)
    (pattern-case exp
		  
       ((:quote ?val)
	(c (cps-quote ?val)))

       ((:lambda ?args . ?body)
	(let ((func (genvar)))
	  (cps-fun (conv-func func ?args ?body env)
		   (c func))))

       ((:begin . ?body)
	(conv* ?body
	       env
	       (lambda (zs)
		 (if (null? zs)
		     (c (cps-quote (if #f #f)))
		     (c (car (last-pair zs)))))))

       ((:if ?cond ?then ?else)
	(conv-if ?cond ?then ?else env c))

       ((:primitive ?op . ?args)
	(let* ((result (genvar)))
	  (conv* ?args
		 env
		 (lambda (zs)
		   (cps-primop ?op
			       (list result)
			       zs
			       (list (c result)))))))
       
       ((:set ?var ?value)
	(conv ?value
	      env
	      (lambda (z)
		(let ((box-var (lookup-sym ?var env))
		      (cont (c (cps-quote (if #f #f)))))
		  (if (bound-variable? box-var)
		      (cps-gen-box-set box-var
				       z
				       cont)
		      (cps-gen-global-set (cps-quote ?var)
					  z
					  cont))))))
       
       ((?func . ?args)
	(let ((macro (and (symbol? ?func)
			  (lookup-macro ?func env))))
	  (if macro
	      (conv (expand-macro macro exp) env c)
	      (conv-apply ?func ?args env c))))
       
       (else
	(if (symbol? exp)
	    (let* ((box-var (lookup-sym exp env))
		   (result-var (genvar))
		   (cont (c result-var)))
	      (if (bound-variable? box-var)
		  (cps-gen-box-ref result-var
				   box-var
				   cont)
		  (cps-gen-variable-ref result-var
					(cps-quote exp)
					cont)))
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

  (let ((cps (conv exp (make-env) (lambda (z) #f))))
    (if (and (cps-fun? cps) (not (cps-fun-cont cps)))
	(cps-fun-func cps)
	(error "can only compile functions"))))

;;; Basic Scheme syntax

(define-suo-macro (quote val)
  `(:quote ,val))

(define-suo-macro (define name . rest)
  (if (symbol? name)
      `(:primitive global-define ',name ,(car rest))
      `(define ,(car name) (lambda ,(cdr name) ,@rest))))

(define-suo-macro (lambda args . body)
  `(:lambda ,args ,@body))

(define-suo-macro (let bindings . body)
  (let ((vars (map car bindings))
	(inits (map cadr bindings)))
    `((lambda ,vars ,@body) ,@inits)))

(define-suo-macro (if cond then . else)
  (if (null? else)
      `(:if ,cond ,then (begin))
      `(:if ,cond ,then ,(car else))))

(define-suo-macro (begin . body)
  `(:begin ,@body))

(define-suo-macro (set! var val)
  `(:set ,var ,val))

(define-suo-macro (letrec bindings . body)
  (let ((vars (map car bindings))
	(inits (map cadr bindings))
	(unspec (if #f #f)))
    (pk
     `(let ,(map (lambda (v) `(,v ',unspec)) vars)
	,@(map (lambda (v i)
		 `(set! ,v ,i))
	       vars inits)
	(let () ,@body)))))

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

;;; Used, bound, and free variables

(define (union* sets)
  (reduce union sets))

(define (map-union func list)
  (union* (map func list)))

(define (cps-used-vars cps)
  (struct-case cps

    ((cps-var)
     (list cps))

    ((cps-quote val)
     '())

    ((cps-app func args)
     (union (cps-used-vars func)
	    (map-union cps-used-vars args)))

    ((cps-func label args body)
     (cps-used-vars body))

    ((cps-fun func cont)
     (union (cps-used-vars cont)
	    (cps-used-vars func)))

    ((cps-primop type results args conts)
     (union (map-union cps-used-vars args)
	    (map-union cps-used-vars conts)))))

(define (cps-bound-vars cps)
  (struct-case cps

    ((cps-var)
     '())

    ((cps-quote val)
     '())

    ((cps-app func args)
     '())

    ((cps-func label args body)
     (union (list label)
	    (union args
		   (cps-bound-vars body))))

    ((cps-fun func cont)
     (union (cps-bound-vars cont)
	    (cps-bound-vars func)))

    ((cps-primop type results args conts)
     (union results
	    (map-union cps-bound-vars conts)))))

(define (cps-free-vars cps)
  (set-difference (cps-used-vars cps) (cps-bound-vars cps)))

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

;;; Closure conversion of FUNs

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
				  (list (cps-quote suo:closure-descriptor)
					code vector-var)
				  (list cont))))))

;; Get the code object out of the closure record CLOSURE and continue
;; with CONT.  No type checking.
;;
(define (cps-gen-closure-ref-code result closure cont)
  (cps-primop 'record-ref
	      (list result)
	      (list closure (cps-quote 0))
	      (list cont)))

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

(define-dynamic-attr cps-replacement)

(define (cps-closure-convert cps)
  (struct-case cps
	       
    ((cps-var)
     (or (cps-replacement cps) cps))

    ((cps-quote)
     cps)

    ((cps-app func args)
     (let ((closure (cps-closure-convert func))
	   (code-var (genvar)))
       (cps-gen-closure-ref-code code-var
				 closure
				 (cps-app code-var
					  (cons closure 
						(map cps-closure-convert
						     args))))))

    ((cps-func label args body)
     (cps-func label
	       (map cps-closure-convert args)
	       (cps-closure-convert body)))

    ((cps-fun func cont)
     (let* ((func-args (cps-func-args func))
	    (cont-arg (car func-args))
	    (closure-arg (genvar))
	    (closure-var (genvar))
	    (free-vars (cps-free-vars func))
	    (closure-vars (map (lambda (v) (genvar)) free-vars))
	    (func-name (cps-func-name func)))
       (pk-cps 'free (cps-func-name func) free-vars)
       (cps-fun (cps-func func-name
			  (cons closure-arg func-args)
			  (cps-gen-open-closure closure-vars
						closure-arg
						(with-attr*
						 (cps-replacement free-vars
								  closure-vars)
						 (cps-closure-convert
						  (cps-func-body func)))))
		(cps-gen-make-closure closure-var
				      func-name
				      free-vars
				      (with-attr (cps-replacement func-name
								  closure-var)
				        (cps-closure-convert cont))))))

    ((cps-primop type results args conts)
     (cps-primop type 
		 (map cps-closure-convert results)
		 (map cps-closure-convert args)
		 (map cps-closure-convert conts)))))

;;; Register allocation and calling convention implementation

;; Callig convention: all arguments in registers starting from
;; register 0.

(define-param cps-next-register)

(define (cps-register-allocate cps)

  (define (allocate-regs vars cont)
    (let loop ((idx (cps-next-register))
	       (regs '())
	       (rest vars))
      (cond ((null? rest)
	     (with-param (cps-next-register idx)
	       (with-attr* (cps-replacement vars (reverse! regs))
		 (cont))))
	    (else
	     (loop (1+ idx)
		   (cons (cps-reg idx) regs)
		   (cdr rest))))))

  (struct-case cps
   
    ((cps-var)
     (or (cps-replacement cps) cps))
    
    ((cps-quote)
     cps)
    
    ((cps-app func args)
     (cps-app (cps-register-allocate func)
	      (map cps-register-allocate args)))
    
    ((cps-func label args body)
     (with-param (cps-next-register 0)
       (allocate-regs args
		      (lambda ()
			(cps-func label
				  (map cps-register-allocate args)
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
  
  (struct-case cps

    ((cps-var)
     ;; refers to a function and its replacement is (cps-quote code)
     (cps-replacement cps))

    ((cps-reg)
     cps)

    ((cps-quote)
     cps)

    ((cps-app func args)
     (cps-asm-shuffle (cps-asm-context)
		      (map cps-code-generate args)
		      (map cps-reg (iota (length args))))
     (cps-asm-go (cps-asm-context) (cps-code-generate func)))

    ((cps-func label args body)
     (let ((ctxt (cps-asm-make-context)))
       (with-param (cps-asm-context ctxt)
         (cps-code-generate body))
       (cps-asm-finish ctxt)))

    ((cps-fun func cont)
     (let ((code (cps-code-generate func)))
       (with-attr (cps-replacement (cps-func-name func) (cps-quote code))
         (cps-code-generate cont))))

    ((cps-primop type results args conts)
     (let ((ctxt (cps-asm-context))
	   (results (map cps-code-generate results))
	   (args (map cps-code-generate args)))
       (case type
	 ((record)
	  (apply cps-asm-record ctxt (car results) args)
	  (cps-code-generate (car conts)))
	 ((record-ref)
	  (cps-asm-record-ref ctxt (car results) (car args) (cadr args))
	  (cps-code-generate (car conts)))
	 ((record-set)
	  (cps-asm-record-set ctxt (car args) (cadr args) (caddr args))
	  (cps-code-generate (car conts)))
	 ((vector)
	  (apply cps-asm-vector ctxt (car results) args)
	  (cps-code-generate (car conts)))
	 ((vector-ref)
	  (cps-asm-vector-ref ctxt (car results) (car args) (cadr args))
	  (cps-code-generate (car conts)))
	 ((syscall)
	  (cps-asm-syscall ctxt (car results) args)
	  (cps-code-generate (car conts)))
	 ((if-eq)
	  (let ((else-label (cps-asm-make-label ctxt)))
	    (cps-asm-if-not-eq ctxt (car args) (cadr args) else-label)
	    (cps-code-generate (car conts))
	    (cps-asm-def-label ctxt else-label)
	    (cps-code-generate (cadr conts))))
	 (else
	  (error "unsupported primop" type)))))))

;;; Assembling, machine dependent

(define (cps-asm-make-context)
  (let ((bytes (make-u8vector 1024 0))
	(idx 0)
	(fixups '())
	(literals '())
	(labels '()))

    (lambda (op . args)

      (define (emit-byte b)
	(u8vector-set! bytes idx b)
	(set! idx (1+ idx)))

      (define (emit-bytes . bs)
	(pk 'emit bs)
	(for-each emit-byte bs))

      (define (emit-word w)
	(u8vector-set-u32! bytes idx w)
	(set! idx (+ idx 4)))
    
      (define (fixup-word idx delta)
	(u8vector-set-u32! bytes idx (+ (u8vector-ref-u32 bytes idx) delta)))

      (define (fixup! proc)
	(set! fixups (cons proc fixups)))
      
      (define (register-literal obj)
	(let ((pos (list-index literals obj)))
	  (or pos
	      (begin
		(set! literals (append literals (list obj)))
		(1- (length literals))))))

      (define (regoff op)
	(* 4 (cps-reg-idx op)))

      (define (litoff op)
	(* 4 (register-literal (cps-quote-value op))))

      (define (emit-litoff op)
	(let ((pos idx))
	  (emit-word (litoff op))
	  (fixup! (lambda ()
		    (fixup-word pos (+ 4 idx))))))

      (define (finish)
	(set! idx (* 4 (quotient (+ idx 3) 4)))
	(for-each (lambda (f) (f)) fixups)
	(suo:code (pkx (u8vector->u32vector bytes idx))
		  (list->vector literals)))

      (case op
	((emit-bytes)
	 (apply emit-bytes args))
	((emit-word)
	 (emit-word (car args)))
	((emit-literal-offset)
	 (emit-litoff (car args)))
	((make-label)
	 (make-label))
	((def-label)
	 (def-label (car args)))
	((finish)
	 (finish))
	(else
	 (error "unsupported op" op))))))

(define (cps-asm-bytes ctxt . bytes)
  (apply ctxt 'emit-bytes bytes))

(define (cps-asm-word ctxt word)
  (ctxt 'emit-word word))

(define (cps-asm-literal-offset ctxt literal)
  (ctxt 'emit-literal-offset literal))

(define (cps-asm-make-label ctxt)
  (ctxt 'make-label))

(define (cps-asm-def-label ctxt label)
  (ctxt 'def-label label))

(define (cps-asm-finish ctxt)
  (ctxt 'finish))


(define (cps-asm-regoff-byte reg)
  (let ((off (* 4 (cps-reg-idx reg))))
    (cond
     ((<= -128 off -1)
      (+ off 256))
     ((<= 0 off 127)
      off)
     (else
      (error "register out of bounds" reg)))))
  
(define (cps-asm-op-to-eax ctxt op)
  (cond ((cps-reg? op)
	 ;; mov off(%ebp),%eax
	 (cps-asm-bytes ctxt #x8b #x45 (cps-asm-regoff-byte op)))
	(else
	 ;; mov off(%esi),%eax
	 (cps-asm-bytes ctxt #x8b #x86)
	 (cps-asm-literal-offset ctxt op))))

(define (cps-asm-word-to-eax ctxt word)
  ;; mov $num,%eax
  (cps-asm-bytes ctxt #xb8)
  (cps-asm-word ctxt word))
  
(define (cps-asm-eax-to-reg ctxt op)
  (or (cps-reg? op)
      (error "must be reg" op))
  ;; mov %eax,off(%ebp)
  (cps-asm-bytes ctxt #x89 #x45 (cps-asm-regoff-byte op)))

(define (cps-asm-swap-eax-and-reg ctxt reg)
   ;; mov %eax,%ebx
   (cps-asm-bytes ctxt #x89 #xc3)
   ;; mov reg,%eax
   (cps-asm-op-to-eax ctxt reg)
   ;; mov %ebx,off(%ebp)
   (cps-asm-bytes ctxt #x89 #x5d (cps-asm-regoff-byte reg)))

(define (reg-index lst reg)
  (let loop ((i 0)
	     (l lst))
    (cond ((null? l)
	   #f)
	  ((and (cps-reg? (car l))
		(= (cps-reg-idx (car l)) (cps-reg-idx reg)))
	   i)
	  (else
	   (loop (1+ i) (cdr l))))))

(define (cps-asm-shuffle ctxt from to)
  (pk-cps 'shuffle from to)
  (let ((done '()))
    (define (do-one idx)
      ;; from[idx] is in eax.  Swap to[idx] and eax.
      (pk-cps '-> (list-ref to idx))
      (set! done (cons idx done))
      (cps-asm-swap-eax-and-reg ctxt (list-ref to idx)))
    (define (do-cycle idx)
      (pk-cps (list-ref from idx) '->)
      (cps-asm-op-to-eax ctxt (list-ref from idx))
      (let loop ((idx idx))
	(do-one idx)
	(let ((next (reg-index from (list-ref to idx))))
	  (if (and next (not (memq next done)))
	      (loop next)))))
    (do ((idx 0 (1+ idx)))
	((= idx (length from)))
      (if (not (memq idx done))
	  (do-cycle idx)))))

(define (cps-asm-go ctxt to)
  (pk-cps 'go to)
  (cps-asm-op-to-eax ctxt to)
  ;; mov %eax,%esi
  (cps-asm-bytes ctxt #x89 #xc6)
  ;; addl 4,%eax
  (cps-asm-bytes ctxt #x83 #xc0 #x04)
  ;; jmp *%eax
  (cps-asm-bytes ctxt #xff #xe0))

(define (cps-asm-syscall ctxt res args)
  (for-each (lambda (a)
	      (cps-asm-op-to-eax ctxt a)
	      ;; push %eax
	      (cps-asm-bytes ctxt #x50))
	    (reverse args))
  (cps-asm-word-to-eax ctxt (length args))
  ;; push %eax
  (cps-asm-bytes ctxt #x50)
  (cps-asm-op-to-eax ctxt (cps-reg -1))
  ;; call *eax
  (cps-asm-bytes ctxt #xff #xd0)
  ;; add off,%esp
  (cps-asm-bytes ctxt #x83 #xc4 (* 4 (1+ (length args))))
  (cps-asm-eax-to-reg ctxt res))

(define (cps-asm-alloc-to-ebx ctxt words)
  ;; mov %edi,%ebx
  (cps-asm-bytes ctxt #x89 #xfb)
  ;; lea off(%edi),%edi
  (cps-asm-bytes ctxt #x8d #xbf)
  (cps-asm-word ctxt (* 4 words)))

(define (cps-asm-store-eax-to-ebx ctxt)
  ;; mov %eax,(%ebx)
  (cps-asm-bytes ctxt #x89 #x03))

(define (cps-asm-store-eax-to-ebx-and-inc ctxt)
  (cps-asm-store-eax-to-ebx ctxt)
  ;; lea 4(%ebx),%ebx
  (cps-asm-bytes ctxt #x8d #x5b #x04))

(define (cps-asm-ref-eax-to-eax ctxt idx)
  ;; mov off(%eax),%eax
  (cps-asm-bytes ctxt #x8b #x80)
  (cps-asm-word ctxt (* 4 idx)))

(define (cps-asm-record ctxt res desc . values)
  (pk-cps 'record desc values)
  (cps-asm-alloc-to-ebx ctxt (1+ (length values)))
  (cps-asm-op-to-eax ctxt desc)
  ;; or 3,%eax
  (cps-asm-bytes ctxt #x83 #xc8 #x03)
  (cps-asm-store-eax-to-ebx-and-inc ctxt)
  (for-each (lambda (v)
	      (cps-asm-op-to-eax ctxt v)
	      (cps-asm-store-eax-to-ebx-and-inc ctxt))
	    values)
  ;; lea -off(%ebx),%eax
  (cps-asm-bytes ctxt #x8d #x83)
  (cps-asm-word ctxt (+ (- (* 4 (1+ (length values)))) #x100000000))
  (cps-asm-eax-to-reg ctxt res))

(define (cps-asm-record-ref ctxt res rec idx)
  (pk-cps 'record-ref rec idx)
  (cond ((cps-quote? idx)
	 (cps-asm-op-to-eax ctxt rec)
	 (cps-asm-ref-eax-to-eax ctxt (1+ (cps-quote-value idx)))
	 (cps-asm-eax-to-reg ctxt res))
	(else
	 (error "no computed indices yet, sorry"))))

(define (cps-asm-record-set ctxt rec idx val)
  (pk-cps 'record-set rec idx val)
  (cond ((cps-quote? idx)
	 (cps-asm-op-to-eax ctxt rec)
	 ;; lea off(%eax),%ebx
	 (cps-asm-bytes ctxt #x8d #x98)
	 (cps-asm-word ctxt (* 4 (1+ (cps-quote-value idx))))
	 (cps-asm-op-to-eax ctxt val)
	 (cps-asm-store-eax-to-ebx ctxt))
	(else
	 (error "no computed indices yet, sorry"))))

(define (cps-asm-vector ctxt res . values)
  (pk-cps 'vector values)
  (cps-asm-alloc-to-ebx ctxt (1+ (length values)))
  (cps-asm-word-to-eax ctxt (+ #x80000000 (* 16 (length values)) 3))
  (cps-asm-store-eax-to-ebx-and-inc ctxt)
  (for-each (lambda (v)
	      (cps-asm-op-to-eax ctxt v)
	      (cps-asm-store-eax-to-ebx-and-inc ctxt))
	    values)
  ;; lea -off(%ebx),%eax
  (cps-asm-bytes ctxt #x8d #x83)
  (cps-asm-word ctxt (+ (- (* 4 (1+ (length values)))) #x100000000))
  (cps-asm-eax-to-reg ctxt res))

(define (cps-asm-vector-ref ctxt res vec idx)
  (pk-cps 'vector-ref vec idx)
  (cond ((cps-quote? idx)
	 (cps-asm-op-to-eax ctxt vec)
	 (cps-asm-ref-eax-to-eax ctxt (1+ (cps-quote-value idx)))
	 (cps-asm-eax-to-reg ctxt res))
	(else
	 (error "no computed indices yet, sorry"))))

(define (cps-asm-if-not-eq ctxt v1 v2 lab)
  (error "not yet, sorry"))
