;;;; suo-cross -- a part of Suo in plain Scheme, and support for
;;;;              writing images

;;; Overview of the bootstrap process

;; Bootstrapping Suo happens roughly like this:
;;
;; - An empty environment is created in the hosting Scheme system.
;;   This is called the 'boot environment'.
;;
;; - A carefully selected set of bindings is created in the boot
;;   environnment to prime it.  Essentially, the boot environment can
;;   then support evaluating Suo source code.
;;
;; - The Suo source code files that contain the compiler are evaluated
;;   in the boot environment.  This fills the boot environment to a
;;   point where the host Scheme can execute the Suo compiler.
;;   Redefinitions of bindings that already exist in the boot
;;   environment are ignored.  That way, we can load the same source
;;   files that are later compiled to form the Suo image.  For
;;   example, the boot environment will contain definitions for
;;   'cons', 'car', 'cdr', and 'pair?' that use pair objects of the
;;   host Scheme.  When loading the Suo compiler into the boot
;;   environment, the loaded files can be the ones that define Suo
;;   completely, including its own primitives for pairs, but they are
;;   ignored and the Suo compiler in the boot environment will use
;;   host pairs instead.
;;
;; - Another empty environment is created, the 'image environment'.
;;
;; - A few bindings are introduced into this environment for some
;;   objects that the compiler (or the image writer) uses in generated
;;   code and that need to be available to Suo as well.  Examples are
;;   the objects that represent types, such as the object that
;;   represents the type of symbols.
;;
;; - The compiler is used to compile a set of Suo source files.  These
;;   source files define the initial Suo image.  They contain the
;;   compiler, of course, and a read-eval-print loop.
;;
;; - The result of the compilations of the last step is a procedure
;;   that when incoked carries out the the toplevel actions of the
;;   compiled files.  That procedure is written to a file using the
;;   Suo binary layout of objects.
;;
;; - The Suo runtime then loads the image file created in the last
;;   step and starts the boot procedure, which eventually launches
;;   into the repl.
;;
;; - From there on, the image is changed using the repl and the Suo
;;   runtime can suspend and restart sessions.

;;; The boot environment

;; The boot environment is implemented as a Guile module.  This allows
;; us to simply 'eval' code in it.  The module will be initially empty
;; and we need to introduce every needed binding into it, using
;; 'register-strong-boot-binding' or its macro variant 'boot-import'.
;;
;; Attempts to re-binding bindings done with
;; 'register-strong-boot-binding' will be ignored when evaluating code
;; in the boot environment.  Other re-bindings will result in errors.

(define boot-module (make-module 1031))

(define strong-boot-bindings '())

(define (register-strong-boot-binding sym var)
  (module-add! boot-module sym var)
  (set! strong-boot-bindings (cons sym strong-boot-bindings)))

(define (import-boot-binding sym)
  (let ((host-var (or (module-variable (current-module) 
				       (symbol-append 'suo: sym))
		      (module-variable (current-module) sym))))
    (register-strong-boot-binding sym host-var)))

(define-macro (boot-import . syms)
  `(begin
     ,@(map (lambda (sym)
	      `(import-boot-binding ',sym))
	    syms)))

;; Macros can use 'bootstrap-phase' to control their expansion.  See
;; 'define-record' for an example.
;;
;; The bootstrap phase can be on of 'load-for-boot' (when the macro is
;; expanded to fill the boot environment), 'compile-for-image' (when
;; the macro is expanded to produce code that is compiled into the
;; image environment), or 'running' (when the macro is expanded in a
;; running image).

(define current-bootstrap-phase 'load-for-boot)

(define (suo:bootstrap-phase)
  current-bootstrap-phase)

(boot-import bootstrap-phase)

;; We need a few basic bindings to get going.

(boot-import define lambda if begin
	     set! quote quasiquote
	     let let* letrec
	     apply)

;; Things would be fairly simple for toplevel variable definitions
;; except that we can't redefine toplevel 'define's without also
;; redefining internal 'defines'.  Because of this, we have a custom
;; lookup closure of the boot-module, which is a pretty Guile specific
;; thing...

(define ignored-variable (make-variable #f))

(define (boot-module-eval-closure sym define?)
  ;;(pk 'eval-closure sym define?)
  (if define?
      (if (memq sym strong-boot-bindings)
	  (begin
	    (pk 'ignore sym)
	    ignored-variable)
	  (begin
	    (pk 'boot-define sym)
	    (module-make-local-var! boot-module sym)))
      (begin
	(module-local-variable boot-module sym))))

(set-module-eval-closure! boot-module boot-module-eval-closure)

(define-macro (suo:define-function name . exprs)
  `(define ,name ,(car exprs)))

(define-macro (suo:define-record-type name  expr)
  `(define ,name ,expr))

(boot-import define-function define-record-type)

;; We remember macros more explicitely.  The compiler uses the macros
;; in the boot environment when expanding code that is compiled for
;; the image.
;;
;; A 'transformer' is a function that will return the expansion of a
;; form when applied to the argument expressions in that form (i.e.,
;; when applied to the cdr of the form).

(define boot-macro-transformers '())

(define (register-boot-macro sym transformer)
  (set! boot-macro-transformers
	(acons sym transformer boot-macro-transformers))
  (boot-eval `(define ,sym ',(defmacro:transformer transformer))))

(define-macro (suo:define-macro head . rest)
  (if (pair? head)
      `(define-macro ,(car head) (lambda ,(cdr head) ,@rest))
      `(register-boot-macro ',head ,(car rest))))

(boot-import register-boot-macro define-macro)

;; Loading books

;; Suo books have an external representation that is geared towards
;; editing it directly in a normal text editor.  We load these books
;; by ignoring the sections structure in them completely and just
;; evaluating the expressions in them.
;;
;; As promised, we can simply use 'eval' and 'load' to evaluate forms
;; in the boot environment.

(define (suofy-symbols form)
  (cond ((symbol? form)
	 (suo:intern-symbol
	  (suo:create-symbol 0 (list (symbol->string form)))))
	((pair? form)
	 (cons (suofy-symbols (car form))
	       (suofy-symbols (cdr form))))
	(else
	 form)))
	
(define (read-forms-from-string str)
  (with-input-from-string str
    (lambda ()
      (let loop ((res '()))
	(let ((f (read)))
	  (cond ((eof-object? f)
		 (reverse res))
		(else
		 (loop (cons f res)))))))))

(define (eval-string str eval)
  (eval (cons 'begin (read-forms-from-string str))))

(define (load-book filename eval)
  (with-input-from-file filename
    (lambda ()

      (define (read-chunk delim)
	(let loop ((res ""))
	  (let ((line (read-line)))
	    (cond ((eof-object? line)
		   (if (zero? (string-length res))
		       line
		       res))
		  ((string-prefix? delim line)
		   res)
		  (else
		   (loop (string-append res "\n" line)))))))

      (let loop ()
	(if (not (eof-object? (read-chunk "-")))
	    (let ((exp (read-chunk "@")))
	      (if (not (eof-object? exp))
		  (begin
		    (eval-string exp eval)
		    (loop)))))))))

(define (boot-eval form)
  (eval form boot-module))

(define (boot-load-book filename)
  (pk 'boot-load-book filename)
  (load-book filename boot-eval))

;;; Representation of Suo values in the boot environment.

;; The Suo compiler is written for Suo; it uses Suo specific data
;; types such as bytevecs and the Suo variant of records, for example.
;; When executing it in the boot environment, we need to provide the
;; necessary functions for dealing with these data types.  Thus, we
;; implement all of the basic Suo data types in plain Scheme here, and
;; then pre-define the corresponding functions in the boot
;; environment.
;;
;; Singletons like '#f' and '()', numbers, characters, pairs, vectors,
;; keywords and strings are implemented directly by using the host
;; Scheme data tyes.  The compiler uses nothing Suo specific about
;; them.
;;
;; Suo symbols are richer than normal Scheme symbols: they have a list
;; of strings as their components instead of just a single string as
;; the name.  But symbols need to be available from the start for
;; bootstrapping reasons and thus we have to implement them here as
;; well.

;; (define suo-symbol-type
;;   (make-record-type 'symbol '(offset components)))

;; (define suo:symbol? (record-predicate suo-symbol-type))
;; (define suo:create-symbol (record-constructor suo-symbol-type))
;; (define suo:symbol-offset (record-accessor suo-symbol-type 'offset))
;; (define suo:symbol-components (record-accessor suo-symbol-type 'components))

;; (define boot-symbols (make-hash-table 511))

(define symbol-extra-info (make-hash-table 511))

(define (make-symbol-extra-info sym)
  (let ((str (symbol->string sym)))
    (cond ((equal? str ".")
	   (cons 0 '()))
	  ((equal? str "/")
	   (cons #t '()))
	  (else
	   (let ((comps (string-split (symbol->string sym) #\/)))
	     (if (equal? "" (car comps))
		 (cons #t (cdr comps))
		 (cons 0 comps)))))))

(define (get-symbol-extra-info sym)
  (or (hashq-ref symbol-extra-info sym)
      (let ((info (make-symbol-extra-info sym)))
	(hashq-set! symbol-extra-info sym info)
	info)))

(define (suo:symbol-offset sym)
  (car (get-symbol-extra-info sym)))

(define (suo:symbol-components sym)
  (cdr (get-symbol-extra-info sym)))

(define (string-concat del . strings)
  (cond ((null? strings)
	 "")
	((null? (cdr strings))
	 (car strings))
	(else
	 (apply string-concat del
		(string-append (car strings) del (cadr strings))
		(cddr strings)))))

(define (suo:symbol offset components)
  (cond ((and (equal? offset 0)
	      (null? components))
	 '.)
	(else
	 (or (eq? #t offset)
	     (zero? offset)
	     (error "symbol offsets not supported, sorry:" offset components))
	 (let ((name (apply string-concat "/" components)))
	   (string->symbol (if (eq? offset #t)
			       (string-append "/" name)
			       name))))))

(boot-import eq?

	     + - * < = > <= >= quotient remainder 1- 1+
	     number->string zero?

	     pair? cons car cdr set-car! set-cdr!

	     char? integer->char char->integer

	     vector? vector-length make-vector vector-ref vector-set!
	     vector->list

	     symbol? symbol symbol-offset symbol-components

	     keyword? symbol->keyword keyword->symbol

	     string? make-string string-length string-ref string-set!
	     string-append)

;; Records are implemented using, errm, records, but with records of a
;; different kind.  The fields of the Suo record are simply stored as
;; a Scheme vector.

(define suo-record-record-type
  (make-record-type 'record-record '(type fields)
		    (lambda (obj port) (display "#<suo-record>" port))))

(define suo:record? (record-predicate suo-record-record-type))

(define (suo:record-length rec)
  (suo:record-ref (suo:type-of-record rec) 0))

(define suo:record
  (let ((constructor (record-constructor suo-record-record-type)))
    (lambda (type . fields)
      (constructor type (apply vector fields)))))

(define suo:make-record
  (let ((constructor (record-constructor suo-record-record-type)))
    (lambda (type init)
      (constructor type (make-vector (suo:record-ref type 0) init)))))

(define suo:type-of-record (record-accessor suo-record-record-type 'type))

(define suo-record-fields (record-accessor suo-record-record-type 'fields))

(define (suo:record-ref rec idx)
  (vector-ref (suo-record-fields rec) idx))

(define (suo:record-set! rec idx val)
  (vector-set! (suo-record-fields rec) idx val))

(define suo-record-set-type! (record-modifier suo-record-record-type 'type))

(define suo:record-type-type #f)

(define (suo:record-with-type? obj type)
  (and (suo:record? obj)
       (eq? (suo:type-of-record obj) type)))

(define (suo:record-is-a? obj type)
  (and (suo:record? obj)
       (let ((ancestry (suo:record-ref (suo:type-of-record obj) 2))
	     (pos (1- (vector-length (suo:record-ref type 2)))))
	 (and (< pos (vector-length ancestry))
	      (eq? type (vector-ref ancestry pos))))))

(define (suo:record-type-accessor type slot-name)
  (let ((index (list-index (suo:record-ref type 3) slot-name)))
    (if index
	(lambda-with-setter ((obj)
			     (if (suo:record-is-a? obj type)
				 (suo:record-ref obj index)
				 (error "wrong type: " obj)))
			    ((obj val)
			     (if (suo:record-is-a? obj type)
				 (suo:record-set! obj index val)
				 (error "wrong type: " obj))))
	(error "no such slot: " slot-name))))

(define (suo:record-type-constructor type args)
  (lambda args (apply suo:record type args)))

(define boot-record-types '())

(define (suo:make-record-type name parent-type slots)
  (pk 'boot-record-type name slots)
  (if parent-type
      (error "inheritance not supported"))
  (let* ((ancestry (vector #f))
	 (type (suo:record suo:record-type-type 
			   (length slots)
			   name
			   ancestry
			   slots
			   '())))
    (vector-set! ancestry 0 type)
    (cond ((eq? name 'record-type)
	   (set! suo:record-type-type type)
	   (suo-record-set-type! type type)))
    type))

(boot-import record? record-with-type?
	     type-of-record record-length record-ref record-set!
	     record make-record make-record-type
	     record-type-accessor record-type-constructor)

;; Code objects are implemented as records with explicit bytevec and
;; vector for the instructions and literals, respectively.

(define suo-code-record-type
  (make-record-type 'code-record '(insns literals)
		    (lambda (obj port) (display "#<suo-code>" port))))

(define suo-code-insns (record-accessor suo-code-record-type 'insns))

(define suo-code-literals (record-accessor suo-code-record-type 'literals))

(define suo:code? (record-predicate suo-code-record-type))

(define suo:code (record-constructor suo-code-record-type))

(boot-import code? code)

;; Bytevecs are implemented with SRFI-4 u8vectors.

(define (suo:make-bytevec n)
  (make-u8vector n))

(define (suo:bytevec? obj)
  (u8vector? obj))

(define (suo:bytevec-length-8 v)
  (u8vector-length v))

(define (suo:bytevec-ref-u8 v i)
  (u8vector-ref v i))

(define (suo:bytevec-set-u8! v i val)
  (u8vector-set! v i val))

(define (suo:bytevec-length-16 v)
  (quotient (u8vector-length v) 2))

;;; Little endian

(define (suo:bytevec-ref-u16 v i)
  (+ (* (u8vector-ref v (1+ (* 2 i))) #x100)
     (u8vector-ref v (* 2 i))))

(define (suo:bytevec-set-u16! v i val)
  (u8vector-set! v (1+ (* 2 i)) (quotient val #x100))
  (u8vector-set! v (* 2 i) (remainder val #x100)))

(define (suo-bytevec-u8->list bv)
  (u8vector->list bv))

(define (suo-bytevec-u32->list bv)
  (do ((i 0 (1+ i))
       (res '() (cons (+ (* (suo:bytevec-ref-u16 bv (1+ (* 2 i))) #x10000)
			 (suo:bytevec-ref-u16 bv (* 2 i)))
		      res)))
      ((= i (quotient (suo:bytevec-length-16 bv) 2))
       (reverse! res))))

(boot-import make-bytevec bytevec?
	     bytevec-length-8 bytevec-ref-u8 bytevec-set-u8!
	     bytevec-length-16 bytevec-ref-u16 bytevec-set-u16!)

;; string-bytes is used for low-level debugging.  It returns a bytevec
;; with the characters of the given string.

(define (suo:string-bytes str)
  (list->u8vector (map char->integer (string->list str))))

(boot-import string-bytes)

;;; Miscellaneous definitions for the boot environment

;; When an error occurs without appropriate handlers, sys:panic will
;; be called eventually.

(define (suo:sys:panic)
  (error "panic"))

(boot-import sys:panic)

;; Suo uses 'parameters' instead of dynamic-wind.  We implement them
;; ourselves with Guile fluids here.

(define (suo:make-parameter val)
  (let ((f (make-fluid)))
    (fluid-set! f val)
    (lambda args
      (cond ((null? args)
	     (fluid-ref f))
	    ((null? (cdr args))
	     (fluid-set! f (car args)))
	    (else
	     f)))))

(define (suo:call-p parameter value func)
  (with-fluid* (parameter #f 'get-fluid) value func))

(boot-import make-parameter call-p)

;; We also import the printing functions into the boot environment so
;; that output is fastish..

(boot-import display write newline pretty-print)

;; Hash tables that use eq? as the equality check are implemented by
;; the Suo runtime since they need to interact closely with the GC.

(define suo:make-hashq-table make-hash-table)

(boot-import make-hashq-table hashq-ref hashq-set!)

;; Setters are stored on the side

(define setters (make-hash-table 31))

(define (suo:setter proc)
  (hashq-ref setters proc))

(define (suo:init-setter-setter proc)
  (hashq-set! setters proc
	      (lambda (setter proc)
		(hashq-set! setters proc setter))))

(define-macro (lambda-with-setter l s)
  `(let ((l (lambda ,@l))
	 (s (lambda ,@s)))
     (hashq-set! setters l s)
     l))

(boot-import setter init-setter-setter)

;; Multiple values

(define suo:call-v call-with-values)

(boot-import values call-v)

;;; The image environment

;; Toplevel macros are compiled and made available in the image, but
;; they are never used to expand forms during compilation.  Instead,
;; the compiler uses the macros in the boot environment to expand code
;; that it compiles for the image environment.
;;
;; All variables must be declared before they are first referenced.

(define boot-bindings (make-hash-table 513))

(define symbol-value
  (lambda-with-setter ((sym)
		       ;;(pk 'image-value sym)
		       (hashq-ref boot-bindings sym))
		      ((sym val)
		       ;;(pk 'image-binding sym val)
		       (hashq-set! boot-bindings sym val))))
		      
(define (lookup* sym in-open-dirs? allow-undefined?)
  (boot-eval `(lookup* ',sym ',in-open-dirs? ',allow-undefined?)))

(define (enter sym val)
  (boot-eval `(enter ',sym ',val)))

(define undeclared-variables '())

(define (image-lookup sym)
  (resolve-transmogrification (lookup* sym #t #t)))

(define (check-undefined-variables)
  (let ((variable@type (boot-eval 'variable@type))
	(closure@type (boot-eval 'closure@type)))
    (for-each (lambda (sym)
		(let ((val (image-lookup sym)))
		  (cond ((suo:record-with-type? val variable@type)
			 (if (eq? (begin) (suo:record-ref val 0))
			     (pk 'undeclared-variable sym)))
			((suo:record-with-type? val closure@type)
			 (if (not (suo:record-ref val 1))
			     (pk 'undeclared-function sym)))
			(else
			 (pk 'huh? sym)))))
	      undeclared-variables)))

;; (define (suo:variable-declare sym)
;;   (let ((variable@type (boot-eval 'variable@type)))
;;     (let ((old (lookup* sym #f #t)))
;;       (cond ((not old)
;; 	     (let ((var (suo:record variable@type (begin))))
;; 	       ;;(pk 'image-variable sym)
;; 	       (enter sym var)
;; 	       var))
;; 	    ((suo:record-with-type? old variable@type)
;; 	     old)
;; 	    (else
;; 	     (error "already declared, but not as variable: " sym))))))

(define (suo:function-declare sym)
  (let ((closure@type (boot-eval 'closure@type)))
    (let ((old (lookup* sym #f #t)))
      (cond ((not old)
	     (let ((func (suo:record closure@type #f #f #f #f)))
	       ;;(pk 'image-function sym)
	       (enter sym func)
	       func))
	    ((suo:record-with-type? old closure@type)
	     ;;(pk 'image-function-late sym)
	     old)
	    (else
	     (error "already declared, but not as function: " sym))))))

;; (define (suo:variable-lookup sym)
;;   (let ((variable@type (boot-eval 'variable@type)))
;;     (let ((val (lookup* sym #t #t)))
;;       (cond ((not val)
;; 	     ;;(pk 'image-undeclared sym)
;; 	     (set! undeclared-variables (cons sym undeclared-variables))
;; 	     (let ((var (suo:record variable@type (if #f #f))))
;; 	       (enter sym var)
;; 	       var))
;; 	    ((suo:record-with-type? val variable@type)
;; 	     val)
;; 	    (else
;; 	     (error "not a variable: " sym))))))

(define (suo:function-lookup sym)
  (let ((closure@type (boot-eval 'closure@type))
	(full-name (boot-eval `(symbol-concat (current-directory) ',sym))))
    (let ((val (lookup* sym #t #t)))
      (cond ((not val)
	     ;; (pk 'image-undeclared sym)
	     (set! undeclared-variables (cons full-name undeclared-variables))
	     (let ((func (suo:record closure@type #f #f #f #f)))
	       (enter sym func)
	       func))
	    ((suo:record-with-type? val closure@type)
	     (resolve-transmogrification val))
	    (else
	     (error "not a function: " sym))))))

(define (suo:macro-lookup sym)
  (assq-ref boot-macro-transformers sym))

(define (suo:macro-define sym val)
  (let* ((macro@type (boot-eval 'macro@type))
	 (mac (suo:record macro@type val)))
    ;; (pk 'image-macro sym)
    (boot-eval `(enter ',sym ',mac))))

(define-macro (suo:declare-variables . syms)
  ;; Nothing to do when loading into host Scheme
  #f)

(register-boot-macro
 'declare-variables
 (lambda syms
   (apply pk 'image-declare syms)
   (for-each (lambda (sym)
	       (boot-eval `(variable-declare ',sym)))
	     syms)
   '(begin)))

(boot-import symbol-value
	     ;; lookup* lookup
	     ;; variable-declare variable-lookup
	     function-declare
	     function-lookup
	     macro-lookup
	     ;; macro-define
	     ;; declare-variables
	     )

;; Some of the toplevel forms that are compiled are simple enough that
;; they can be carried out at compile time.  Other forms are collected
;; into one big 'toplevel expression' that is then compiled into a
;; procedure that will be called when the image is booted.

(define image-expressions '())

(define (add-image-expression exp)
  ;; (pk 'image-exp exp)
  (set! image-expressions (cons exp image-expressions)))

(define (add-image-variable-init var val)
  (add-image-expression `(record-set! ',var 0 ,val)))

(define (add-image-closure-transmogrify clos val)
  (add-image-expression `(transmogrify-objects (vector ',clos) (vector ,val))))

(define transmogrifications (make-hash-table 1023))

(define (resolve-transmogrification obj)
  (or (hashq-ref transmogrifications obj) obj))

(define (transmogrify-objects from to)
  (hashq-set! transmogrifications from (resolve-transmogrification to)))

;; Compilation happens by evaluating all forms in the image files with
;; a special variant of eval, called 'image-eval'.  'image-eval' only
;; really handles toplevel forms.

(define (constant? exp)
  (if (pair? exp)
      (eq? (car exp) :quote)
      (not (symbol? exp))))

(define (constant-value exp)
  (if (pair? exp)
      (cadr exp)
      exp))

(define (image-eval form)

  (define (compile form)
    (boot-eval `(/base/compile ',form)))

  (define (variable-lookup sym)
    (boot-eval `(variable-lookup ',sym)))

  (define (variable-declare sym)
    ((boot-eval 'variable-declare) sym))

  (define (function-declare sym)
    ((boot-eval 'function-declare) sym))

  (define (macro-define sym val)
    (boot-eval `(macro-define ',sym ',val)))

  (define (eval form)
    (let ((exp (compile form)))
      (if (pair? exp)
	  (case (car exp)
	    ((:begin)
	     (let loop ((forms (cdr exp)))
	       (cond ((null? forms)
		      (if #f #f))
		     ((null? (cdr forms))
		      (eval (car forms)))
		     (else
		      (eval (car forms))
		      (loop (cdr forms))))))
	    ((:set)
	     (let* ((name (cadr exp))
		    (val-exp (compile (caddr exp)))
		    (var (variable-lookup name)))
	       (if (constant? val-exp)
		   (suo:record-set! var 0 (constant-value val-exp))
		   (add-image-variable-init var val-exp))
	       (if #f #f)))
	    ((:define)
	     (let* ((name (cadr exp))
		    (var (variable-declare name))
		    (val-exp (compile (caddr exp))))
	       (if (constant? val-exp)
		   (suo:record-set! var 0 (constant-value val-exp))
		   (add-image-variable-init var val-exp))
	       (if #f #f)))
	    ((:define-function)
	     (let* ((name (cadr exp))
		    (full-name (boot-eval `(symbol-concat
					    (current-directory) ',name)))
		    (func (function-declare name))
		    (val-exp (compile (caddr exp))))
	       (set! undeclared-variables
		     (delq full-name undeclared-variables))
	       (if (constant? val-exp)
		   (transmogrify-objects func (constant-value val-exp))
		   (add-image-closure-transmogrify func val-exp))
	       (if #f #f)))
	    ((:define-macro)
	     (let* ((name (cadr exp))
		    (val-exp (compile (caddr exp))))
	       (if (constant? val-exp)
		   (macro-define name (constant-value val-exp))
		   (error "macro transformer not constant: " val-exp)))
	     (if #f #f))
	    ((:import-boot-record-type)
	     (let ((name (cadr exp)))
	       (pk 'imported-boot-record-type name)
	       (enter name (boot-eval name))))
	    (else
	     (add-image-expression exp)))
	  (error "useless form at toplevel: " exp))))

  (eval form))

;; Where there is 'eval', there is 'load'.

(define (image-load file)
  (pk 'image-load file)
  (set! current-bootstrap-phase 'compile-for-image)
  (with-input-from-file file
    (lambda ()
      (let loop ((form (read)))
	(cond ((not (eof-object? form))
	       (image-eval form)
	       (loop (read))))))))

(define (suo:read-text-line)
  (let ((l (read-line)))
    (if (eof-object? l)
	l
	(string-append l "\n"))))

(define suo:the-eof-object (with-input-from-string "" read))

(boot-import read-text-line eof-object? the-eof-object)

(define image-books '())

(define (image-eval-section sec)
  (let* ((desc (boot-eval `(section-description ',sec)))
	 (title-len (or (string-index desc #\newline)
			(string-length desc))))
    (pk 'compiling (substring desc 0 title-len)))
  (image-eval (cons 'begin
		   (boot-eval `(read-forms-from-string
				(section-expressions ',sec)))))
  (boot-eval `(let ((sec ',sec))
		(set! (section-id sec) #f)
		(set! (section-committed sec) (section-proposed sec))
		(set! (section-proposed sec) #f)))
  (for-each image-eval-section (boot-eval `(section-children ',sec))))

(define suo:call-cc call/cc)

(define image-boot-procs '())

(define (suo:eval form)
  (set! image-expressions '())
  (image-eval form)
  (if (not (null? image-expressions))
      (let ((comp (boot-eval
		   `(/base/compile
		     '(:lambda () ,@(reverse image-expressions))))))
	(or (constant? comp)
	    (error "expected constant"))
	(set! image-boot-procs (cons (constant-value comp)
				     image-boot-procs)))))

(define (image-expression)
  `((:lambda (loop) (loop loop ',(reverse image-boot-procs)))
    (:lambda (loop procs)
      (:primitive if-eq? () (procs '())
		  ((:begin)
		   (:primitive car (proc) (procs)
			       ((:begin
				 (proc)
				 (:primitive cdr (rest) (procs)
					     ((loop loop rest)))))))))))

(boot-import eval call-cc)

(define (image-load-book file)
  (pk 'image-load file)
  (set! current-bootstrap-phase 'compile-for-image)
  (with-input-from-file file
    (lambda ()
      (let ((sec (boot-eval '(read-section #t))))
	(boot-eval `(commit-section ',sec #t))
	(set! image-books (cons sec image-books))))))

(define (image-import-books)
  (let ((variable@type (boot-eval 'variable@type))
	(all-books-variable (boot-eval '(variable-lookup '/books/all-books))))
    (suo:record-set! all-books-variable 0 (reverse image-books))))

;; We copy some variable bindings from the boot environment over to
;; the image environment in order to ease bootstrapping further.  All
;; bindings that are copied are for record types that the compiler
;; uses.  It is easier to define these record types once for both the
;; boot and image environment.

(define (import-symbol-definition-from-boot sym)
  (let* ((old (symbol-value sym))
	 (name (boot-eval `(symbol-basename ',sym)))
	 (new (boot-eval (string->symbol name))))
    (pk 'importing sym old new)
    (if old
	(transmogrify-objects old new)
	(enter sym new))))

(define-macro (image-import-from-boot . syms)
  `(begin
     ,@(map (lambda (sym)
	      `(import-symbol-definition-from-boot ',sym))
	    syms)))

(define (image-import-boot-record-types)
  (for-each import-symbol-definition-from-boot boot-record-types))

;;; Dumping Suo objects into a u32vector

;; Once everything is compiled, we have a list of toplevel bindings
;; and the result of compiling the toplevel expressions into a 'boot
;; procedure'.  The next step is to write a binary representation of
;; all this to a file.  The binary representation is of course the one
;; that the Suo runtime uses, and the file can simply be loaded into
;; memory, be relocated, and then the boot procedure can be called.
;;
;; The compiler and this image writer collect some information that is
;; passed as the 'bootinfo' to the image.  This information includes
;; the list of all symbols that are contained in the image, the list
;; of all keywords, and the list of all toplevel bindings (variables
;; and macros).  This bootinfo can be accessed with the form
;; '(bootinfo)', which is recognized by the compiler.  The image
;; writer patches the image after all information is available.  The
;; compiler and the image writer communicate via the special and
;; unique object 'bootinfo-marker'.

(define bootinfo-marker (list 'bootinfo))
(boot-import bootinfo-marker)

;; We make 'bootinfo' available in the boot environment so that the
;; code loaded into it can use it.  But the lists will be empty.

(define (suo:bootinfo)
  (list '() '() '()))

(boot-import bootinfo)

(define (suo:fixnum? n)
  (and (integer? n)
       (<= -536870912 n 536870911)))

(boot-import fixnum?)

(define (integer->bignum x)

  (define (positive->bignum x)
    (define (->limbs x)
      (if (zero? x)
	  '(0)
	  (cons (remainder x #x10000)
		(->limbs (quotient x #x10000)))))
    (let* ((l (->limbs x))
	   (v (suo:make-bytevec (* 2 (length l)))))
      (do ((i 0 (+ i 1))
	   (l l (cdr l)))
	  ((null? l))
	(suo:bytevec-set-u16! v i (car l)))
      (boot-eval `(limbs->bignum ',v))))

  (if (> x 0)
      (positive->bignum x)
      (error "no negative bignums yet, sorry")))

(define (dump-object obj)
  (pk 'dump)

  (let ((suo:string@type (boot-eval 'string@type))
	(suo:symbol@type (boot-eval 'symbol@type))
	(suo:keyword@type (boot-eval 'keyword@type))

	(mem (make-u32vector (* 1024 1024)))
	(idx 0)
	(ptr-hash (make-hash-table (1- (* 1024 1024))))
	(all-symbols '())
	(all-keywords '())
	(bootinfo-idxs '()))

    (define (grow-mem)
      (let ((mem2 (make-u32vector (+ idx (* 1024 1024))))
	    (len (u32vector-length mem)))
	(do ((i 0 (1+ i)))
	    ((= i len))
	  (u32vector-set! mem2 i (u32vector-ref mem i)))
	(set! mem mem2)))

    (define (alloc obj n-words)
      (let ((ptr idx))
	(hashq-set! ptr-hash obj ptr)
	(set! idx (+ idx n-words))
	(if (> idx (u32vector-length mem))
	    (grow-mem))
	ptr))

    (define (emit-words ptr . words)
      (do ((i ptr (1+ i))
	   (w words (cdr w)))
	  ((null? w))
	(if (car w)
	    (u32vector-set! mem i (car w))
	    (set! bootinfo-idxs (cons i bootinfo-idxs)))))

    (define (bytes->big-endian-words bytes)
      (let loop ((bs bytes)
		 (ws '())
		 (w 0)
		 (i 0))
	(cond ((null? bs)
	       (reverse (if (zero? i)
			    ws
			    (cons (ash w (* (- 4 i) 8)) ws))))
	      ((= i 3)
	       (loop (cdr bs)
		     (cons (+ (ash w 8) (car bs)) ws)
		     0
		     0))
	      (else
	       (loop (cdr bs)
		     ws
		     (+ (ash w 8) (car bs))
		     (1+ i))))))

    (define (bytes->little-endian-words bytes)
      (let loop ((bs bytes)
		 (ws '())
		 (w 0)
		 (i 0))
	(cond ((null? bs)
	       (reverse (if (zero? i)
			    ws
			    (cons w ws))))
	      ((= i 3)
	       (loop (cdr bs)
		     (cons (+ w (ash (car bs) 24)) ws)
		     0
		     0))
	      (else
	       (loop (cdr bs)
		     ws
		     (+ w (ash (car bs) (* i 8)))
		     (1+ i))))))

    (define (emit obj . indirects)
      (let ((obj (resolve-transmogrification obj)))
	(cond
	 ((pair? obj)
	  (let ((ptr (alloc obj 2)))
	    (emit-words ptr (asm (car obj)) (asm (cdr obj)))))
	 ((suo:record? obj)
	  (let* ((type (suo:type-of-record obj))
		 (fields (vector->list (suo-record-fields obj)))
		 (ptr (alloc obj (1+ (length fields)))))
	    (if (not  (eqv? (length fields) (suo:record-ref type 0)))
		(error "inconsistent record:" 
		       (suo:record-ref type 1)
		       (length fields)
		       (suo:record-ref type 0)))
	    (for-each (lambda (i) (hashq-set! ptr-hash i ptr))
		      indirects)
	    (apply emit-words ptr
		   (+ (asm type) 3)
		   (map asm fields))))
	 ((vector? obj)
	  (let ((ptr (alloc obj (1+ (vector-length obj)))))
	    (apply emit-words ptr
		   (+ #x80000000 (* (vector-length obj) 16) 3)
		   (map asm (vector->list obj)))))
	 ((suo:bytevec? obj)
	  (let* ((len (suo:bytevec-length-8 obj))
		 (words (quotient (+ len 3) 4))
		 (ptr (alloc obj (1+ words))))
	    (apply emit-words ptr
		   (+ #x80000000 (* len 16) 11)
		   (bytes->little-endian-words (suo-bytevec-u8->list obj)))))
	 ((string? obj)
	  (let ((suo-str (suo:record suo:string@type
				     (apply u8vector
					    (map char->integer
						 (string->list obj))))))
	    (emit suo-str obj)))
	 ((symbol? obj)
	  (let ((suo-sym (suo:record suo:symbol@type 
				     (suo:symbol-offset obj)
				     (suo:symbol-components obj))))
	    (set! all-symbols (cons obj all-symbols))
	    (emit suo-sym obj)))
	 ((keyword? obj)
	  (let ((suo-keyword (suo:record suo:keyword@type
					 (keyword->symbol obj))))
	    (set! all-keywords (cons obj all-keywords))
	    (emit suo-keyword obj)))
	 ((suo:code? obj)
	  (let* ((insns (suo-code-insns obj))
		 (insn-words (quotient (suo:bytevec-length-8 insns) 4))
		 (literals (suo-code-literals obj))
		 (literal-words (vector-length literals))
		 (ptr (alloc obj (+ 1 insn-words literal-words))))
	    (apply emit-words ptr
		   (+ #x80000000
		      (* insn-words (* 256 16))
		      (* literal-words 16)
		      15)
		   (append (suo-bytevec-u32->list insns)
			   (map asm (vector->list literals))))))
	 ((integer? obj)
	  (pk 'bignum obj
	      (suo:fixnum? obj)
	      (= -536870912 obj))
	      
	  ;; bignum, fixnums are handled by asm below
	  (let ((suo-bignum (integer->bignum obj)))
	    (emit suo-bignum obj)))
	 (else
	  ;; There shouldn't be unsupported objects of course, but we
	  ;; help here with developing the bootstrap process itself by
	  ;; allowing them.  As long as the objects are not actually
	  ;; used...
	  (pk 'unsupported-value obj)
	  (let ((ptr (alloc obj 2)))
	    (emit-words ptr 26 26))))))

    (define (asm obj)
      (let ((obj (resolve-transmogrification obj)))
	(cond
	 ((eq? obj bootinfo-marker)
	  #f)
	 ((suo:fixnum? obj)
	  (+ (* 4 obj)
	     (if (< obj 0) #x100000000 0)
	     1))
	 ((char? obj)
	  (+ (* 8 (char->integer obj)) 6))
	 ((eq? obj '())
	  2)
	 ((eq? obj #t)
	  10)
	 ((eq? obj #f)
	  18)
	 ((eq? obj (if #f #f))
	  26)
	 (else
	  (let ((ptr (hashq-ref ptr-hash obj)))
	    (* 4 (or ptr
		     (begin (emit obj)
			    (hashq-ref ptr-hash obj)))))))))

    (asm obj)

    ;; The toplevel might contain symbols or keywords that are
    ;; otherwise unreferenced, and keywords might reference otherwise
    ;; unused symbols, so the order here is important.
    ;;
    (let ((all-bindings (hash-map->list cons boot-bindings)))
      (asm all-bindings)
      (asm all-keywords)
      (asm all-symbols)
      (for-each (lambda (idx)
		  (pk 'bootindex idx)
		  (emit-words idx (pk 'word (asm (list all-symbols
						       all-keywords
						       all-bindings)))))
		bootinfo-idxs))

    (u32subvector mem 0 idx)))

(define (u32subvector vec start end)
  (let ((vec2 (make-u32vector (- end start))))
    (do ((i start (1+ i)))
	((= i end))
      (u32vector-set! vec2 i (u32vector-ref vec i)))
    vec2))
