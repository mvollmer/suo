;;;; suo-cross -- Part of Suo in plain Scheme

;; When bootstrapping Suo with a second Scheme implementation, we just
;; load a number of files with the toplevel definitions for Suo into
;; the 'build environment'.  These definitions start with basic things
;; like 'cond' and end with the compiler.  These files are called "the
;; suo boot files".
;;
;; After the compiler has been put into the build environment in this
;; way, it is used to compile the 'suo image files'.  The result of
;; this compilation is an image with a boot procedure.  When this
;; image is loaded by the suo runtime, the boot procedure is executed
;; and will carry out the actions specified in the suo boot files.
;;
;; The build environment is special: before loading the boot files
;; into it, it already contains a carefully selected set of bindings
;; and attempts to redefine those bindings will be ignored.  In this
;; way, the boot files can contain definitions that are right for Suo,
;; but need to be different for the build environment.
;;
;; The pre-defined bindings are things like 'if', 'cons', 'car',
;; 'record-ref', etc.  For Suo, they are defined in terms of
;; primitives understood by the compiler and code-generator.  For the
;; build environment, we want to use the native definitions or provide
;; suitable definitions in this file.
;;
;; While compiling the suo image files to get the suo image, the
;; compiler creates the 'image environment'.  This environment
;; contains macros and functions for the suo runtime environment and
;; the macros in it are therefore not useable at compile time.  The
;; compiler looks up macros in the build environment when compiling
;; the image files.  Thus, the boot files need to provide all macros
;; used by the image files.

;;; Representation of suo objects as Scheme values

;; fixnum, characters, singletons, pairs, vectors, strings - directly
;; records - record-record (no bit records)
;; bytevectors - u8vector
;; code - code-record

(define suo:record-record-type
  (make-record-type 'record-record '(type fields)
		    (lambda (obj port) (display "#<suo-record>" port))))

(define suo:record? (record-predicate suo:record-record-type))

(define (suo:record-with-type? obj type)
  (and (suo:record? obj)
       (eq? (suo:record-type obj) type)))

(define (suo:record-length rec)
  (suo:record-ref (suo:record-type rec) 0))

(define suo:record
  (let ((constructor (record-constructor suo:record-record-type)))
    (lambda (type . fields)
      (constructor type (apply vector fields)))))

(define suo:make-record
  (let ((constructor (record-constructor suo:record-record-type)))
    (lambda (type init)
      (constructor type (make-vector (suo:record-ref type 0) init)))))

(define suo:record-type (record-accessor suo:record-record-type 'type))

(define suo:record-fields (record-accessor suo:record-record-type 'fields))

(define (suo:record-ref rec idx)
  (vector-ref (suo:record-fields rec) idx))

(define (suo:record-set! rec idx val)
  (vector-set! (suo:record-fields rec) idx val))

(define suo:record-set-type! (record-modifier suo:record-record-type 'type))

(define suo:code-record-type
  (make-record-type 'code-record '(insns literals)
		    (lambda (obj port) (display "#<suo-code>" port))))

(define suo:code? (record-predicate suo:code-record-type))

(define suo:code (record-constructor suo:code-record-type))

(define suo:code-insns (record-accessor suo:code-record-type 'insns))

(define suo:code-literals (record-accessor suo:code-record-type 'literals))

;;; The self-referential type of record types

(define suo:record-type-type (suo:record #f 2 'record-type))
(suo:record-set-type! suo:record-type-type
		      suo:record-type-type)

;;; Basic record types, needed by the compiler

(define (suo:make-record-type n-fields name)
  (suo:record suo:record-type-type n-fields name))

(define suo:box-type (suo:make-record-type 1 'box))

(define suo:variable-type (suo:make-record-type 2 'variable))

(define suo:macro-type (suo:make-record-type 2 'macro))

(define suo:closure-type (suo:make-record-type 2 'closure))

(define suo:symbol-type (suo:make-record-type 1 'symbol))

(define suo:keyword-type (suo:make-record-type 1 'keyword))

;;; The image environment

;; The compiler accesses and manipulates the image environment by
;; calling 'register-toplevel-macro', 'lookup-toplevel-macro', and
;; 'lookup-toplevel-variable'.  It also uses 'expand-macro' to invoke
;; a macro transformer function.
 
;;; The build environment

;; Things are fairly simple for toplevel variable definitions.  When
;; creating the suo-like environment in the build system by loading
;; the suo boot files, a toplevel variable definition is either
;; ignored (when a definition for the same name already exists), or a
;; new variable is installed.
;;
;; Macros are more complicated.  A macro that is defined when loading
;; the suo boot files is put both into the suo-like environment (when
;; no definition for its name exists), and made available ...

(define suo:toplevel '())
(define suo:build-macros '())

(define (suo:expand-macro macro form)
  (apply macro (cdr form)))

(define-macro (suo:define-macro head . rest)
  (if (pair? head)
      `(define-macro ,(car head) (lambda ,(cdr head) ,@rest))
      (register-build-macro head (car rest))))

(define (register-build-macro sym transformer-exp)
  (pk 'build-macro sym)
  (let ((transformer (eval transformer-exp build-module)))
    (eval `(define ,sym ',(defmacro:transformer transformer)) build-module)
    (set! suo:build-macros (acons sym transformer suo:build-macros))))

(define (register-toplevel-binding sym obj)
  (if (assq-ref suo:toplevel sym)
      (error "redefinition" sym))
  (set! suo:toplevel (acons sym obj suo:toplevel))
  obj)
  
(define (suo:lookup-toplevel-variable sym)
  (if (suo:lookup-toplevel-macro sym)
      (error "not a variable:" sym))
  (or (assq-ref suo:toplevel sym)
      (let ((var (suo:record suo:variable-type (if #f #f) sym)))
	(pk 'variable sym)
	(register-toplevel-binding sym var))))

(define (suo:register-toplevel-macro sym val)
  (pk 'macro sym)
  (register-toplevel-binding sym (suo:record suo:macro-type val sym)))

(define (suo:lookup-toplevel-macro sym)
  (assq-ref suo:build-macros sym))

(define (suo:sys:halt)
  (error "halt"))

(define (suo:make-parameter val)
  (let ((f (make-fluid)))
    (fluid-set! f val)
    (lambda args
      (if (null? args)
	  (fluid-ref f)
	  (fluid-set! f (car args))))))

(define build-pre-defines
  '(lambda
    define
    define-macro
    begin
    quote
    quasiquote
    if
    let let* letrec
    set!
    apply
    dynamic-wind
    make-parameter
    sys:halt
    
    lookup-toplevel-macro expand-macro
    lookup-toplevel-variable

    ;; Output
    display write newline pretty-print

    ;; Testing and booleans
    eq?

    ;; Pairs
    pair? cons car cdr set-car! set-cdr!
    ;; map for-each cadr caddr append memv memq
    ;; acons assq assq-ref

    ;; Characters
    char? integer->char char->integer

    ;; Strings
    string? make-string string-length string-ref string-set!

    ;; Bootinfo
    bootinfo

    ;; Symbols
    symbol-type symbol? string->symbol symbol->string gensym

    ;; Keywords
    keyword-type keyword? keyword->symbol

    ;; Fixnums
    fixnum? +:2 -:2 *:2 quotient remainder <
    ;; + - * <= >= >

    ;; Vectors
    vector? vector-length make-vector vector-ref vector-set!
    vector->list

    ;; Records
    record? record-with-type? record-type record-length record-ref record-set!
    make-record record-type-type make-record-type

    ;; Types
    variable-type macro-type box-type closure-type

    ;; Code
    code code? code-insns code-literals

    ;; XXX
    make-u32vector u32vector-ref u32vector-set! u32vector-length
    u32vector->list
    make-hash-table hashq-ref hashq-set!
    ash expt))

(define build-module (make-module 1031))

(define ignored-variable (make-variable #f))

(define (suo:bootinfo)
  (cons '() '()))

(define (suo:fixnum? n)
  (integer? n))

(define (suo:+:2 a b)
  (+ a b))

(define (suo:-:2 a b)
  (- a b))

(define (suo:*:2 a b)
  (* a b))

(for-each (lambda (sym)
	    (module-add! build-module sym
			 (or (module-variable (current-module)
					      (symbol-append 'suo: sym))
			     (module-variable (current-module)
					      sym)
			     (error "nope" sym))))
	  build-pre-defines)

(set-module-eval-closure! 
 build-module
 (lambda (sym define?)
   (if define?
       (if (memq sym build-pre-defines)
	   (begin
	     (pk 'ignore sym)
	     ignored-variable)
	   (begin
	     (pk 'define sym)
	     (module-make-local-var! build-module sym)))
       (begin
	 ;;(pk 'lookup sym)
	 (module-local-variable build-module sym)))))

(define (suo:load-for-build filename)
  (save-module-excursion
   (lambda ()
     (set-current-module build-module)
     (load filename))))

(define (suo:eval-for-build form)
  (eval form build-module))

(define suo:topexp '())

(define (suo:add-toplevel-expression exp)
  (pk 'cross-exp exp)
  (set! suo:topexp (cons exp suo:topexp)))

(define (suo:toplevel-expression)
  (cons 'begin (reverse suo:topexp)))

(define (literal? exp)
  (and (not (pair? exp)) (not (symbol? exp))))

(define (suo:cross-eval form)
  (let ((exp (suo:eval-for-build `(macroexpand ',form))))
    (if (pair? exp)
	(case (car exp)
	  ((:define)
	   (let* ((name (cadr exp))
		  (val (caddr exp))
		  (var (suo:lookup-toplevel-variable name))
		  (comp-val (suo:eval-for-build `(compile ',val))))
	     (if (and (pair? comp-val) (eq? :quote (car comp-val)))
		 (suo:record-set! var 0 (cadr comp-val))
		 (if (literal? comp-val)
		     (suo:record-set! var 0 comp-val)
		     (suo:add-toplevel-expression 
		      `(primop record-set ',var 0 ,comp-val))))))
	  ((:define-macro)
	   (let* ((name (cadr exp))
		  (val (caddr exp))
		  (comp-val (suo:eval-for-build `(compile ',val))))
	     (suo:register-toplevel-macro name comp-val)))
	  (else
	   (suo:add-toplevel-expression exp))))))

(define (suo:load-for-image file)
  (with-input-from-file file
    (lambda ()
      (let loop ((form (read)))
	(cond ((not (eof-object? form))
	       (suo:cross-eval form)
	       (loop (read))))))))

(define (import-from-build sym)
  (let ((var (suo:lookup-toplevel-variable sym)))
    (suo:record-set! var 0 (suo:eval-for-build sym))))

(define (import-build-types)
  (for-each import-from-build '(record-type-type
				symbol-type
				keyword-type
				variable-type
				macro-type
				closure-type
				box-type)))
