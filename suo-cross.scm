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
;; bytevecs - u8vector
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

(define suo:string-type (suo:make-record-type 1 'string))

(define suo:symbol-type (suo:make-record-type 1 'symbol))

(define suo:keyword-type (suo:make-record-type 1 'keyword))

(define suo:bignum-type (suo:make-record-type 1 'bignum))

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
      (cond ((null? args)
	     (fluid-ref f))
	    ((null? (cdr args))
	     (fluid-set! f (car args)))
	    (else
	     f)))))

(define (suo:call/p parameter value func)
  (with-fluid* (parameter #f 'get-fluid) value func))

(define (host-string-chars str)
  (apply u8vector (map char->integer (string->list str))))

(define topexp-variable-inits '())

(define (topexp-variable-init? var)
  (memq var topexp-variable-inits))

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

(define (suo:bytevec-ref-u16 v i)
  (+ (* (u8vector-ref v (* 2 i)) #x100)
     (u8vector-ref v (1+ (* 2 i)))))

(define (suo:bytevec-set-u16! v i val)
  (u8vector-set! v (* 2 i) (quotient val #x100))
  (u8vector-set! v (1+ (* 2 i)) (remainder val #x100)))

(define (suo:bytevec-u32->list bv)
  (do ((i 0 (1+ i))
       (res '() (cons (+ (* (suo:bytevec-ref-u16 bv (* 2 i)) #x10000)
			 (suo:bytevec-ref-u16 bv (1+ (* 2 i))))
		      res)))
      ((= i (quotient (suo:bytevec-length-16 bv) 2))
       (reverse! res))))

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
    make-parameter call/p dynamic-wind
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
    bootinfo suo-bootinfo-marker

    ;; Strings
    string-type

    ;; Symbols
    symbol-type symbol? string->symbol symbol->string gensym

    ;; Keywords
    keyword-type keyword? keyword->symbol

    ;; Fixnums
    fixnum? +:2 -:2 *:2 quotient remainder <
    ;; + - * <= >= >

    ;; Bignums
    bignum-type

    ;; Vectors
    vector? vector-length make-vector vector-ref vector-set!
    vector->list

    ;; Records
    record? record-with-type? record-type record-length record-ref record-set!
    make-record record-type-type make-record-type

    ;; Types
    variable-type macro-type box-type closure-type

    ;; Code
    code? code

    ;; Bytevecs
    make-bytevec bytevec?
    bytevec-length-8 bytevec-ref-u8 bytevec-set-u8!
    bytevec-length-16 bytevec-ref-u16 bytevec-set-u16!

    ;; XXX
    topexp-variable-init?
    make-hash-table 
    make-hashq-table hashq-ref hashq-set!
    host-string-chars
    ash expt))

(define build-module (make-module 1031))

(define ignored-variable (make-variable #f))

(define suo-bootinfo-marker (list 'bootinfo))

(define (suo:bootinfo)
  (list '() '() '()))

(define (suo:fixnum? n)
  (and (integer? n)
       (<= -536870912 n 536870911)))

(define (suo:+:2 a b)
  (+ a b))

(define (suo:-:2 a b)
  (- a b))

(define (suo:*:2 a b)
  (* a b))

(define suo:make-hashq-table make-hash-table)

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
	  ((:begin)
	   (let loop ((forms (cdr exp)))
	     (cond ((null? forms)
		    (if #f #f))
		   ((null? (cdr forms))
		    (suo:cross-eval (car forms)))
		   (else
		    (suo:cross-eval (car forms))
		    (loop (cdr forms))))))
	  ((:define)
	   (let* ((name (cadr exp))
		  (val (caddr exp))
		  (var (suo:lookup-toplevel-variable name))
		  (comp-val (suo:eval-for-build `(compile ',val))))
	     (if (and (pair? comp-val) (eq? :quote (car comp-val)))
		 (suo:record-set! var 0 (cadr comp-val))
		 (if (literal? comp-val)
		     (suo:record-set! var 0 comp-val)
		     (begin
		       (set! topexp-variable-inits 
			     (cons var topexp-variable-inits))
		       (suo:add-toplevel-expression
			`(primop record-set ',var 0 ,comp-val)))))))
	  ((:define-macro)
	   (let* ((name (cadr exp))
		  (val (caddr exp))
		  (comp-val (suo:eval-for-build `(compile ',val))))
	     (if (and (pair? comp-val) (eq? :quote (car comp-val)))
		 (suo:register-toplevel-macro name (cadr comp-val))
		 (error "expected ':quote': " comp-val))))
	  (else
	   (suo:add-toplevel-expression exp))))))

(define (suo:check-undefineds)
  (pk (length suo:toplevel) 'bindings)
  (for-each (lambda (binding)
	      (let ((sym (car binding))
		    (obj (cdr binding)))
		(if (and (suo:record-with-type? obj suo:variable-type)
			 (eq? (if #f #f) (suo:record-ref obj 0))
			 (not (topexp-variable-init? obj)))
		    (pk 'undefined sym))))
	    suo:toplevel))

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
				string-type
				symbol-type
				keyword-type
				bignum-type
				variable-type
				macro-type
				closure-type
				box-type)))

;;; Dumping suo objects into a u32vector

(define all-suo-symbols '())
(define all-suo-keywords '())

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
      (suo:eval-for-build `(limbs->bignum ',v))))

  (if (> x 0)
      (positive->bignum x)
      (error "no negative bignums yet, sorry")))

(define (dump-object obj)
  (pk 'dump)

  (let ((mem (make-u32vector 102400))
	(idx 0)
	(ptr-hash (make-hash-table 100311))
	(bootinfo-idxs '()))

    (define (grow-mem)
      (let ((mem2 (make-u32vector (+ idx 102400)))
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

    (define (bytes->words bytes)
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

    (define (emit obj)
      (cond
       ((pair? obj)
	(let ((ptr (alloc obj 2)))
	  (emit-words ptr (asm (car obj)) (asm (cdr obj)))))
       ((suo:record? obj)
	(let* ((type (suo:record-type obj))
	       (fields (vector->list (suo:record-fields obj)))
	       (ptr (alloc obj (1+ (length fields)))))
	  (if (not (eqv? (length fields) (suo:record-ref type 0)))
	      (error "inconsistent record"))
	  (apply emit-words ptr
		 (+ (asm type) 3)
		 (map asm fields))))
       ((vector? obj)
	(let ((ptr (alloc obj (1+ (vector-length obj)))))
	  (apply emit-words ptr
		 (+ #x80000000 (* (vector-length obj) 16) 3)
		 (map asm (vector->list obj)))))
       ((u8vector? obj)
	(let* ((len (u8vector-length obj))
	       (words (quotient (+ len 3) 4))
	       (ptr (alloc obj (1+ words))))
	  (apply emit-words ptr
		 (+ #x80000000 (* len 16) 11)
		 (bytes->words (u8vector->list obj)))))
       ((string? obj)
	(let ((suo-str (suo:record suo:string-type
				   (apply u8vector
					  (map char->integer
					       (string->list obj))))))
	  (emit suo-str)
	  ;; register the original OBJ as required
	  (hashq-set! ptr-hash obj (hashq-ref ptr-hash suo-str))))
       ((symbol? obj)
	(let ((suo-sym (suo:record suo:symbol-type (symbol->string obj))))
	  (set! all-suo-symbols (cons suo-sym all-suo-symbols))
	  (emit suo-sym)
	  ;; register the original OBJ as required
	  (hashq-set! ptr-hash obj (hashq-ref ptr-hash suo-sym))))
       ((keyword? obj)
	(let ((suo-keyword (suo:record suo:keyword-type (keyword->symbol obj))))
	  (set! all-suo-keywords (cons suo-keyword all-suo-keywords))
	  (emit suo-keyword)
	  ;; register the original OBJ as required
	  (hashq-set! ptr-hash obj (hashq-ref ptr-hash suo-keyword))))
       ((suo:code? obj)
	(let* ((insns (suo:code-insns obj))
	       (insn-words (quotient (suo:bytevec-length-8 insns) 4))
	       (literals (suo:code-literals obj))
	       (literal-words (vector-length literals))
	       (ptr (alloc obj (+ 1 insn-words literal-words))))
	  (apply emit-words ptr
		 (+ #x80000000
		    (* insn-words (* 256 16))
		    (* literal-words 16)
		    15)
		 (append (suo:bytevec-u32->list insns)
			 (map asm (vector->list literals))))))
       ((integer? obj)
	;; bignum
	(let ((suo-bignum (integer->bignum obj)))
	  (emit suo-bignum)
	  ;; register the original OBJ as required
	  (hashq-set! ptr-hash obj (hashq-ref ptr-hash suo-bignum))))
       (else
	(error "unsupported value: " obj))))

    (define (asm obj)
      (cond
       ((eq? obj suo-bootinfo-marker)
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
			  (hashq-ref ptr-hash obj))))))))

    (asm obj)

    ;; The toplevel might contain symbols or keywords that are
    ;; otherwise unreferenced, and keywords might otherwise unused
    ;; symbols, so the order here is important.
    ;;
    (asm suo:toplevel)
    (asm all-suo-keywords)
    (asm all-suo-symbols)
    (for-each (lambda (idx)
		(emit-words idx (asm (list all-suo-symbols
					   all-suo-keywords
					   suo:toplevel))))
	      bootinfo-idxs)

    (u32subvector mem 0 idx)))

(define (u32subvector vec start end)
  (let ((vec2 (make-u32vector (- end start))))
    (do ((i start (1+ i)))
	((= i end))
      (u32vector-set! vec2 i (u32vector-ref vec i)))
    vec2))
