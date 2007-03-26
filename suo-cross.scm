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
  (if define?
      (if (memq sym strong-boot-bindings)
	  (begin
	    (pk 'ignore sym)
	    ignored-variable)
	  (begin
	    (pk 'boot-define sym)
	    (module-make-local-var! boot-module sym)))
      (begin
	;;(pk 'lookup sym)
	(module-local-variable boot-module sym))))

(set-module-eval-closure! boot-module boot-module-eval-closure)

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

;; As promised, we can simply use 'eval' and 'load' to evaluate forms
;; in the boot environment.

(define (boot-eval form)
  (eval form boot-module))

(define (boot-load filename)
  (pk 'boot-load filename)
  (save-module-excursion
   (lambda ()
     (set-current-module boot-module)
     (load filename))))

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
;; symbols, keywords and strings are implemented directly by using the
;; host Scheme data tyes.  The compiler uses nothing Suo specific
;; about them.

(boot-import eq?

	     + - * < = > <= >= quotient remainder

	     pair? cons car cdr set-car! set-cdr!

	     char? integer->char char->integer

	     vector? vector-length make-vector vector-ref vector-set!
	     vector->list

	     symbol? string->symbol symbol->string gensym

	     keyword? keyword->symbol

	     string? make-string string-length string-ref string-set!)

;; Records are implemented using, errm, records, but with records of a
;; different kind.  The fields of the Suo record are simply stored as
;; a Scheme vector.

(define suo-record-record-type
  (make-record-type 'record-record '(type fields)
		    (lambda (obj port) (display "#<suo-record>" port))))

(define suo:record? (record-predicate suo-record-record-type))

(define (suo:record-length rec)
  (suo:record-ref (suo:record-type rec) 0))

(define suo:record
  (let ((constructor (record-constructor suo-record-record-type)))
    (lambda (type . fields)
      (constructor type (apply vector fields)))))

(define suo:make-record
  (let ((constructor (record-constructor suo-record-record-type)))
    (lambda (type init)
      (constructor type (make-vector (suo:record-ref type 0) init)))))

(define suo:record-type (record-accessor suo-record-record-type 'type))

(define suo-record-fields (record-accessor suo-record-record-type 'fields))

(define (suo:record-ref rec idx)
  (vector-ref (suo-record-fields rec) idx))

(define (suo:record-set! rec idx val)
  (vector-set! (suo-record-fields rec) idx val))

(define suo-record-set-type! (record-modifier suo-record-record-type 'type))

;; The type of record types.  It has three fields:
;;
;; n-fields - the number of fields as a fixnum
;; name     - a arbitrary symbol used when printing, etc.
;; ancestry - a vector with all parent types; see below for details
;;
;; The '(is-a OBJ TYPE)' relation consists in determining whether TYPE
;; is among the types in the 'ancestry' vector of the type of OBJ.
;; Since only single-inheritance is supported, the test doesn't
;; require searching all of the ancestry vector.  If it is in the
;; ancestry it can only be in one position and we can check that
;; position directly.
;;
;; More concretely, the ancestry vector lists the parent types starting
;; with the root type at position zero and ending with the type it
;; belongs to in the last position.

(define suo:record-type-type (suo:record #f 3 'record-type (vector #f)))
(suo-record-set-type! suo:record-type-type
		      suo:record-type-type)
(vector-set! (suo:record-ref suo:record-type-type 2) 0 suo:record-type-type)

(define (suo:record-with-type? obj type)
  (and (suo:record? obj)
       (eq? (suo:record-type obj) type)))

(define (suo:record-is-a? obj type)
  (and (suo:record? obj)
       (let ((ancestry (suo:record-ref (suo:record-type obj) 2))
	     (pos (1- (vector-length (suo:record-ref type 2)))))
	 (and (< pos (vector-length ancestry))
	      (eq? type (vector-ref ancestry pos))))))

(define (suo:make-record-type n-fields name parent-type)
  (if parent-type
      (error "inheritance not supported"))
  (let* ((ancestry (vector #f))
	 (type (suo:record suo:record-type-type n-fields name ancestry)))
    (vector-set! ancestry 0 type)
    type))

(boot-import record? record-with-type?
	     record-type record-length record-ref record-set!
	     record-type-type record make-record make-record-type)

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

;;; Basic record types, needed by the compiler.  These are not defined
;;; in "suo-base" since we need them to be identical (i.e., 'eq?') in
;;; the boot environment and the image environment.  See below.

(define suo:box-type (suo:make-record-type 1 'box #f))

(define suo:variable-type (suo:make-record-type 2 'variable #f))

(define suo:macro-type (suo:make-record-type 2 'macro #f))

(define suo:closure-type (suo:make-record-type 3 'closure #f))

(define suo:string-type (suo:make-record-type 1 'string #f))

(define suo:symbol-type (suo:make-record-type 1 'symbol #f))

(define suo:keyword-type (suo:make-record-type 1 'keyword #f))

(define suo:bignum-type (suo:make-record-type 1 'bignum #f))

(boot-import box-type variable-type macro-type closure-type
             string-type symbol-type keyword-type bignum-type)

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

(define (suo:bytevec-ref-u16 v i)
  (+ (* (u8vector-ref v (* 2 i)) #x100)
     (u8vector-ref v (1+ (* 2 i)))))

(define (suo:bytevec-ref-u16 v i)
  (+ (* (u8vector-ref v (* 2 i)) #x100)
     (u8vector-ref v (1+ (* 2 i)))))

(define (suo:bytevec-set-u16! v i val)
  (u8vector-set! v (* 2 i) (quotient val #x100))
  (u8vector-set! v (1+ (* 2 i)) (remainder val #x100)))

(define (suo-bytevec-u8->list bv)
  (u8vector->list bv))

(define (suo-bytevec-u32->list bv)
  (do ((i 0 (1+ i))
       (res '() (cons (+ (* (suo:bytevec-ref-u16 bv (* 2 i)) #x10000)
			 (suo:bytevec-ref-u16 bv (1+ (* 2 i))))
		      res)))
      ((= i (quotient (suo:bytevec-length-16 bv) 2))
       (reverse! res))))

(boot-import make-bytevec bytevec?
	     bytevec-length-8 bytevec-ref-u8 bytevec-set-u8!
	     bytevec-length-16 bytevec-ref-u16 bytevec-set-u16!)

;; string-chars is used for low-level debugging.  It returns a bytevec
;; with the characters of the given string.

(define (suo:string-chars str)
  (list->u8vector (map char->integer (string->list str))))

(boot-import string-chars)

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

(define (suo:call/p parameter value func)
  (with-fluid* (parameter #f 'get-fluid) value func))

(boot-import make-parameter call/p)

;; We also import the printing functions into the boot environment so
;; that output is fastish..

(boot-import display write newline pretty-print)

;; Hash tables that use eq? as the equality check are implemented by
;; the Suo runtime since they need to interact closely with the GC.

(define suo:make-hashq-table make-hash-table)

(boot-import make-hashq-table hashq-ref hashq-set!)


;;; The image environment

;; The compiler itself doesn't define toplevel variables or macros in
;; the image environment.  That is done by 'image-eval' below.  The
;; compiler merely looks up variables and macros with
;; 'lookup-toplevel-variable', and 'lookup-toplevel-macro',
;; respectively.
;;
;; Toplevel variables follows the Suo model, of course: when a
;; variable is used for the first time, it is silently created by
;; 'lookup-toplevel-variable'.  However, there will be warnings at
;; then end of compilation about referenced but not yet defined
;; variables.

(define image-bindings '())
(define image-variable-inits '())

(define (register-image-binding sym obj)
  (set! image-bindings (acons sym obj image-bindings)))

(define (lookup-image-variable sym)
  (let ((var (assq-ref image-bindings sym)))
    (if var
	(if (suo:record-with-type? var suo:variable-type)
	    var
	    (error "not a variable: " sym))
	(let ((var (suo:record suo:variable-type (if #f #f) sym)))
	  (pk 'image-variable sym)
	  (register-image-binding sym var)
	  var))))

(define (check-undefined-variables)
  (pk (length image-bindings) 'bindings)
  (for-each (lambda (binding)
	      (let ((sym (car binding))
		    (obj (cdr binding)))
		(if (and (suo:record-with-type? obj suo:variable-type)
			 (eq? (if #f #f) (suo:record-ref obj 0))
			 (not (memq obj image-variable-inits)))
		    (pk 'undefined sym))))
	    image-bindings))

(define suo:lookup-toplevel-variable lookup-image-variable)
(boot-import lookup-toplevel-variable)

;; Toplevel macros are compiled and made available in the image, but
;; they are never used to expand forms during compilation.  Instead,
;; the compiler uses the macros in the boot environment to expand code
;; that it compiles for the image environment.

(define (register-image-macro sym val)
  (if (assq-ref image-bindings sym)
      (error "redefined: " sym)
      (begin
	(pk 'image-macro sym)
	(register-image-binding sym (suo:record suo:macro-type val sym)))))

(define (lookup-boot-macro-transformer sym)
  (assq-ref boot-macro-transformers sym))

(define suo:lookup-toplevel-macro-transformer lookup-boot-macro-transformer)
(boot-import lookup-toplevel-macro-transformer)

;; Some of the toplevel forms that are compiled are simple enough that
;; they can be carried out at compile time.  Other forms are collected
;; into one big 'toplevel expression' that is then compiled into a
;; procedure that will be called when the image is booted.

(define image-expressions '())

(define (add-image-expression exp)
  (pk 'image-exp exp)
  (set! image-expressions (cons exp image-expressions)))

(define (add-image-variable-init var val)
  (set! image-variable-inits (cons var image-variable-inits))
  (add-image-expression `(primop record-set ',var 0 ,val)))

(define (image-expression)
  (cons 'begin (reverse image-expressions)))

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

  (define (eval1 form)
    (let ((exp (boot-eval `(compile ',form))))
      (if (pair? exp)
	  (case (car exp)
	    ((:begin)
	     (let loop ((forms (cdr exp)))
	       (cond ((null? forms)
		      (if #f #f))
		     ((null? (cdr forms))
		      (eval1 (car forms)))
		     (else
		      (eval1 (car forms))
		      (loop (cdr forms))))))
	    ((:set :define)
	     (let* ((name (cadr exp))
		    (val (eval1 (caddr exp)))
		    (var (lookup-image-variable name)))
	       (if (constant? val)
		   (suo:record-set! var 0 (constant-value val))
		   (add-image-variable-init var val))
	       (if #f #f)))
	    ((:define-macro)
	     (let* ((name (cadr exp))
		    (val (eval1 (caddr exp))))
	       (if (constant? val)
		   (register-image-macro name (constant-value val))
		   (error "macro transformer not constant: " val)))
	     (if #f #f))
	    (else
	     exp))
	  exp)))

  (let ((exp (eval1 form)))
    (if (not (constant? exp))
	(add-image-expression exp))))

;; Where there is 'eval', there is 'load'.

(define (image-load file)
  (pk 'image-load file)
  (with-input-from-file file
    (lambda ()
      (let loop ((form (read)))
	(cond ((not (eof-object? form))
	       (image-eval form)
	       (loop (read))))))))

;; We copy some variable bindings from the boot environment over to
;; the image environment in order to ease bootstrapping further.  All
;; bindings that are copied are for record types that the compiler
;; uses.  it is easier to define these record types once for both the
;; boot and image environment.

(define (import-image-binding-from-boot sym)
  (let ((var (lookup-image-variable sym)))
    (suo:record-set! var 0 (boot-eval sym))))

(define-macro (image-import-from-boot . syms)
  `(begin
     ,@(map (lambda (sym)
	      `(import-image-binding-from-boot ',sym))
	    syms)))
  
(image-import-from-boot record-type-type
			string-type
			symbol-type
			keyword-type
			bignum-type
			variable-type
			macro-type
			closure-type
			box-type)

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

  (let ((mem (make-u32vector (* 1024 1024)))
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
	       (fields (vector->list (suo-record-fields obj)))
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
       ((suo:bytevec? obj)
	(let* ((len (suo:bytevec-length-8 obj))
	       (words (quotient (+ len 3) 4))
	       (ptr (alloc obj (1+ words))))
	  (apply emit-words ptr
		 (+ #x80000000 (* len 16) 11)
		 (bytes->words (suo-bytevec-u8->list obj)))))
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
	  (set! all-symbols (cons suo-sym all-symbols))
	  (emit suo-sym)
	  ;; register the original OBJ as required
	  (hashq-set! ptr-hash obj (hashq-ref ptr-hash suo-sym))))
       ((keyword? obj)
	(let ((suo-keyword (suo:record suo:keyword-type (keyword->symbol obj))))
	  (set! all-keywords (cons suo-keyword all-keywords))
	  (emit suo-keyword)
	  ;; register the original OBJ as required
	  (hashq-set! ptr-hash obj (hashq-ref ptr-hash suo-keyword))))
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
	;; bignum, fixnums are handled by asm below
	(let ((suo-bignum (integer->bignum obj)))
	  (emit suo-bignum)
	  ;; register the original OBJ as required
	  (hashq-set! ptr-hash obj (hashq-ref ptr-hash suo-bignum))))
       (else
	(error "unsupported value: " obj))))

    (define (asm obj)
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
			  (hashq-ref ptr-hash obj))))))))

    (asm obj)

    ;; The image-bindings might contain symbols or keywords that are
    ;; otherwise unreferenced, and keywords might use otherwise unused
    ;; symbols, so the order here is important.
    ;;
    (asm image-bindings)
    (asm all-keywords)
    (asm all-symbols)
    (for-each (lambda (idx)
		(emit-words idx (asm (list all-symbols
					   all-keywords
					   image-bindings))))
	      bootinfo-idxs)

    (u32subvector mem 0 idx)))

(define (u32subvector vec start end)
  (let ((vec2 (make-u32vector (- end start))))
    (do ((i start (1+ i)))
	((= i end))
      (u32vector-set! vec2 i (u32vector-ref vec i)))
    vec2))
