(define (pp val)
  (pretty-print val)
  val)

;;; Structures

;; (define-struct NAME BASE FIELD...) defines a new structure type.
;;
;; BASE is either (), indicating no base type, or (BASE-NAME), where
;; BASE-NAME is the name of a previously defined structure type.
;;
;; FIELD is either just NAME (a symbol, called a non-defaulted field)
;; or (NAME DEFAULT) where DEFAULT is a an arbitrary expression.
;;
;; (NAME VALUE...) is a constructor and returns a new object with
;; fields initialized according to the arguments of the constructor.
;; VALUE... is a list with one member for each non-defaulted field of
;; the structure, including those from BASE.  (The expressions for
;; defaulted fields are evaluated in an environment where the values
;; for the non-defaulted fields are visible with their names. XXX -
;; not yet.)
;;
;; (NAME? obj) is the predicate.
;;
;; (NAME-FIELD obj) is an accessor (getter and setter) for FIELD.
;;
;; define-struct also registers the structure type so that it can be
;; used as a BASE for other structure types and with struct-case.

;; The meta structure must be done in long hand...

(define-class struct-type ()
  (base :accessor struct-type-base
	:init-keyword :base)
  (direct-fields :accessor struct-type-direct-fields
		 :init-keyword :direct-fields))

(define (struct-type-fields type)
  (append (if (struct-type-base type)
	      (struct-type-fields (struct-type-base type))
	      '())
	  (struct-type-direct-fields type)))

(define struct-types '())

(define (register-struct-type name type)
  (set! struct-types (acons name type struct-types)))

(define (find-struct-type name)
  (or (assq-ref struct-types name)
      (error "no such struct type" name)))

(define-macro (define-struct name opt-base . fields)

  (define (field-name f) (if (list? f) (car f) f))
  (define field-defaulted? list?)
  (define field-default cadr)
  
  (define (field->slot f)
    (let ((fn (field-name f)))
      `(,fn :accessor ,(symbol-append name '- fn)
	    :init-keyword ,(symbol->keyword fn)
	    ,@(if (field-defaulted? f) 
		  `(:init-value ,(field-default f)) 
		  '()))))

  (let* ((class-name (symbol-append name '/class))
	 (base-name (if (null? opt-base) #f (car opt-base)))
	 (base (and base-name (find-struct-type base-name)))
	 (accessors (map (lambda (f)
			   (symbol-append name '- (field-name f)))
			 fields))
	 (type (make struct-type 
		 :base base
		 :direct-fields fields))
	 (constructor-args (remove-if field-defaulted?
				      (struct-type-fields type))))

    (register-struct-type name type)

    `(begin
       (define-class ,class-name ,(if base-name
				      (list (symbol-append base-name '/class))
				      '())
	 ,@(map field->slot fields))
       (define (,(symbol-append name '?) x) (is-a? x ,class-name))
       (define (,name ,@constructor-args)
	 (make ,class-name
	   ,@(apply append (map (lambda (f)
				  (list (symbol->keyword f)
					f))
				constructor-args)))))))

(define-macro (struct-case exp . clauses)
  ;; clause -> ((NAME FIELD...) BODY...)
  (let ((tmp (gensym)))
    (define (field-name f) (if (list? f) (car f) f))
    (define (struct-clause->cond-clause clause)
      (let* ((pattern (car clause))
	     (name (car pattern))
	     (args (cdr pattern))
	     (body (cdr clause)))
	`((,(symbol-append name '?) ,tmp)
	  ((lambda ,args ,@body)
	   ,@(map (lambda (f)
		    `(,(symbol-append name '- (field-name f)) ,tmp))
		  (list-head
		   (struct-type-fields (find-struct-type name))
		   (length args)))))))
    `(let ((,tmp ,exp))
       (cond ,@(map struct-clause->cond-clause clauses)
	     (else (error "unsupported struct instance" ,tmp))))))

;;; Pattern matching

;; A pattern is a cons tree with embedded pattern variables.  A
;; pattern variable matches anything, everything else matches only
;; things that are eqv? to it.  A pattern variable is a symbol that
;; starts with a '?'.

(define (pattern-variable? var)
  (and (symbol? var) (char=? (string-ref (symbol->string var) 0) #\?)))

;; Return a list of the pattern variables contained in PAT.

(define (pattern-variables pat)
  (cond
   ((pattern-variable? pat)
    (list pat))
   ((pair? pat)
    (append (pattern-variables (car pat)) (pattern-variables (cdr pat))))
   (else
    '())))

;; Determine whether VAL matches PAT and when it does, return a list
;; of the values of the pattern variables, in the same order as
;; returned by pattern-variables.  When VAL doesn't match, return #f.

(define (pattern-match pat val)
  (cond
   ((pattern-variable? pat)
    (list val))
   ((pair? pat)
    (and (pair? val)
	 (let ((car-match (pattern-match (car pat) (car val)))
	       (cdr-match (pattern-match (cdr pat) (cdr val))))
	   (and car-match cdr-match (append car-match cdr-match)))))
   (else
    (and (eqv? pat val) '()))))

(define-macro (pattern-case exp . clauses)
  ;; CLAUSE -> (else BODY)
  ;; CLAUSE -> (PAT BODY)

  (define clause-pat car)
  (define clause-body cdr)
  (define (else-clause? c) (eq? (clause-pat c) 'else))
  (define (clause-args c) 
    (if (else-clause? c)
	'() 
	(pattern-variables (clause-pat c))))
    
  (let ((exp-var (gensym))
	(body-vars (map (lambda (c) (gensym)) clauses)))
    `(let ((,exp-var ,exp)
	   ,@(map (lambda (v c)
		    `(,v (lambda ,(clause-args c) ,@(clause-body c))))
		  body-vars
		  clauses))
       (cond
	,@(map (lambda (v c)
		 (if (else-clause? c)
		     `(else
		       (,v))
		     `((pattern-match ',(clause-pat c) ,exp-var)
		       => (lambda (r) (apply ,v r)))))
	       body-vars
	       clauses)))))

(define (u32subvector vec start end)
  (let ((vec2 (make-u32vector (- end start))))
    (do ((i start (1+ i)))
	((= i end))
      (u32vector-set! vec2 i (u32vector-ref vec i)))
    vec2))

(define (pkx vec)
  (do ((i 0 (1+ i)))
      ((= i (uniform-vector-length vec)))
    (display (number->string (uniform-vector-ref vec i) 16))
    (display " "))
  (newline)
  vec)

(define (dotted-list? lst)
  (cond ((pair? lst)
	 (dotted-list? (cdr lst)))
	(else
	 (not (null? lst)))))

(define (dotted-list-copy lst)
  (cond ((pair? lst)
	 (cons (car lst) (dotted-list-copy (cdr lst))))
	((else
	  lst))))

(define (dotted-list-length lst)
  (cond ((pair? lst)
	 (1+ (dotted-list-length (cdr lst))))
	(else
	 0)))
