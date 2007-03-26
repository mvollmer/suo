(define (pp val)
  (pretty-print val)
  val)

;;; Record definers

;; (define-record NAME FIELD...) defines a new record type and
;; associated procedures.
;;
;; (NAME VALUE...) is a constructor and returns a new object with
;; fields initialized according to the arguments of the constructor.
;; VALUE... is a list with one member for each field of the record.
;;
;; (NAME? obj) is the predicate.
;;
;; (NAME-FIELD obj) is a getter for FIELD.
;;
;; (set-NAME-FIELD! obj val) is a setter for FIELD.
;;
;; NAME/type is the type of the record.

(define-macro (define-record name . fields)
  (let* ((type-name (symbol-append name '/type))
	 (pred-name (symbol-append name '?)))
    `(begin
       (define ,type-name (make-record-type ,(length fields) ',name #f))
       (define (,pred-name x) (record-is-a? x ,type-name))
       (define (,name ,@fields) (record ,type-name ,@fields))
       ,@(map (lambda (f i)
		`(begin
		   (define (,(symbol-append name '- f) x)
		     (if (,pred-name x)
			 (record-ref x ,i)
			 (error:wrong-type x)))
		   (define (,(symbol-append 'set- name '- f '!) x y)
		     (if (,pred-name x y)
			 (record-set! x ,i y)
			 (error:wrong-type x)))))
	      fields (iota (length fields))))))

(define-macro (record-case exp . clauses)
  ;; clause -> ((NAME FIELD...) BODY...)
  (let ((tmp (gensym)))
    (define (record-clause->cond-clause clause)
      (let ((pattern (car clause)))
	(if (eq? pattern 'else)
	    clause
	    (let* ((name (car pattern))
		   (args (cdr pattern))
		   (body (cdr clause)))
	      `((,(symbol-append name '?) ,tmp)
		((lambda ,args ,@body)
		 ,@(map (lambda (i)
			  `(record-ref ,tmp ,i))
			(iota (length args)))))))))
    `(let ((,tmp ,exp))
       (cond ,@(map record-clause->cond-clause clauses)
	     (else (error "unsupported record instance" ,tmp))))))

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
