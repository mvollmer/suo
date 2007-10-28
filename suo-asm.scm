;;; Machine independent parts of the low-level code generator

(define primitives '())

(define (register-primitive name dispatcher)
  (set! primitives (acons name dispatcher primitives)))

;; (define-primitive (name (result ...) (arg ...) (extra-cont-label ...))
;;                   body ...)
(define-macro (define-primitive head . body)
  (let* ((name (car head))
	 (result-names (cadr head))
	 (n-results (length result-names))
	 (arg-names (caddr head))
	 (n-args (dotted-list-length arg-names))
	 (extra-cont-names (cadddr head))
	 (n-extra-conts (length extra-cont-names))
	 (args-cmp (if (dotted-list? arg-names) '>= '=))
	 (all-args (cons* 'ctxt (append extra-cont-names
					result-names
					arg-names)))
	 (asm-body `(lambda (ctxt results args extra-cont-labels)
		      (if (not (= (length extra-cont-labels) ,n-extra-conts))
			  (error "wrong number of continuations: "
				 ,n-extra-conts (length extra-cont-labels)))
		      (if (not (= (length results) ,n-results))
			  (error "wrong number of results"))
		      (if (not (,args-cmp (length args) ,n-args))
			  (error "wrong number of args"))
		      (let ((func (lambda ,all-args ,@(cdr (assq 'asm body)))))
			(apply func ctxt 
			       (append extra-cont-labels results args)))))
	 (alloc-size-body (let ((b (assq-ref body 'alloc-size)))
			    `(lambda ,arg-names
			       ,@(if b b '(0)))))
	 (dispatcher `(lambda (op)
			(case op
			  ((asm)
			   ,asm-body)
			  ((alloc-size)
			   ,alloc-size-body)
			  (else
			   (error "unsupported op: " op))))))
    `(register-primitive ',name ,dispatcher)))

;; (define-primop (name (result ...) (arg ...)) body ...)
(define-macro (define-primop head . body)
  `(define-primitive ,(append head '(())) ,@body))
  
;; (define-primif (name (arg ...) (else-label)) body ...)
(define-macro (define-primif head . body)
  `(define-primitive (,(car head) () ,@(cdr head)) ,@body))

(define (cps-primitive-asm ctxt type results args extra-cont-labels)
  (pk2 'primop type)
  (let ((dispatcher (or (assq-ref primitives type)
			(error "unsupported primitive: " type))))
    ((dispatcher 'asm) ctxt results args extra-cont-labels)))

(define (cps-primitive-alloc-size type args)
  (let ((dispatcher (or (assq-ref primitives type)
			(error "unsupported primitive: " type))))
    (apply (dispatcher 'alloc-size) args)))

(define (cps-asm-make-context debug-info)
  (let ((wydes (make-bytevec (* 4 10240)))
	(idx 0)
	(fixups '())
	(literals (list debug-info)))

    (lambda (op . args)

      (define (emit-u16 w)
	(bytevec-set-u16! wydes idx w)
	(set! idx (+ idx 1)))

      (define (fixup-s16 idx val)
	(bytevec-set-u16! wydes idx (+ (bytevec-ref-u16 wydes idx)
				       (s2val val))))

      (define (fixup! proc)
	(set! fixups (cons proc fixups)))
      
      (define (register-literal obj)
	(let ((pos (list-index literals obj)))
	  (or pos
	      (begin
		(set! literals (append literals (list obj)))
		(1- (length literals))))))

      (define (emit-s16-with-litoff w lit)
	(let ((pos idx)
	      (off (register-literal lit)))
	  (emit-u16 (s2val w))
	  (fixup! (lambda ()
		    (fixup-s16 pos (+ (* 2 idx) (* 4 (1+ off))))))))

      (define (make-label)
	(cons #f #f))

      (define (def-label label)
	(set-car! label idx))

      (define (emit-s16-with-laboff w lab)
	(let ((pos idx))
	  (emit-u16 (s2val w))
	  (fixup! (lambda ()
		    (fixup-s16 pos (* 2 (- (car lab) (1- pos))))))))

      (define (finish)
	(for-each (lambda (f) (f)) fixups)
	(code (bytevec-subvector-u16 wydes 0 idx)
	      (list->vector literals)))

      (case op
	((emit-u16)
	 (for-each emit-u16 args))
	((emit-s16-with-litoff)
	 (emit-s16-with-litoff (car args) (cadr args)))
	((emit-s16-with-laboff)
	 (emit-s16-with-laboff (car args) (cadr args)))
	((make-label)
	 (make-label))
	((def-label)
	 (def-label (car args)))
	((finish)
	 (finish))
	(else
	 (error "unsupported op" op))))))

(define (pk2 . args)
  (car (last-pair args)))

(define (cps-asm-u16 ctxt . wydes)
  (apply ctxt 'emit-u16 wydes))

(define (cps-asm-s16-with-litoff ctxt w lit)
  (ctxt 'emit-s16-with-litoff w lit))

(define (cps-asm-s16-with-laboff ctxt w label)
  (ctxt 'emit-s16-with-laboff w label))

(define (cps-asm-u32 ctxt word)
  (cps-asm-u16 ctxt (quotient word #x10000) (remainder word #x10000)))

(define cps-asm-word cps-asm-u32)

(define (cps-asm-word-with-s2-litoff ctxt word literal)
  (cps-asm-u16 ctxt (quotient word #x10000))
  (cps-asm-s16-with-litoff ctxt (remainder word #x10000) literal))

(define (cps-asm-make-label ctxt)
  (ctxt 'make-label))

(define (cps-asm-def-label ctxt label)
  (ctxt 'def-label label))

(define (cps-asm-word-with-s2-laboff ctxt word laboff)
  (cps-asm-u16 ctxt (quotient word #x10000))
  (cps-asm-s16-with-laboff ctxt (remainder word #x10000) laboff))

(define (cps-asm-finish ctxt)
  (ctxt 'finish))

(define (s2val val)
  (cond
   ((and (<= 0 val) (<= val #x7FFFF))
    val)
   ((and (<= (- #x8000) val) (<= val -1))
    (+ val #x10000))
   (else
    (error "value out of bounds" val))))

(define (object->bits obj)
  (cond
   ((fixnum? obj)
    (+ (* 4 obj) 1))
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
    #f)))

;; Put the registers listed in FROM into the registers listed in TO.
;;
;; The tricky parts are that there might be loops in the dependencies
;; between FROM and TO, and that a register might appear more than
;; once in FROM.

(define (cps-asm-shuffle-with-move ctxt from to move)

  (let ((dst-done '()))

    ;; handle-reg stores REG into its destinations and makes sure that
    ;; these destinations are used as sources (if needed) before being
    ;; overwritten.  WAITING is a list of registers that are waiting to
    ;; be used as sources and can't be stored into.  If that is needed,
    ;; handle-reg stores into the register TMP instead.  handle-reg
    ;; returns the register that has last been stored into TMP, or #f
    ;; when TMP is unused.
    ;;
    (define (handle-reg src waiting)
      (let loop ((f from)
		 (t to)
		 (s src)
		 (c #f))
	(cond ((null? f)
	       c)
	      ((eq? src (car f))
	       (let ((dst (car t)))
		 (cond ((or (eq? src dst)
			    (memq dst dst-done))
			(loop (cdr f) (cdr t) s c))
		       (else
			(cond ((memq dst waiting)
			       ;; tmp must be free
			       (assert (not c))
			       (move dst 'tmp)
			       (move s dst)
			       (set! dst-done (cons dst dst-done))
			       (loop (cdr f) (cdr t) s dst))
			      (else
			       (let ((tmp-cont
				      (handle-reg dst (cons s waiting))))
				 ;; tmp must not have been overwritten
				 (assert (or (not c) (not tmp-cont)))
				 (if (eq? tmp-cont s)
				     (move 'tmp dst)
				     (move s dst))
				 (set! dst-done (cons dst dst-done))
				 (loop (cdr f) (cdr t) dst
				       (or tmp-cont c)))))))))
	      (else
	       (loop (cdr f) (cdr t) s c)))))
 
    (for-each (lambda (src)
		(handle-reg src '()))
	      from)))

(pk 'asm)
