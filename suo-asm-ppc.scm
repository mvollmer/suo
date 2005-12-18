;;; Code generation and assembling, PowerPC version

(define primitives '())

(define (register-primitive name func)
  (set! primitives (acons name func primitives)))

;; (define-primop (name (result ...) (arg ...)) body ...)
(define-macro (define-primop head . body)
  (let* ((name (car head))
	 (result-names (list-copy (cadr head)))
	 (n-results (length result-names))
	 (arg-names (caddr head))
	 (n-args (dotted-list-length arg-names))
	 (args-cmp (if (dotted-list? arg-names) '>= '=))
	 (all-args (cons 'ctxt (append! result-names arg-names))))
    `(let ((func (lambda ,all-args ,@body)))
       (register-primitive
	',name
	(lambda (ctxt results args extra-cont-labels)
	  (if (not (= (length results) ,n-results))
	      (error "wrong number of results"))
	  (if (not (,args-cmp (length args) ,n-args))
	      (error "wrong number of args"))
	  (if (not (null? extra-cont-labels))
	      (error "wrong number of continuations"))
	  (apply func ctxt (append results args)))))))

;; (define-primif (name (arg ...) (else-label)) body ...)
(define-macro (define-primif head . body)
  (let* ((name (car head))
	 (else-name (car (caddr head)))
	 (arg-names (cadr head))
	 (n-args (dotted-list-length arg-names))
	 (args-cmp (if (dotted-list? arg-names) '>= '=))
	 (all-args (cons* 'ctxt else-name arg-names)))
    `(let ((func (lambda ,all-args ,@body)))
       (register-primitive
	',name
	(lambda (ctxt results args extra-cont-labels)
	  (if (not (zero? (length results)))
	      (error "wrong number of results"))
	  (if (not (,args-cmp (length args) ,n-args))
	      (error "wrong number of args"))
	  (if (not (= (length extra-cont-labels) 1))
	      (error "wrong number of continuations"))
	  (apply func ctxt (append extra-cont-labels args)))))))

(define (cps-asm-primop ctxt type results args extra-cont-labels)
  (let ((func (or (assq-ref primitives type)
		  (error "unsupported" type))))
    (func ctxt results args extra-cont-labels)))

(define (cps-asm-make-context)
  (let ((words (make-u32vector 1024 0))
	(idx 0)
	(fixups '())
	(literals '()))

    (lambda (op . args)

      (define (emit-word w)
	(u32vector-set! words idx w)
	(set! idx (+ idx 1)))
    
      (define (fixup-low-s2 idx val)
	(u32vector-set! words idx (+ (u32vector-ref words idx)
				     (s2val val))))

      (define (fixup! proc)
	(set! fixups (cons proc fixups)))
      
      (define (register-literal obj)
	(let ((pos (list-index literals obj)))
	  (or pos
	      (begin
		(set! literals (append literals (list obj)))
		(1- (length literals))))))

      (define (emit-word-with-s2-litoff word lit)
	(let ((pos idx)
	      (off (register-literal lit)))
	  (emit-word word)
	  (fixup! (lambda ()
		    (fixup-low-s2 pos (* 4 (+ 1 idx off)))))))

      (define (make-label)
	(cons #f #f))

      (define (def-label label)
	(set-car! label idx))

      (define (emit-word-with-s2-laboff word lab)
	(let ((pos idx))
	  (emit-word word)
	  (fixup! (lambda ()
		    (fixup-low-s2 pos (* 4 (- (car lab) pos)))))))

      (define (finish)
	(for-each (lambda (f) (f)) fixups)
	(suo:code (u32subvector words 0 idx)
		  (list->vector literals)))

      (case op
	((emit-word)
	 (emit-word (car args)))
	((emit-word-with-s2-litoff)
	 (emit-word-with-s2-litoff (car args) (cadr args)))
	((emit-word-with-s2-laboff)
	 (emit-word-with-s2-laboff (car args) (cadr args)))
	((make-label)
	 (make-label))
	((def-label)
	 (def-label (car args)))
	((finish)
	 (finish))
	(else
	 (error "unsupported op" op))))))

(define (cps-asm-word ctxt word)
  (ctxt 'emit-word word))

(define (cps-asm-word-with-s2-litoff ctxt word literal)
  (ctxt 'emit-word-with-s2-litoff word literal))

(define (cps-asm-make-label ctxt)
  (ctxt 'make-label))

(define (cps-asm-def-label ctxt label)
  (ctxt 'def-label label))

(define (cps-asm-word-with-s2-laboff ctxt word label)
  (ctxt 'emit-word-with-s2-laboff word label))

(define (cps-asm-finish ctxt)
  (ctxt 'finish))

(define (s2val val)
  (cond
   ((<= 0 val #x7FFFF)
    val)
   ((<= (- #x8000) val -1)
    (+ val #x10000))
   (else
    (error "value out of bounds" val))))

(define (reg-off reg)
  (s2val (* 4 (cps-reg-idx reg))))

(define (cps-asm-op-to-r3 ctxt op)
  (cond ((cps-reg? op)
	 ;; lwz r3,regoff(r14)
	 (cps-asm-word ctxt (+ #x806e0000 (reg-off op))))
	(else
	 ;; lwz r3,litoff(r15)
	 (cps-asm-word-with-s2-litoff ctxt #x806f0000
					 (cps-quote-value op)))))

(define (cps-asm-r3-to-reg ctxt reg)
  ;; stw r3,off(r14)
  (cps-asm-word ctxt (+ #x906e0000 (reg-off reg))))

(define (cps-asm-ref-r3-to-r3 ctxt off)
  ;; lwz r3,off(r3)
  (cps-asm-word ctxt (+ #x80630000 (s2val (* 4 off)))))

(define (cps-asm-swap-r3-and-reg ctxt reg)
  ;; mr r4,r3
  (cps-asm-word ctxt #x7c641b78)
  ;; ...
  (cps-asm-op-to-r3 ctxt reg)
  ;; stw r4,regoff(r14)
  (cps-asm-word ctxt (+ #x908e0000 (reg-off reg))))

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
  (let ((done '()))
    (define (do-one idx)
      ;; from[idx] is in r0.  Swap to[idx] and r0.
      (set! done (cons idx done))
      (cps-asm-swap-r3-and-reg ctxt (list-ref to idx)))
    (define (do-cycle idx)
      (cps-asm-op-to-r3 ctxt (list-ref from idx))
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
  (cps-asm-op-to-r3 ctxt to)
  ;; mr r15,r3
  (cps-asm-word ctxt #x7c6f1b78)
  ;; addi r3,r3,4
  (cps-asm-word ctxt #x38630004)
  ;; mtctr r3
  (cps-asm-word ctxt #x7c6903a6)
  ;; bctr
  (cps-asm-word ctxt #x4e800420))

(define-primop (syscall (res) args)
  (for-each (lambda (a r)
	      (cps-asm-op-to-r3 ctxt a)
	      ;; mr rR,r3
	      (cps-asm-word ctxt (+ #x7c000378
				    (* 3 (expt 2 11))
				    (* r (expt 2 16))
				    (* 3 (expt 2 21)))))
	    args (list-head '(4 5 6 7 8 9 10)
			    (length args)))
  (cps-asm-op-to-r3 ctxt (cps-reg -1))
  ;; mtctr r3
  (cps-asm-word ctxt #x7c6903a6)
  ;; li r3,nargs
  (cps-asm-word ctxt (+ #x38600000 (s2val (length args))))
  ;; bctrl
  (cps-asm-word ctxt #x4e800421)
  (cps-asm-r3-to-reg ctxt res))

(define (cps-asm-alloc-to-r4 ctxt words)
  ;; mr r4,r16
  (cps-asm-word ctxt #x7e048378)
  ;; addi r16,r16,off
  (cps-asm-word ctxt (+ #x3a100000 (s2val (* 4 words)))))

(define (cps-asm-store-r3-to-r4 ctxt idx)
  ;; stw r3,off(r4)
  (cps-asm-word ctxt (+ #x90640000 (s2val (* 4 idx)))))

(define-primop (record (res) (desc . values))
  (cps-asm-alloc-to-r4 ctxt (1+ (length values)))
  (cps-asm-op-to-r3 ctxt desc)
  ;; ori r3,r3,3
  (cps-asm-word ctxt #x60630003)
  (cps-asm-store-r3-to-r4 ctxt 0)
  (for-each (lambda (v i)
	      (cps-asm-op-to-r3 ctxt v)
	      (cps-asm-store-r3-to-r4 ctxt (1+ i)))
	    values (iota (length values)))
  ;; mr r3,r4
  (cps-asm-word ctxt #x7c832378)
  (cps-asm-r3-to-reg ctxt res))

(define-primop (record-ref (res) (rec idx))
  (cond ((cps-quote? idx)
	 (cps-asm-op-to-r3 ctxt rec)
	 (cps-asm-ref-r3-to-r3 ctxt (1+ (cps-quote-value idx)))
	 (cps-asm-r3-to-reg ctxt res))
	(else
	 (error "no computed indices yet, sorry"))))

(define-primop (record-set () (rec idx val))
  (cond ((cps-quote? idx)
	 (cps-asm-op-to-r3 ctxt rec)
	 ;; mr r4,r3
	 (cps-asm-word ctxt #x7c641b78)
	 (cps-asm-op-to-r3 ctxt val)
	 (cps-asm-store-r3-to-r4 ctxt (1+ (cps-quote-value idx))))
	(else
	 (error "no computed indices yet, sorry"))))

(define (cps-asm-word-to-r3 ctxt word)
  ;; lis r3,upper
  (cps-asm-word ctxt (+ #x3c600000 (quotient word #x10000)))
  ;; addi r3,r3,lower
  (cps-asm-word ctxt (+ #x38630000 (remainder word #x10000))))

(define-primop (vector (res) values)
  (cps-asm-alloc-to-r4 ctxt (1+ (length values)))
  (cps-asm-word-to-r3 ctxt (+ #x80000000 (* 16 (length values)) 3))
  (cps-asm-store-r3-to-r4 ctxt 0)
  (for-each (lambda (v i)
	      (cps-asm-op-to-r3 ctxt v)
	      (cps-asm-store-r3-to-r4 ctxt (1+ i)))
	    values (iota (length values)))
  ;; mr r3,r4
  (cps-asm-word ctxt #x7c832378)
  (cps-asm-r3-to-reg ctxt res))

(define-primop (vector-ref (res) (vec idx))
  (cond ((cps-quote? idx)
	 (cps-asm-op-to-r3 ctxt vec)
	 (cps-asm-ref-r3-to-r3 ctxt (1+ (cps-quote-value idx)))
	 (cps-asm-r3-to-reg ctxt res))
	(else
	 (error "no computed indices yet, sorry"))))

(define-primif (if-eq? (v1 v2) (else-label))
  (cps-asm-op-to-r3 ctxt v1)
  ;; mr r4,r3
  (cps-asm-word ctxt #x7c641b78)
  (cps-asm-op-to-r3 ctxt v2)
  ;; cmpw cr7,r3,r4
  (cps-asm-word ctxt #x7f832000)
  ;; bne cr7,lab
  (cps-asm-word-with-s2-laboff ctxt #x409e0000 else-label))

(define-primif (if-fixnum? (v) (else-label))
  (cps-asm-op-to-r3 ctxt v)
  ;; andi r3,r3,3
  (cps-asm-word ctxt #x70630003)
  ;; cmpwi cr7,r3,1
  (cps-asm-word ctxt #x2f830001)
  ;; bne cr7,lab
  (cps-asm-word-with-s2-laboff ctxt #x409e0000 else-label))
  
(define-primop (add-fixnum (res) (arg1 arg2))
  (cps-asm-op-to-r3 ctxt arg1)
  ;; mr r4,r3
  (cps-asm-word ctxt #x7c641b78)
  (cps-asm-op-to-r3 ctxt arg2)
  ;; subi r3,r3,1
  (cps-asm-word ctxt #x3863ffff)
  ;; addo. r3,r3,r4
  (cps-asm-word ctxt #x7c632615)
  (let ((out (cps-asm-make-label ctxt)))
    ;; bns out
    (cps-asm-word-with-s2-laboff ctxt #x40830000 out)
    (cps-asm-op-to-r3 ctxt (cps-quote #f))
    (cps-asm-def-label ctxt out))
  (cps-asm-r3-to-reg ctxt res))

(define-primop (sub-fixnum (res) (arg1 arg2))
  (cps-asm-op-to-r3 ctxt arg1)
  ;; mr r4,r3
  (cps-asm-word ctxt #x7c641b78)
  (cps-asm-op-to-r3 ctxt arg2)
  ;; subi r3,r3,1
  (cps-asm-word ctxt #x3863ffff)
  ;; sub r3,r4,r3
  (cps-asm-word ctxt #x7c632050)
  (cps-asm-r3-to-reg ctxt res))
