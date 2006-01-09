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
