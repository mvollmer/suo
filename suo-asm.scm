;;;; suo-asm -- assembler

;; XXX - these instructions will be turned into primops for the
;;       compiler.
;;
;; Instructions form a graph: they can have more than one continuation
;; and a instruction can be used as a continuation in more than one
;; place.
;;
;; Operands can be either (reg N), naming register number N, or (quote
;; OBJ), referring to the literal OBJ.  A destination must name a
;; register.
;;
;; - copy OP DST CONT
;;
;; Copy OP to DST, then continue with CONT.
;;
;; - add OP1 OP2 DST GOOD BAD
;;
;; If OP1 and OP2 contain a fixnum and their sum is also a fixnum, store
;; that sum in DST and continue with GOOD.  Otherwise continue with BAD.
;;
;; - if OP THEN ELSE
;;
;; If OP is #t, continue with THEN, else continue with ELSE.
;;
;; - if-pair OP THEN ELSE
;;
;; If OP is a pair...
;;
;; - ref OP OFF DST CONT
;;
;; Load the OFFth word of OP into DST and continue with CONT.  OP must
;; be a non-immediate and the pointed to object must indeed have a
;; valid Scheme value at that offset.
;;
;; - set OP OFF OP CONT
;;
;; Set the OFFth word of OP to OP and continue with CONT.
;;
;; - go OP
;;
;; Jump to the code object in OP.
;;
;; - sys CONT
;;
;; Perform a syscall, then continue with CONT.  A syscall expects its
;; arguments in fixed registers. (reg 0) determines the function to be
;; performed, etc.  TBD.
;;
;;
;; Typical instruction sequences:
;;
;; (if-pair (reg 1) 
;;          (ref (reg 1) (quote 0) (reg 2) ...)
;;          (go (quote not-a-pair)))

(define (pkx vec)
  (do ((i 0 (1+ i)))
      ((= i (uniform-vector-length vec)))
    (display (number->string (uniform-vector-ref vec i) 16))
    (display " "))
  (newline)
  vec)

(define (u8vector-ref-u32 uvec idx)
  ;; little endian
  (+ (u8vector-ref uvec idx)
     (* 256 (+ (u8vector-ref uvec (+ idx 1))
	       (* 256 (+ (u8vector-ref uvec (+ idx 2))
			 (* 256 (u8vector-ref uvec (+ idx 3)))))))))

(define (u8vector-set-u32! uvec idx w)
  ;; little endian
  (u8vector-set! uvec idx (remainder w 256))
  (u8vector-set! uvec (+ idx 1) (remainder (quotient w 256) 256))
  (u8vector-set! uvec (+ idx 2) (remainder (quotient w (* 256 256)) 256))
  (u8vector-set! uvec (+ idx 3) (remainder (quotient w (* 256 256 256)) 256)))

(define (u8vector->u32vector u8 len)
  (let ((u32 (make-u32vector (/ len 4))))
    (do ((i 0 (1+ i)))
	((= i (u32vector-length u32)))
      (u32vector-set! u32 i (u8vector-ref-u32 u8 (* i 4))))
    u32))

(define (assemble-instructions ins)

  (let ((bytes (make-u8vector 32 0))
	(idx 0)
	(fixups '())
	(literals '())
	(labels (make-hash-table 31)))

    (define (emit-byte b)
      (u8vector-set! bytes idx b)
      (set! idx (1+ idx)))

    (define (emit-bytes . bs)
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

    (define (reg? op)
      (eq? (car op) 'reg))

    (define (regoff op)
      (* 4 (cadr op)))

    (define (lit? op)
      (eq? (car op) 'quote))

    (define (litoff op)
      (* 4 (register-literal (cadr op))))

    (define (emit-litoff op)
      (let ((pos idx))
	(emit-word (litoff op))
	(fixup! (lambda ()
		  (fixup-word pos (+ 4 idx))))))

    (define (emit-op-to-eax op)
      (cond ((reg? op)
	     ;; mov off(%ebp),%eax
	     (let ((off (regoff op)))
	       (cond
		((<= -128 off -1)
		 (emit-bytes #x8b #x45 (+ off 256)))
		((<= 0 off 127)
		 (emit-bytes #x8b #x45 off))
		(else
		 (error "register out of bounds" op)))))
	    (else
	     ;; mov off(%esi),%eax
	     (emit-bytes #x8b #x86)
	     (emit-litoff op))))
    
    (define (emit-op-to-esi op)
      (cond ((reg? op)
	     ;; mov off(%ebp),%esi
	     (let ((off (regoff op)))
	       (cond
		((<= -128 off -1)
		 (emit-bytes #x8b #x75 (+ off 256)))
		((<= 0 off 127)
		 (emit-bytes #x8b #x75 off))
		(else
		 (error "register out of bounds" op)))))
	    (else
	     ;; mov off(%esi),%esi
	     (emit-bytes #x8b #xb6)
	     (emit-litoff op))))

    (define (emit-eax-to-reg op)
      (or (reg? op)
	  (error "must be reg" op))
      (emit-bytes #x89 #x45 (regoff op)))

    (define (asm-copy op1 op2 cont)
      (emit-op-to-eax op1)
      (emit-eax-to-reg op2)
      (asm cont))

    (define (asm-sys op cont)
      (emit-op-to-eax op)
      ;; push %eax
      (emit-bytes #x50)
      (emit-op-to-eax '(reg -1))
      ;; call *eax
      (emit-bytes #xff #xd0)
      ;; add $4,%esp
      (emit-bytes #x83 #xc4 #x04)
      (asm cont))

    (define (asm-go op)
      (emit-op-to-esi op)
      ;; lea 4(%esi),%eax
      (emit-bytes #x8d #x46 #x04)
      ;; jmp *%eax
      (emit-bytes #xff #xe0))

    (define (asm ins)
      (let ((pos (hashq-ref labels ins)))
	(if pos
	    (begin
	      ;; lea lab(%esi),%eax
	      (emit-bytes #x8d #x46 pos)
	      ;; jmp *%eax
	      (emit-bytes #xff #xe0))
	    (let ((pos (* 4 idx)))
	      (hashq-set! labels ins pos)
	      (case (car ins)
		((copy)
		 (apply asm-copy (cdr ins)))
		((sys)
		 (apply asm-sys (cdr ins)))
		((go)
		 (apply asm-go (cdr ins)))
		(else
		 (error "unrecognized instruction" (car ins))))))))

    (define (finish)
      (set! idx (* 4 (quotient (+ idx 3) 4)))
      (for-each (lambda (f) (f)) fixups)
      (pkx bytes)
      (suo:code (pkx (u8vector->u32vector bytes idx))
		(list->vector literals)))

    (asm ins)
    (finish)))
