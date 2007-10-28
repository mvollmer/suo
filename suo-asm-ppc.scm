;;; Code generation and assembling, PowerPC version

(define (reg-off reg)
  (s2val (* 4 (cps-reg-idx reg))))

(define (cps-asm-op-to-r3 ctxt op)
  (cond ((cps-reg? op)
	 ;; lwz r3,regoff(r14)
	 (cps-asm-u16 ctxt #x806e (reg-off op)))
	(else
	 (let ((bits (object->bits (cps-quote-value op))))
	   (cond ((and bits (<= -32767 bits) (< bits 32768))
		  ;; li r3,lit
		  (cps-asm-u16 ctxt #x3860)
		  (cps-asm-u16 ctxt (s2val bits)))
		 (else
		  ;; lwz r3,litoff(r15)
		  (cps-asm-u16 ctxt #x806f)
		  (cps-asm-s16-with-litoff ctxt 0 (cps-quote-value op))))))))

(define (cps-asm-op-to-r4 ctxt op)
  (cond ((cps-reg? op)
	 ;; lwz r4,regoff(r14)
	 (cps-asm-u16 ctxt #x808e (reg-off op)))
	(else
	 ;; lwz r4,litoff(r15)
	 (cps-asm-u16 ctxt #x808f)
	 (cps-asm-s16-with-litoff ctxt 0 (cps-quote-value op)))))

(define (cps-asm-r3-to-reg ctxt reg)
  ;; stw r3,off(r14)
  (cps-asm-u16 ctxt #x906e (reg-off reg)))

(define (cps-asm-ref-r3-to-r3 ctxt off)
  ;; lwz r3,off(r3)
  (cps-asm-u16 ctxt #x8063 (s2val (* 4 off))))

(declare-variables cps-verbose)

(define (cps-asm-shuffle ctxt from to)

  (define (move src dst)
    (if cps-verbose
	(pk (cps-render src) '-> (cps-render dst)))
    (if (eq? src 'tmp)
	(cps-asm-move-rX-to-rY ctxt 4 3)
	(cps-asm-op-to-r3 ctxt src))
    (if (eq? dst 'tmp)
	(cps-asm-move-rX-to-rY ctxt 3 4)
	(cps-asm-r3-to-reg ctxt dst)))
  
  (cps-asm-shuffle-with-move ctxt from to move))

(define (cps-asm-go ctxt to)
  (cps-asm-op-to-r3 ctxt to)
  ;; mr r15,r3
  (cps-asm-u16 ctxt #x7c6f #x1b78)
  ;; addi r3,r3,4
  (cps-asm-u16 ctxt #x3863 #x0004)
  ;; mtctr r3
  (cps-asm-u16 ctxt #x7c69 #x03a6)
  ;; bctr
  (cps-asm-u16 ctxt #x4e80 #x0420))

(define (cps-asm-move-rX-to-rY ctxt x y)
  ;; mr rY,rX
  (cps-asm-u32 ctxt (+ #x7c000378
		       (* x (expt 2 11))
		       (* y (expt 2 16))
		       (* x (expt 2 21)))))

(define (cps-asm-dump-regs ctxt)
  (cps-asm-op-to-r4 ctxt (cps-quote 11))
  (cps-asm-op-to-r3 ctxt (cps-reg -1))
  ;; mtctr r3
  (cps-asm-u16 ctxt #x7c69 #x03a6)
  ;; li r3,nargs
  (cps-asm-u16 ctxt #x3860 (s2val 1))
  ;; bctrl
  (cps-asm-u16 ctxt #x4e80 #x0421))

(define (cps-asm-prologue ctxt sig alloc-size)
  (let ((lab (cps-asm-make-label ctxt)))
    (cps-asm-op-to-r3 ctxt (cps-quote sig))
    (cps-asm-op-to-r4 ctxt (cps-reg 0))
    ;; cmpw cr7,r3,r4
    (cps-asm-u16 ctxt #x7f83 #x2000)
    ;; beq cr7,lab
    (cps-asm-u16 ctxt #x419e)
    (cps-asm-s16-with-laboff ctxt 0 lab)
    ;; call adjust_call_sig
    (cps-asm-r3-to-reg ctxt (cps-reg -5))
    (cps-asm-op-to-r3 ctxt (cps-reg -4))
    ;; mtctr r3
    (cps-asm-u16 ctxt #x7c69 #x03a6)
    ;; bctrl
    (cps-asm-u16 ctxt #x4e80 #x0421)
    (cps-asm-def-label ctxt lab)
    (cps-asm-alloc-check ctxt alloc-size #t)))

(define-primop (syscall (res) args)
  (asm
   (for-each (lambda (a r)
	       (cps-asm-op-to-r3 ctxt a)
	       (cps-asm-move-rX-to-rY ctxt 3 r))
	     args (list-head '(4 5 6 7 8 9 10)
			     (length args)))
   (cps-asm-op-to-r3 ctxt (cps-reg -1))
   ;; mtctr r3
   (cps-asm-u16 ctxt #x7c69 #x03a6)
   ;; li r3,nargs
   (cps-asm-u16 ctxt #x3860 (s2val (length args)))
   ;; bctrl
   (cps-asm-u16 ctxt #x4e80 #x0421)
   (cps-asm-r3-to-reg ctxt res)))

(define (cps-asm-panic ctxt)
  (cps-asm-op-to-r3 ctxt (cps-reg -1))
  ;; mtctr r3
  (cps-asm-u16 ctxt #x7c69 #x03a6)
  ;; li r3,0
  (cps-asm-u16 ctxt #x3860 #x0000)
  ;; bctrl
  (cps-asm-u16 ctxt #x4e80 #x0421))
  
(define-primop (get-reg (res) (idx))
  (asm
   (cps-asm-op-to-r4 ctxt idx)
   ;;   c:   7c 8e 22 14     add     r4,r14,r4
   ;;  10:   80 64 ff ff     lwz     r3,-1(r4)
   (cps-asm-u16 ctxt #x7c8e #x2214)
   (cps-asm-u16 ctxt #x8064 #xffff)
   (cps-asm-r3-to-reg ctxt res)))
  
(define-primop (set-reg (res) (idx val))
  (asm
   (cps-asm-op-to-r4 ctxt idx)
   (cps-asm-op-to-r3 ctxt val)
   ;;   c:   7c 8e 22 14     add     r4,r14,r4
   ;;  10:   90 64 ff ff     stw     r3,-1(r4)
   (cps-asm-u16 ctxt #x7c8e #x2214)
   (cps-asm-u16 ctxt #x9064 #xffff)
   (cps-asm-r3-to-reg ctxt res)))
  
(define (cps-asm-alloc-check ctxt size always?)
  (if (or (> size 0) always?)
      (let ((lab (cps-asm-make-label ctxt)))
	;; addi r4,r16,off
	(cps-asm-u16 ctxt #x3890 (s2val (* 4 size)))
	;; cmpw cr7,r4,r17
	(cps-asm-u16 ctxt #x7f84 #x8800)
	;; blt cr7,lab
	(cps-asm-u16 ctxt #x419c)
	(cps-asm-s16-with-laboff ctxt 0 lab)
	;; branch to gc_glue, which knows about our register setup
	(cps-asm-op-to-r3 ctxt (cps-reg -2))
	;; mtctr r3
	(cps-asm-u16 ctxt #x7c69 #x03a6)
	;; bctrl
	(cps-asm-u16 ctxt #x4e80 #x0421)
	(cps-asm-def-label ctxt lab))))

(define (cps-asm-alloc-to-r4 ctxt words)
  ;; mr r4,r16
  (cps-asm-u16 ctxt #x7e04 #x8378)
  ;; addi r16,r16,off
  (cps-asm-u16 ctxt #x3a10 (* 4 words)))

(define (cps-asm-alloc-r3-bytes-to-r4 ctxt)
  (let ((lab (cps-asm-make-label ctxt)))
    ;; add r4,r16,r3
    (cps-asm-u16 ctxt #x7c90 #x1a14)
    ;; cmpw cr7,r4,r17
    (cps-asm-u16 ctxt #x7f84 #x8800)
    ;; blt cr7,lab
    (cps-asm-u16 ctxt #x419c)
    (cps-asm-s16-with-laboff ctxt 0 lab)
    ;; branch to gc_glue, which knows about our register setup
    (cps-asm-op-to-r3 ctxt (cps-reg -2))
    ;; mtctr r3
    (cps-asm-u16 ctxt #x7c69 #x03a6)
    ;; bctrl
    (cps-asm-u16 ctxt #x4e80 #x0421)
    (cps-asm-def-label ctxt lab)
    ;; mr r0,r16
    (cps-asm-u16 ctxt #x7e00 #x8378)
    ;; mr r16,r4
    (cps-asm-u16 ctxt #x7c90 #x2378)
    ;; mr r4,r0
    (cps-asm-u16 ctxt #x7c04 #x0378)))

(define (cps-asm-store-r3-to-r4 ctxt idx)
  ;; stw r3,off(r4)
  (cps-asm-u16 ctxt #x9064 (s2val (* 4 idx))))

(define-primop (record (res) (desc . values))
  (alloc-size
   (1+ (length values)))
  (asm
   (cps-asm-alloc-to-r4 ctxt (1+ (length values)))
   (cps-asm-op-to-r3 ctxt desc)
   ;; ori r3,r3,3
   (cps-asm-u16 ctxt #x6063 #x0003)
   (cps-asm-store-r3-to-r4 ctxt 0)
   (for-each (lambda (v i)
	       (cps-asm-op-to-r3 ctxt v)
	       (cps-asm-store-r3-to-r4 ctxt (1+ i)))
	     values (iota (length values)))
   ;; mr r3,r4
   (cps-asm-u16 ctxt #x7c83 #x2378)
   (cps-asm-r3-to-reg ctxt res)))

(define-primop (make-record (res) (desc n-fields init))
  (alloc-size
   #t)
  (asm
   (cps-asm-op-to-r3 ctxt n-fields)
   ;; rlwinm  r3,r3,0,0,29  (r3 = r3 & ~3)
   (cps-asm-u16 ctxt #x5463 #x003a)
   ;; addi r3,r3,4
   (cps-asm-u16 ctxt #x3863 #x0004)
   (cps-asm-alloc-r3-bytes-to-r4 ctxt)
   ;; mr r3,r4
   (cps-asm-u16 ctxt #x7c83 #x2378)
   (cps-asm-r3-to-reg ctxt res)
   (cps-asm-op-to-r3 ctxt desc)
   ;; ori r3,r3,3
   (cps-asm-u16 ctxt #x6063 #x0003)
   (cps-asm-store-r3-to-r4 ctxt 0)
   (cps-asm-op-to-r3 ctxt init)
   ;; mr r0,r3
   (cps-asm-move-rX-to-rY ctxt 3 0)
   (cps-asm-op-to-r3 ctxt n-fields)
   (let ((out (cps-asm-make-label ctxt))
	 (loop (cps-asm-make-label ctxt)))
     (cps-asm-def-label ctxt loop)
     ;; addic. r3,r3,-4
     (cps-asm-u16 ctxt #x3463 #xfffc)
     ;; ble out
     (cps-asm-u16 ctxt #x4081)
     (cps-asm-s16-with-laboff ctxt 0 out)
     ;; stwu r0,4(r4)
     (cps-asm-u16 ctxt #x9404 #x0004)
     ;; XXX - b loop
     (cps-asm-word ctxt #x4bfffff4)
     (cps-asm-def-label ctxt out))))

(define-primif (if-record? (obj desc) (else-label))
  (asm
   (cps-asm-op-to-r3 ctxt obj)
   ;; andi r3,r3,3
   (cps-asm-u16 ctxt #x7063 #x0003)
   ;; cmpwi cr7,r3,0
   (cps-asm-u16 ctxt #x2f83 #x0000)
   ;; bne cr7,lab
   (cps-asm-u16 ctxt #x409e)
   (cps-asm-s16-with-laboff ctxt 0 else-label)
   (cps-asm-op-to-r3 ctxt obj)
   (cps-asm-ref-r3-to-r3 ctxt 0)
   (if (and (cps-quote? desc)
	    (eq? (cps-quote-value desc) #t))
       (begin
	 ;; any descriptor is ok
	 ;; cmpwi cr7,r3,0
	 (cps-asm-u16 ctxt #x2f83 #x0000)
	 ;; blt cr7,lab
	 (cps-asm-u16 ctxt #x419c)
	 (cps-asm-s16-with-laboff ctxt 0 else-label)
	 ;; andi r3,r3,3
	 (cps-asm-u16 ctxt #x7063 #x0003)
	 ;; cmpwi cr7,r3,3
	 (cps-asm-u16 ctxt #x2f83 #x0003)
	 ;; bne cr7,lab
	 (cps-asm-u16 ctxt #x409e)
	 (cps-asm-s16-with-laboff ctxt 0 else-label))
       (begin
	 ;; must have a specific descriptor
	 ;; mr r4,r3
	 (cps-asm-u16 ctxt #x7c64 #x1b78)
	 (cps-asm-op-to-r3 ctxt desc)
	 ;; ori r3,r3,3
	 (cps-asm-u16 ctxt #x6063 #x0003)
	 ;; cmpw cr7,r3,r4
	 (cps-asm-u16 ctxt #x7f83 #x2000)
	 ;; bne cr7,lab
	 (cps-asm-u16 ctxt #x409e)
	 (cps-asm-s16-with-laboff ctxt 0 else-label)))))

(define-primop (record-desc (res) (rec))
  (asm
   (cps-asm-op-to-r3 ctxt rec)
   (cps-asm-ref-r3-to-r3 ctxt 0)
   ;; rlwinm  r3,r3,0,0,29  (r3 = r3 & ~3)
   (cps-asm-u16 ctxt #x5463 #x003a)
   (cps-asm-r3-to-reg ctxt res)))

(define-primop (record-ref (res) (rec idx))
  (asm
   (cond ((cps-quote? idx)
	  (cps-asm-op-to-r3 ctxt rec)
	  ;;   8:   88 63 00 04     lwz     r3,idx(r3)
	  (cps-asm-u16 ctxt #x8063 (* 4 (1+ (cps-quote-value idx))))
	  (cps-asm-r3-to-reg ctxt res))
	 (else
	  (cps-asm-op-to-r4 ctxt rec)
	  (cps-asm-op-to-r3 ctxt idx)
	  ;;   4:   7c 63 22 14     add     r3,r3,r4
	  (cps-asm-u16 ctxt #x7c63 #x2214)
	  ;;   8:   88 63 00 04     lwz     r3,3(r3)
	  (cps-asm-u16 ctxt #x8063 #x0003)
	  (cps-asm-r3-to-reg ctxt res)))))

(define-primop (record-set (res) (rec idx val))
  (asm
   (cps-asm-op-to-r4 ctxt rec)
   (cps-asm-op-to-r3 ctxt idx)
   ;;   4:   7c 63 22 14     add     r4,r3,r4
   (cps-asm-u16 ctxt #x7c83 #x2214)
   (cps-asm-op-to-r3 ctxt val)
   ;; stw r3,3(r4)
   (cps-asm-u16 ctxt #x9064 #x0003)
   (cps-asm-op-to-r3 ctxt (cps-quote (if #f #f)))
   (cps-asm-r3-to-reg ctxt res)))
  
(define (cps-asm-word-to-r3 ctxt word)
  ;; lis r3,upper
  (cps-asm-u16 ctxt #x3c60 (quotient word #x10000))
  ;; addi r3,r3,lower
  (cps-asm-u16 ctxt #x3863 (remainder word #x10000)))

(define-primif (if-vector? (a) (else-label))
  (asm
   (cps-asm-op-to-r3 ctxt a)
   ;; andi r3,r3,3
   (cps-asm-u16 ctxt #x7063 #x0003)
   ;; cmpwi cr7,r3,0
   (cps-asm-u16 ctxt #x2f83 #x0000)
   ;; bne cr7,lab
   (cps-asm-word-with-s2-laboff ctxt #x409e0000 else-label)
   (cps-asm-op-to-r3 ctxt a)
   (cps-asm-ref-r3-to-r3 ctxt 0)
   ;; cmpwi cr7,r3,0
   (cps-asm-u16 ctxt #x2f83 #x0000)
   ;; bge cr7,lab
   (cps-asm-word-with-s2-laboff ctxt #x409c0000 else-label)
   ;; andi r3,r3,15
   (cps-asm-u16 ctxt #x7063 #x000f)
   ;; cmpwi cr7,r3,3
   (cps-asm-u16 ctxt #x2f83 #x0003)
   ;; bne cr7,lab
   (cps-asm-word-with-s2-laboff ctxt #x409e0000 else-label)))

(define-primop (vector (res) values)
  (alloc-size
   (1+ (length values)))
  (asm
   (cps-asm-alloc-to-r4 ctxt (1+ (length values)))
   (cps-asm-word-to-r3 ctxt (+ #x80000000 (* 16 (length values)) 3))
   (cps-asm-store-r3-to-r4 ctxt 0)
   (for-each (lambda (v i)
	       (cps-asm-op-to-r3 ctxt v)
	       (cps-asm-store-r3-to-r4 ctxt (1+ i)))
	     values (iota (length values)))
   ;; mr r3,r4
   (cps-asm-u16 ctxt #x7c83 #x2378)
   (cps-asm-r3-to-reg ctxt res)))

(define-primop (make-vector (res) (n init))
  (alloc-size
   #t)
  (asm
   (cps-asm-op-to-r3 ctxt n)
   ;; rlwinm  r3,r3,0,0,29  (r3 = r3 & ~3)
   (cps-asm-u16 ctxt #x5463 #x003a)
   ;; addi r3,r3,4
   (cps-asm-u16 ctxt #x3863 #x0004)
   (cps-asm-alloc-r3-bytes-to-r4 ctxt)
   ;; mr r3,r4
   (cps-asm-u16 ctxt #x7c83 #x2378)
   (cps-asm-r3-to-reg ctxt res)
   (cps-asm-op-to-r3 ctxt n)
   ;;    0:   3c 00 80 00     lis     r0,-32768
   (cps-asm-u16 ctxt #x3c00 #x8000)
   ;;    4:   54 63 10 36     rlwinm  r3,r3,2,0,27
   (cps-asm-u16 ctxt #x5463 #x1036)
   ;;    8:   60 00 00 03     ori     r0,r0,3
   (cps-asm-u16 ctxt #x6000 #x0003)
   ;;    c:   7c 63 03 78     or      r3,r3,r0
   (cps-asm-u16 ctxt #x7c63 #x0378)
   (cps-asm-store-r3-to-r4 ctxt 0)
   (cps-asm-op-to-r3 ctxt init)
   ;; mr r0,r3
   (cps-asm-move-rX-to-rY ctxt 3 0)
   (cps-asm-op-to-r3 ctxt n)
   (let ((out (cps-asm-make-label ctxt))
	 (loop (cps-asm-make-label ctxt)))
     (cps-asm-def-label ctxt loop)
     ;; addic. r3,r3,-4
     (cps-asm-u16 ctxt #x3463 #xfffc)
     ;; ble out
     (cps-asm-word-with-s2-laboff ctxt #x40810000 out)
     ;; stwu r0,4(r4)
     (cps-asm-u16 ctxt #x9404 #x0004)
     ;; XXX - b loop
     (cps-asm-u16 ctxt #x4bff #xfff4)
     (cps-asm-def-label ctxt out))))

(define-primop (vector-length (res) (a))
  (asm
   (cps-asm-op-to-r3 ctxt a)
   (cps-asm-ref-r3-to-r3 ctxt 0)
   ;; rlwinm  r3,r3,30,3,29
   (cps-asm-u16 ctxt #x5463 #xf0fa)
   ;; ori     r3,r3,1
   (cps-asm-u16 ctxt #x6063 #x0001)
   (cps-asm-r3-to-reg ctxt res)))

(define-primop (vector-ref (res) (vec idx))
  (asm
   (cps-asm-op-to-r4 ctxt vec)
   (cps-asm-op-to-r3 ctxt idx)
   ;;   4:   7c 63 22 14     add     r3,r3,r4
   (cps-asm-u16 ctxt #x7c63 #x2214)
   ;;   8:   88 63 00 04     lwz     r3,3(r3)
   (cps-asm-u16 ctxt #x8063 #x0003)
   (cps-asm-r3-to-reg ctxt res)))

(define-primop (vector-set (res) (vec idx val))
  (asm
   (cps-asm-op-to-r4 ctxt vec)
   (cps-asm-op-to-r3 ctxt idx)
   ;;   4:   7c 63 22 14     add     r4,r3,r4
   (cps-asm-u16 ctxt #x7c83 #x2214)
   (cps-asm-op-to-r3 ctxt val)
   ;; stw r3,3(r4)
   (cps-asm-u16 ctxt #x9064 #x0003)
   (cps-asm-op-to-r3 ctxt (cps-quote (if #f #f)))
   (cps-asm-r3-to-reg ctxt res)))
  
(define-primif (if-eq? (v1 v2) (else-label))
  (asm
   (cps-asm-op-to-r4 ctxt v1)
   (cps-asm-op-to-r3 ctxt v2)
   ;; cmpw cr7,r3,r4
   (cps-asm-u16 ctxt #x7f83 #x2000)
   ;; bne cr7,lab
   (cps-asm-word-with-s2-laboff ctxt #x409e0000 else-label)))

(define-primif (if-fixnum? (v) (else-label))
  (asm
   (cps-asm-op-to-r3 ctxt v)
   ;; andi r3,r3,3
   (cps-asm-u16 ctxt #x7063 #x0003)
   ;; cmpwi cr7,r3,1
   (cps-asm-u16 ctxt #x2f83 #x0001)
   ;; bne cr7,lab
   (cps-asm-word-with-s2-laboff ctxt #x409e0000 else-label)))
  
(define-primitive (add-fixnum (res) (arg1 arg2) (overflow))
  (asm
   ;; li r0,0
   (cps-asm-u16 ctxt #x3800 #x0000)
   ;; mtxer r0
   (cps-asm-u16 ctxt #x7c01 #x03a6)
   (cps-asm-op-to-r4 ctxt arg1)
   (cps-asm-op-to-r3 ctxt arg2)
   ;; subi r3,r3,1
   (cps-asm-u16 ctxt #x3863 #xffff)
   ;; addo. r3,r3,r4
   (cps-asm-u16 ctxt #x7c63 #x2615)
   ;; bso overflow
   (cps-asm-word-with-s2-laboff ctxt #x41830000 overflow)
   (cps-asm-r3-to-reg ctxt res)))

(define (cps-asm-split-r3 ctxt hi lo)
  ;;  138:   54 64 93 ba     rlwinm  r4,r3,18,14,29
  ;;  13c:   54 63 13 ba     rlwinm  r3,r3,2,14,29
  (cps-asm-u16 ctxt #x5464 #x93ba)
  (cps-asm-u16 ctxt #x5463 #x13ba)
  ;;   f4:   60 84 00 01     ori     r4,r4,1
  ;;   f8:   60 63 00 01     ori     r3,r3,1
  (cps-asm-u16 ctxt #x6084 #x0001)
  (cps-asm-u16 ctxt #x6063 #x0001)
  (cps-asm-r3-to-reg ctxt lo)
  ;; mr r3,r4
  (cps-asm-u16 ctxt #x7c83 #x2378)
  (cps-asm-r3-to-reg ctxt hi))

(define (cps-asm-unfix-r3 ctxt)
  ;; e8:   7c 63 16 70     srawi   r3,r3,2
  (cps-asm-u16 ctxt #x7c63 #x1670))

(define (cps-asm-fix-r3 ctxt)
  ;;   f0:   54 63 13 ba     rlwinm  r3,r3,2,14,29
  (cps-asm-u16 ctxt #x5463 #x13ba)
  ;;   f8:   60 63 00 01     ori     r3,r3,1
  (cps-asm-u16 ctxt #x6063 #x0001))
 
(define-primitive (split-fixnum (hi lo) (a) ())
  (asm
   (cps-asm-op-to-r3 ctxt a)
   (cps-asm-unfix-r3 ctxt)
   (cps-asm-split-r3 ctxt hi lo)))

(define-primitive (add-fixnum2 (hi lo) (a b k) ())
  (asm
   (cps-asm-op-to-r3 ctxt a)
   (cps-asm-unfix-r3 ctxt)
   ;; mr r4,r3
   (cps-asm-u16 ctxt #x7c64 #x1b78)
   (cps-asm-op-to-r3 ctxt b)
   (cps-asm-unfix-r3 ctxt)
   ;;   8:   7c 83 22 14     add     r4,r3,r4
   (cps-asm-u16 ctxt #x7c83 #x2214)
   (cps-asm-op-to-r3 ctxt k)
   (cps-asm-unfix-r3 ctxt)
   ;;   c:   7c 63 22 14     add     r3,r3,r4
   (cps-asm-u16 ctxt #x7c63 #x2214)
   (cps-asm-split-r3 ctxt hi lo)))

(define-primitive (sub-fixnum (res) (arg1 arg2) (overflow))
  (asm
   ;; li r0,0
   (cps-asm-u16 ctxt #x3800 #x0000)
   ;; mtxer r0
   (cps-asm-u16 ctxt #x7c01 #x03a6)
   (cps-asm-op-to-r4 ctxt arg1)
   (cps-asm-op-to-r3 ctxt arg2)
   ;; subi r3,r3,1
   (cps-asm-u16 ctxt #x3863 #xffff)
   ;; subo. r3,r4,r3
   (cps-asm-u16 ctxt #x7c63 #x2451)
   ;; bso overflow
   (cps-asm-word-with-s2-laboff ctxt #x41830000 overflow)
   (cps-asm-r3-to-reg ctxt res)))
  
(define-primitive (sub-fixnum2 (hi lo) (a b k) ())
  (asm
   (cps-asm-op-to-r3 ctxt a)
   (cps-asm-unfix-r3 ctxt)
   ;; mr r4,r3
   (cps-asm-u16 ctxt #x7c64 #x1b78)
   (cps-asm-op-to-r3 ctxt b)
   (cps-asm-unfix-r3 ctxt)
   ;;   c:   7c 83 20 50     subf    r4,r3,r4
   (cps-asm-u16 ctxt #x7c83 #x2050)
   (cps-asm-op-to-r3 ctxt k)
   (cps-asm-unfix-r3 ctxt)
   ;;  a8:   7c 63 07 34     extsh   r3,r3
   (cps-asm-u16 ctxt #x7c63 #x0734)
   ;;   c:   7c 63 22 14     add     r3,r3,r4
   (cps-asm-u16 ctxt #x7c63 #x2214)
   (cps-asm-split-r3 ctxt hi lo)))

(define-primitive (mul-fixnum (res) (arg1 arg2) (overflow))
  (asm
   ;; li r0,0
   (cps-asm-u16 ctxt #x3800 #x0000)
   ;; mtxer r0
   (cps-asm-u16 ctxt #x7c01 #x03a6)
   (cps-asm-op-to-r4 ctxt arg1)
   (cps-asm-op-to-r3 ctxt arg2)
   ;; subi r3,r3,1
   (cps-asm-u16 ctxt #x3863 #xffff)
   ;; srawi r4,2
   (cps-asm-u16 ctxt #x7c84 #x1670)
   ;; mullwo. r3,r3,r4
   (cps-asm-u16 ctxt #x7c63 #x25d7)
   ;; addi r3,r3,1
   (cps-asm-u16 ctxt #x3863 #x0001)
   ;; bso overflow
   (cps-asm-word-with-s2-laboff ctxt #x41830000 overflow)
   (cps-asm-r3-to-reg ctxt res)))

(define-primitive (mul-fixnum2 (hi lo) (a b c k) ())
  (asm
   (cps-asm-op-to-r3 ctxt a)
   (cps-asm-unfix-r3 ctxt)
   ;; mr r4,r3
   (cps-asm-u16 ctxt #x7c64 #x1b78)
   (cps-asm-op-to-r3 ctxt b)
   (cps-asm-unfix-r3 ctxt)
   ;;  18:   7c 83 21 d6     mullw   r4,r3,r4
   (cps-asm-u16 ctxt #x7c83 #x21d6)
   (cps-asm-op-to-r3 ctxt c)
   (cps-asm-unfix-r3 ctxt)
   ;; 1c:   7c 83 22 14     add     r4,r3,r4
   (cps-asm-u16 ctxt #x7c83 #x2214)
   (cps-asm-op-to-r3 ctxt k)
   (cps-asm-unfix-r3 ctxt)
   ;;   c:   7c 63 22 14     add     r3,r3,r4
   (cps-asm-u16 ctxt #x7c63 #x2214)
   (cps-asm-split-r3 ctxt hi lo)))
  
(define-primitive (quotrem-fixnum2 (q r) (a b c) ())
  (asm
   (cps-asm-op-to-r3 ctxt a)
   ;; rlwinm 4,3,14,0,15
   (cps-asm-u16 ctxt #x5464 #x701e)
   (cps-asm-op-to-r3 ctxt b)
   (cps-asm-unfix-r3 ctxt)
   ;; add 4,3,4
   (cps-asm-u16 ctxt #x7c83 #x2214)
   (cps-asm-op-to-r3 ctxt c)
   (cps-asm-unfix-r3 ctxt)
   ;; mr 0,3
   (cps-asm-u16 ctxt #x7c60 #x1b78)
   ;; divwu 3,4,0
   (cps-asm-u16 ctxt #x7c64 #x0396)
   ;; mullw 0,3,0
   (cps-asm-u16 ctxt #x7c03 #x01d6)
   (cps-asm-fix-r3 ctxt)
   (cps-asm-r3-to-reg ctxt q)
   ;; subf 3,0,4
   (cps-asm-u16 ctxt #x7c60 #x2050)
   (cps-asm-fix-r3 ctxt)
   (cps-asm-r3-to-reg ctxt r)))

(define-primop (quotient-fixnum (res) (arg1 arg2))
  (asm
   (cps-asm-op-to-r4 ctxt arg1)
   (cps-asm-op-to-r3 ctxt arg2)
   ;;    0:   7c 63 16 70     srawi   r3,r3,2
   ;;    4:   7c 84 16 70     srawi   r4,r4,2
   ;;    8:   7c 84 1b d6     divw    r4,r4,r3
   ;;    c:   54 84 10 3a     rlwinm  r4,r4,2,0,29
   ;;   10:   60 83 00 01     ori     r3,r4,1
   (cps-asm-u16 ctxt #x7c63 #x1670)
   (cps-asm-u16 ctxt #x7c84 #x1670)
   (cps-asm-u16 ctxt #x7c84 #x1bd6)
   (cps-asm-u16 ctxt #x5484 #x103a)
   (cps-asm-u16 ctxt #x6083 #x0001)
   (cps-asm-r3-to-reg ctxt res)))

(define-primop (remainder-fixnum (res) (arg1 arg2))
  (asm
   (cps-asm-op-to-r4 ctxt arg1)
   (cps-asm-op-to-r3 ctxt arg2)
   ;;    0:   7c 63 16 70     srawi   r3,r3,2
   ;;    4:   7c 84 16 70     srawi   r4,r4,2
   ;;    8:   7c 04 1b d6     divw    r0,r4,r3
   ;;    c:   7c 00 19 d6     mullw   r0,r0,r3
   ;;   10:   7c 80 20 50     subf    r4,r0,r4
   ;;   14:   54 84 10 3a     rlwinm  r4,r4,2,0,29
   ;;   18:   60 83 00 01     ori     r3,r4,1
   (cps-asm-u16 ctxt #x7c63 #x1670)
   (cps-asm-u16 ctxt #x7c84 #x1670)
   (cps-asm-u16 ctxt #x7c04 #x1bd6)
   (cps-asm-u16 ctxt #x7c00 #x19d6)
   (cps-asm-u16 ctxt #x7c80 #x2050)
   (cps-asm-u16 ctxt #x5484 #x103a)
   (cps-asm-u16 ctxt #x6083 #x0001)
   (cps-asm-r3-to-reg ctxt res)))

(define-primif (if-< (v1 v2) (else-label))
  (asm
   (cps-asm-op-to-r4 ctxt v1)
   (cps-asm-op-to-r3 ctxt v2)
   ;; cmpw cr7,r3,r4
   (cps-asm-u16 ctxt #x7f83 #x2000)
   ;; ble cr7,lab
   (cps-asm-word-with-s2-laboff ctxt #x409d0000 else-label)))

(define-primop (cons (res) (a b))
  (alloc-size
   2)
  (asm
   (cps-asm-alloc-to-r4 ctxt 2)
   (cps-asm-op-to-r3 ctxt a)
   (cps-asm-store-r3-to-r4 ctxt 0)
   (cps-asm-op-to-r3 ctxt b)
   (cps-asm-store-r3-to-r4 ctxt 1)
   ;; mr r3,r4
   (cps-asm-u16 ctxt #x7c83 #x2378)
   (cps-asm-r3-to-reg ctxt res)))

(define-primif (if-pair? (a) (else-label))
  (asm
   (cps-asm-op-to-r3 ctxt a)
   ;; andi r3,r3,3
   (cps-asm-u16 ctxt #x7063 #x0003)
   ;; cmpwi cr7,r3,0
   (cps-asm-u16 ctxt #x2f83 #x0000)
   ;; bne cr7,lab
   (cps-asm-word-with-s2-laboff ctxt #x409e0000 else-label)
   (cps-asm-op-to-r3 ctxt a)
   (cps-asm-ref-r3-to-r3 ctxt 0)
   ;; andi r3,r3,3
   (cps-asm-u16 ctxt #x7063 #x0003)
   ;; cmpwi cr7,r3,3
   (cps-asm-u16 ctxt #x2f83 #x0003)
   ;; beq cr7,lab
   (cps-asm-word-with-s2-laboff ctxt #x419e0000 else-label)))

(define-primop (car (res) (a))
  (asm
   (cps-asm-op-to-r3 ctxt a)
   (cps-asm-ref-r3-to-r3 ctxt 0)
   (cps-asm-r3-to-reg ctxt res)))
  
(define-primop (cdr (res) (a))
  (asm
   (cps-asm-op-to-r3 ctxt a)
   (cps-asm-ref-r3-to-r3 ctxt 1)
   (cps-asm-r3-to-reg ctxt res)))

(define-primop (set-car (res) (a b))
  (asm
   (cps-asm-op-to-r4 ctxt a)
   (cps-asm-op-to-r3 ctxt b)
   (cps-asm-store-r3-to-r4 ctxt 0)
   (cps-asm-op-to-r3 ctxt (cps-quote (if #f #f)))
   (cps-asm-r3-to-reg ctxt res)))

(define-primop (set-cdr (res) (a b))
  (asm
   (cps-asm-op-to-r4 ctxt a)
   (cps-asm-op-to-r3 ctxt b)
   (cps-asm-store-r3-to-r4 ctxt 1)
   (cps-asm-op-to-r3 ctxt (cps-quote (if #f #f)))
   (cps-asm-r3-to-reg ctxt res)))

(define-primop (identity (res) (a))
  (asm
   (cps-asm-op-to-r3 ctxt a)
   (cps-asm-r3-to-reg ctxt res)))
  
(define-primif (if-char? (a) (else-label))
  (asm
   (cps-asm-op-to-r3 ctxt a)
   ;; andi r3,r3,7
   (cps-asm-u16 ctxt #x7063 #x0007)
   ;; cmpwi cr7,r3,6
   (cps-asm-u16 ctxt #x2f83 #x0006)
   ;; bne cr7,lab
   (cps-asm-word-with-s2-laboff ctxt #x409e0000 else-label)))

(define-primop (integer->char (res) (a))
  (asm
   (cps-asm-op-to-r3 ctxt a)
   ;; rlwinm  r3,r3,1,0,30
   (cps-asm-u16 ctxt #x5463 #x083c)
   ;; ori     r3,r3,4
   (cps-asm-u16 ctxt #x6063 #x0004)
   (cps-asm-r3-to-reg ctxt res)))

(define-primop (char->integer (res) (a))
  (asm
   (cps-asm-op-to-r3 ctxt a)
   ;; rlwinm  r3,r3,31,1,31
   (cps-asm-u16 ctxt #x5463 #xf87e)
   ;; rlwinm  r3,r3,0,31,29
   (cps-asm-u16 ctxt #x5463 #x07fa)
   (cps-asm-r3-to-reg ctxt res)))

;; MARKER
;; (cps-asm-word ctxt #x7063DEAD)

(define-primif (if-bytevec? (a) (else-label))
  (asm
   (cps-asm-op-to-r3 ctxt a)
   ;; andi r3,r3,3
   (cps-asm-u16 ctxt #x7063 #x0003)
   ;; cmpwi cr7,r3,0
   (cps-asm-u16 ctxt #x2f83 #x0000)
   ;; bne cr7,lab
   (cps-asm-word-with-s2-laboff ctxt #x409e0000 else-label)
   (cps-asm-op-to-r3 ctxt a)
   (cps-asm-ref-r3-to-r3 ctxt 0)
   ;; cmpwi cr7,r3,0
   (cps-asm-u16 ctxt #x2f83 #x0000)
   ;; bge cr7,lab
   (cps-asm-word-with-s2-laboff ctxt #x409c0000 else-label)
   ;; andi r3,r3,15
   (cps-asm-u16 ctxt #x7063 #x000f)
   ;; cmpwi cr7,r3,11
   (cps-asm-u16 ctxt #x2f83 #x000b)
   ;; bne cr7,lab
   (cps-asm-word-with-s2-laboff ctxt #x409e0000 else-label)))

(define-primop (make-bytevec (res) (n))
  (alloc-size
   #t)
  (asm
   (cps-asm-op-to-r3 ctxt n)
   ;; addi r3,r3,12
   (cps-asm-u16 ctxt #x3863 #x000c)
   ;; rlwinm  r3,r3,30,2,29
   (cps-asm-u16 ctxt #x5463 #xf0ba)
   ;; addi r3,r3,4
   (cps-asm-u16 ctxt #x3863 #x0004)
   (cps-asm-alloc-r3-bytes-to-r4 ctxt)
   ;; mr r3,r4
   (cps-asm-u16 ctxt #x7c83 #x2378)
   (cps-asm-r3-to-reg ctxt res)
   (cps-asm-op-to-r3 ctxt n)
   ;;    0:   3c 00 80 00     lis     r0,-32768
   (cps-asm-u16 ctxt #x3c00 #x8000)
   ;;    4:   54 63 10 36     rlwinm  r3,r3,2,0,27
   (cps-asm-u16 ctxt #x5463 #x1036)
   ;;    8:   60 00 00 03     ori     r0,r0,11
   (cps-asm-u16 ctxt #x6000 #x000b)
   ;;    c:   7c 63 03 78     or      r3,r3,r0
   (cps-asm-u16 ctxt #x7c63 #x0378)
   (cps-asm-store-r3-to-r4 ctxt 0)))
  
(define-primop (bytevec-length-8 (res) (a))
  (asm
   (cps-asm-op-to-r3 ctxt a)
   (cps-asm-ref-r3-to-r3 ctxt 0)
   ;; rlwinm  r3,r3,30,3,29
   (cps-asm-u16 ctxt #x5463 #xf0fa)
   ;; ori     r3,r3,1
   (cps-asm-u16 ctxt #x6063 #x0001)
   (cps-asm-r3-to-reg ctxt res)))
  
(define-primop (bytevec-ref-u8 (res) (vec idx))
  (asm
   (cps-asm-op-to-r4 ctxt vec)
   (cps-asm-op-to-r3 ctxt idx)
   ;;   0:   54 63 f0 be     rlwinm  r3,r3,30,2,31
   ;;   4:   7c 63 22 14     add     r3,r3,r4
   ;;   8:   88 63 00 04     lbz     r3,4(r3)
   ;;   c:   54 63 10 3a     rlwinm  r3,r3,2,0,29
   ;;  10:   60 63 00 01     ori     r3,r3,1
   (cps-asm-u16 ctxt #x5463 #xf0be)
   (cps-asm-u16 ctxt #x7c63 #x2214)
   (cps-asm-u16 ctxt #x8863 #x0004)
   (cps-asm-u16 ctxt #x5463 #x103a)
   (cps-asm-u16 ctxt #x6063 #x0001)
   (cps-asm-r3-to-reg ctxt res)))

(define-primop (bytevec-set-u8 (res) (vec idx val))
  (asm
   (cps-asm-op-to-r4 ctxt vec)
   (cps-asm-op-to-r3 ctxt idx)
   ;;   0:   54 63 f0 be     rlwinm  r3,r3,30,2,31
   (cps-asm-u16 ctxt #x5463 #xf0be)
   ;;   4:   7c 63 22 14     add     r4,r3,r4
   (cps-asm-u16 ctxt #x7c83 #x2214)
   (cps-asm-op-to-r3 ctxt val)
   ;;   0:   54 63 f0 be     rlwinm  r3,r3,30,2,31
   (cps-asm-u16 ctxt #x5463 #xf0be)
   ;;   8:   88 63 00 04     stb     r3,4(r4)
   (cps-asm-u16 ctxt #x9864 #x0004)
   (cps-asm-op-to-r3 ctxt (cps-quote (if #f #f)))
   (cps-asm-r3-to-reg ctxt res)))

(define-primop (bytevec-length-16 (res) (a))
  (asm
   (cps-asm-op-to-r3 ctxt a)
   (cps-asm-ref-r3-to-r3 ctxt 0)
   ;;  10:   54 63 e9 3a     rlwinm  r3,r3,29,4,29
   (cps-asm-u16 ctxt #x5463 #xe93a)
   ;; ori     r3,r3,1
   (cps-asm-u16 ctxt #x6063 #x0001)
   (cps-asm-r3-to-reg ctxt res)))

(define-primop (bytevec-ref-u16 (res) (vec idx))
  (asm
   (cps-asm-op-to-r4 ctxt vec)
   (cps-asm-op-to-r3 ctxt idx)
   ;;   0:   54 63 f8 7c     rlwinm  r3,r3,31,1,30
   ;;   4:   7c 63 22 14     add     r3,r3,r4
   ;;   8:   a0 63 00 04     lhz     r3,4(r3)
   ;;   c:   54 63 10 3a     rlwinm  r3,r3,2,0,29
   ;;  10:   60 63 00 01     ori     r3,r3,1
   (cps-asm-u16 ctxt #x5463 #xf87c)
   (cps-asm-u16 ctxt #x7c63 #x2214)
   (cps-asm-u16 ctxt #xa063 #x0004)
   (cps-asm-u16 ctxt #x5463 #x103a)
   (cps-asm-u16 ctxt #x6063 #x0001)
   (cps-asm-r3-to-reg ctxt res)))

(define-primop (bytevec-set-u16 (res) (vec idx val))
  (asm
   (cps-asm-op-to-r4 ctxt vec)
   (cps-asm-op-to-r3 ctxt idx)
   ;;   0:   54 63 f8 7c     rlwinm  r3,r3,31,1,30
   (cps-asm-u16 ctxt #x5463 #xf87c)
   ;;   4:   7c 63 22 14     add     r4,r3,r4
   (cps-asm-u16 ctxt #x7c83 #x2214)
   (cps-asm-op-to-r3 ctxt val)
   ;;   0:   54 63 f0 be     rlwinm  r3,r3,30,2,31
   (cps-asm-u16 ctxt #x5463 #xf0be)
   ;;   8:   b0 64 00 04     sth     r3,4(r4)
   (cps-asm-u16 ctxt #xb064 #x0004)
   (cps-asm-op-to-r3 ctxt (cps-quote (if #f #f)))
   (cps-asm-r3-to-reg ctxt res)))
  
(define-primif (if-code? (a) (else-label))
  (asm
   (cps-asm-op-to-r3 ctxt a)
   ;; andi r3,r3,3
   (cps-asm-u16 ctxt #x7063 #x0003)
   ;; cmpwi cr7,r3,0
   (cps-asm-u16 ctxt #x2f83 #x0000)
   ;; bne cr7,lab
   (cps-asm-word-with-s2-laboff ctxt #x409e0000 else-label)
   (cps-asm-op-to-r3 ctxt a)
   (cps-asm-ref-r3-to-r3 ctxt 0)
   ;; cmpwi cr7,r3,0
   (cps-asm-u16 ctxt #x2f83 #x0000)
   ;; bge cr7,lab
   (cps-asm-word-with-s2-laboff ctxt #x409c0000 else-label)
   ;; andi r3,r3,15
   (cps-asm-u16 ctxt #x7063 #x000f)
   ;; cmpwi cr7,r3,11
   (cps-asm-u16 ctxt #x2f83 #x000f)
   ;; bne cr7,lab
   (cps-asm-word-with-s2-laboff ctxt #x409e0000 else-label)))

(define-primop (code-insn-length (res) (c))
  (asm
   (cps-asm-op-to-r3 ctxt c)
   (cps-asm-ref-r3-to-r3 ctxt 0)
   ;; 20:   54 63 b2 fa     rlwinm  r3,r3,22,11,29
   ;; 24:   60 63 00 01     ori     r3,r3,1
   (cps-asm-u16 ctxt #x5463 #xb2fa)
   (cps-asm-u16 ctxt #x6063 #x0001)
   (cps-asm-r3-to-reg ctxt res)))
  
(define-primop (code-lit-length (res) (c))
  (asm
   (cps-asm-op-to-r3 ctxt c)
   (cps-asm-ref-r3-to-r3 ctxt 0)
   ;; 30:   54 63 f5 ba     rlwinm  r3,r3,30,22,29
   ;; 34:   60 63 00 01     ori     r3,r3,1
   (cps-asm-u16 ctxt #x5463 #xf5ba)
   (cps-asm-u16 ctxt #x6063 #x0001)
   (cps-asm-r3-to-reg ctxt res)))

(define-primop (make-code (res) (insn-length lit-length))
  (alloc-size
   #t)
  (asm
   ;; Step 1.  Compute length in bytes and allocate
   (cps-asm-op-to-r3 ctxt insn-length)
   (cps-asm-op-to-r4 ctxt lit-length)
   ;;  40:   54 63 00 3a     rlwinm  r3,r3,0,0,29
   ;;  44:   54 84 00 3a     rlwinm  r4,r4,0,0,29
   ;;  48:   38 63 00 04     addi    r3,r3,4
   ;;  4c:   7c 64 1a 14     add     r3,r4,r3
   (cps-asm-u16 ctxt #x5463 #x003a)
   (cps-asm-u16 ctxt #x5484 #x003a)
   (cps-asm-u16 ctxt #x3863 #x0004)
   (cps-asm-u16 ctxt #x7c64 #x1a14)
   (cps-asm-alloc-r3-bytes-to-r4 ctxt)
   ;; mr r3,r4
   (cps-asm-u16 ctxt #x7c83 #x2378)
   (cps-asm-r3-to-reg ctxt res)

   ;; Step 2.  Compute tag word and store it.
   (cps-asm-op-to-r3 ctxt insn-length)
   (cps-asm-op-to-r4 ctxt lit-length)
   ;;   60:   3c 00 80 00     lis     r0,-32768
   ;;   64:   54 63 50 26     rlwinm  r3,r3,10,0,19
   ;;   68:   54 84 10 36     rlwinm  r4,r4,2,0,27
   ;;   6c:   60 00 00 0f     ori     r0,r0,15
   ;;   70:   7c 63 03 78     or      r3,r3,r0
   ;;   74:   7c 83 1b 78     or      r3,r4,r3
   (cps-asm-u16 ctxt #x3c00 #x8000)  
   (cps-asm-u16 ctxt #x5463 #x5026)  
   (cps-asm-u16 ctxt #x5484 #x1036)  
   (cps-asm-u16 ctxt #x6000 #x000f)  
   (cps-asm-u16 ctxt #x7c63 #x0378)  
   (cps-asm-u16 ctxt #x7c83 #x1b78)  
   (cps-asm-op-to-r4 ctxt res)
   (cps-asm-store-r3-to-r4 ctxt 0)

   ;; Step 3.  Initialize lits with #f.
   (cps-asm-op-to-r3 ctxt insn-length)
   ;;  40:   54 63 00 3a     rlwinm  r3,r3,0,0,29
   (cps-asm-u16 ctxt #x5463 #x003a)
   ;; add r4,r4,r3
   (cps-asm-u16 ctxt #x7c84 #x1a14)
   (cps-asm-op-to-r3 ctxt (cps-quote #f))
   (cps-asm-move-rX-to-rY ctxt 3 0)
   (cps-asm-op-to-r3 ctxt lit-length)
   (let ((out (cps-asm-make-label ctxt))
	 (loop (cps-asm-make-label ctxt)))
     (cps-asm-def-label ctxt loop)
     ;; addic. r3,r3,-4
     (cps-asm-u16 ctxt #x3463 #xfffc)
     ;; ble out
     (cps-asm-word-with-s2-laboff ctxt #x40810000 out)
     ;; stwu r0,4(r4)
     (cps-asm-u16 ctxt #x9404 #x0004)
     ;; XXX - b loop
     (cps-asm-u16 ctxt #x4bff #xfff4)
     (cps-asm-def-label ctxt out))))

(pk 'asm-ppc)
