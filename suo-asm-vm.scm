;;; Code generation and assembling

(declare-variables cps-verbose)

(define (cps-asm-insn-1 ctxt op xyz)
  (cps-asm-u16 ctxt (+ (* op 256) (quotient xyz 32768)))
  (cps-asm-u16 ctxt (remainder xyz 32768)))

(define (cps-asm-insn-2 ctxt op x yz)
  (cps-asm-u16 ctxt (+ (* op 256) x))
  (cps-asm-u16 ctxt yz))

(define (cps-asm-insn-2-with-laboff ctxt op x lab)
  (cps-asm-u16 ctxt (+ (* op 256) x))
  (cps-asm-s16-with-laboff ctxt 0 lab))

(define (cps-asm-insn-3 ctxt op x y z)
  (cps-asm-u16 ctxt (+ (* op 256) x))
  (cps-asm-u16 ctxt (+ (* y 256) z)))

(define (u8-const? obj off)
  (and (cps-quote? obj)
       (let ((val (cps-quote-value obj)))
	 (and (fixnum? obj)
	      (<= 0 (+ val off))
	      (< (+ val off) 256)))))
	 
(define (s16-const? obj)
  (and (cps-quote? obj)
       (let ((val (cps-quote-value obj)))
	 (and (fixnum? obj)
	      (<= -32768 val)
	      (< val 32768)))))

(define RR 0)
(define RL 1)
(define LR 2)
(define LL 3)

(define HALT           0)
(define MISC           4)
(define MOVE           8)
(define REF           12)
(define SET           16)
(define SETL          20)
(define CMP           24)
(define TRAP          28)

(define MOVEI        128)
(define REFI         129)
(define SETI         130)
(define ALLOCI       131)
(define INITI        132)
(define FILLI        133)
(define CHECK_ALLOCI 134)

(define BRANCH       255)

(define MISCOP_GO            0)
(define MISCOP_ALLOC         1)
(define MISCOP_INIT          2)
(define MISCOP_INIT_REC      3)
(define MISCOP_INIT_VEC      4)
(define MISCOP_FILL          5)
(define MISCOP_COPY          6)
(define MISCOP_LOAD_DESC     7)
(define MISCOP_LOAD_LENGTH   8)
(define MISCOP_TEST_REC      9)
(define MISCOP_TEST_VEC     10)
(define MISCOP_TEST_DESC    11)

(define CMPOP_EQ             0)

(define TRAPOP_SYSCALL       0)
(define TRAPOP_CHECK_CALLSIG 1)
(define TRAPOP_CHECK_ALLOC   2)

(define IF_FALSE             0)


(define (cps-asm-reglit-instr ctxt op x y z)

  (define (val p)
    (cond ((cps-reg? p)
	   (cps-reg-idx p))
	  ((cps-quote? p)
	   (cps-asm-litidx ctxt (cps-quote-value p)))
	  (else
	   p)))

  (cps-asm-instr-3 ctxt (+ op
			   (if (cps-quote? y) 2 0)
			   (if (cps-quote? z) 1 0))
		   (cps-reg-idx x) (val y) (val z)))

(define (cps-asm-branch ctxt op lab)
  (cps-asm-insn-2-with-laboff ctxt BRANCH op lab))
	
(define (cps-asm-shuffle ctxt from to)

  (define (move src dst)
    (if cps-verbose
	(pk (cps-render src) '-> (cps-render dst)))
    (let ((src (if (eq? src 'tmp) 255))
	  (dst (if (eq? dst 'tmp) 255)))
      (if (s16-const? src)
	  (cps-asm-insn-2 ctxt MOVEI 
			  (if (cps-reg? dst)
			      (cps-reg-idx dst)
			      dst)
			  (cps-quote-value src))
	  (cps-asm-reglit-instr ctxt MOVE dst src 0))))
  
  (cps-asm-shuffle-with-move ctxt from to move))

(define (cps-asm-go ctxt to)
  (cps-asm-reglit-instr ctxt MISC 0 to MISCOP_GO))

(define (cps-asm-prologue ctxt sig alloc-size)
  (cps-asm-insn-2 ctxt CHECK_CALLSIG 0 sig)
  (cps-asm-insn-1 ctxt CHECK_ALLOCI alloc-size))

(define-primop (syscall (res) args)
  (asm
   (cps-asm-insn-1 ctxt SYSCALL (length args))
   (for-each (lambda (a)
	       (cond ((cps-reg? a)
		      (cps-asm-u16 ctxt 0)
		      (cps-asm-u16 ctxt (cps-reg-idx a)))
		     (else
		      (cps-asm-u16 ctxt #x8000)
		      (cps-asm-s16-with-litoff ctxt 0 (cps-quote-value a)))))
	     args)))

(define (cps-asm-panic ctxt)
  (cps-asm-insn-1 ctxt HALT 0))
  
(define-primop (record (res) (desc . values))
  (alloc-size
   (1+ (length values)))
  (asm
   (cps-asm-insn-2 ctxt ALLOCI (cps-reg-idx dst) (1+ (length values)))
   (cps-asm-reglit-insn ctxt MISC 0 desc MISCOP_INIT_DESC)
   (for-each (lambda (v)
	       (cps-asm-reglit-insn ctxt MISC 0 v MISCOP_INIT))
	     values)))

(define-primop (make-record (res) (desc n-fields init))
  (alloc-size
   #t)
  (asm
   (cps-asm-reglit-insn ctxt MISC res n-fields MISCOP_ALLOC)
   (cps-asm-reglit-insn ctxt MISC 0 desc MISCOP_INIT_DESC)
   (cps-asm-reglit-insn ctxt MISC 0 init MISCOP_FILL)))

(define-primif (if-record? (obj desc) (else-label))
  (asm
   (cps-asm-reglit-insn ctxt MISC 0 obj MISCOP_TEST_REC)
   (if (not (and (cps-quote? desc)
		 (eq? (cps-quote-value desc) #t)))
       (cps-asm-reglit-insn ctxt MISC 0 desc MISCOP_TEST_DESC))
   (cps-asm-insn-branch ctxt IF_FALSE else-label))

(define-primop (record-desc (res) (rec))
  (asm
   (cps-asm-reglit-insn ctxt MISC res rec MISCOP_LOAD_DESC)))

(define-primop (record-ref (res) (rec idx))
  (asm
   (if (u8-const? idx)
       (cps-asm-reglit-insn ctxt REFI res rec (cps-quote-value idx))
       (cps-asm-reglit-insn ctxt REF res rec idx))))

(define-primop (record-set (res) (rec idx val))
  (asm
   (if (cps-reg? rec)
       (if (u8-const? idx 1)
	   (cps-asm-reglit-insn ctxt SETI rec val (1+ (cps-quote-value idx)))
	   (cps-asm-reglit-insn ctxt SET rec val idx))
       (cps-asm-reglit-insn ctxt SETL rec val idx))
   (cps-asm-set ctxt rec idx val)
   (cps-asm-get ctxt (cps-reg-idx res) (cps-quote (if #f #f)))))
  
(define-primif (if-vector? (a) (else-label))
  (asm
   (cps-asm-reglit-insn ctxt MISC TAG_VECTOR obj MISCOP_TEST_VEC)
   (cps-asm-insn-branch ctxt IF_FALSE else-label))

(define-primop (vector (res) values)
  (alloc-size
   (1+ (length values)))
  (asm
   (cps-asm-insn-2 ctxt ALLOCI (cps-reg-idx dst) (1+ (length values)))
   (cps-asm-reglit-insn ctxt MISC TAG_VECTOR desc MISCOP_INIT_VEC)
   (for-each (lambda (v)
	       (cps-asm-reglit-insn ctxt MISC 0 v MISCOP_INIT))
	     values)))

(define-primop (make-vector (res) (n init))
  (alloc-size
   #t)
  (asm
   (cps-asm-reglit-insn ctxt MISC res n-fields MISCOP_ALLOC)
   (cps-asm-reglit-insn ctxt MISC TAG_VECTOR n MISCOP_INIT_VEC)
   (cps-asm-reglit-insn ctxt MISC 0 init MISCOP_FILL)))

(define-primop (vector-length (res) (a))
  (asm
   (cps-asm-reglit-insn ctxt MISC res a MISCOP_LOAD_LENGTH)))

(define-primop (vector-ref (res) (vec idx))
  (asm
   (if (u8-const? idx)
       (cps-asm-reglit-insn ctxt REFI res rec (cps-quote-value idx))
       (cps-asm-reglit-insn ctxt REF res rec idx))))

(define-primop (vector-set (res) (vec idx val))
  (asm
   (if (cps-reg? rec)
       (if (u8-const? idx 1)
	   (cps-asm-reglit-insn ctxt SETI rec val (1+ (cps-quote-value idx)))
	   (cps-asm-reglit-insn ctxt SET rec val idx))
       (cps-asm-reglit-insn ctxt SETL rec val idx))
   (cps-asm-set ctxt rec idx val)
   (cps-asm-get ctxt (cps-reg-idx res) (cps-quote (if #f #f)))))
  
(define-primif (if-eq? (v1 v2) (else-label))
  (asm
   (cps-asm-reglit-insn ctxt CMP CMPOP_EQ v1 v2)
   (cps-asm-insn-branch ctxt IF_FALSE else-label)))

(define-primop (cons (res) (a b))
  (alloc-size
   2)
  (asm))

;; (define-primif (if-pair? (a) (else-label))
;;   (asm))

;; (define-primop (car (res) (a))
;;   (asm))
  
;; (define-primop (cdr (res) (a))
;;   (asm))

;; (define-primop (set-car (res) (a b))
;;   (asm))

;; (define-primop (set-cdr (res) (a b))
;;   (asm))

;; (define-primif (if-fixnum? (v) (else-label))
;;   (asm))
  
;; (define-primitive (add-fixnum (res) (arg1 arg2) (overflow))
;;   (asm))

;; (define-primitive (split-fixnum (hi lo) (a) ())
;;   (asm))

;; (define-primitive (add-fixnum2 (hi lo) (a b k) ())
;;   (asm))

;; (define-primitive (sub-fixnum (res) (arg1 arg2) (overflow))
;;   (asm))
  
;; (define-primitive (sub-fixnum2 (hi lo) (a b k) ())
;;   (asm))

;; (define-primitive (mul-fixnum (res) (arg1 arg2) (overflow))
;;   (asm))

;; (define-primitive (mul-fixnum2 (hi lo) (a b c k) ())
;;   (asm))
  
;; (define-primitive (quotrem-fixnum2 (q r) (a b c) ())
;;   (asm))

;; (define-primop (quotient-fixnum (res) (arg1 arg2))
;;   (asm))

;; (define-primop (remainder-fixnum (res) (arg1 arg2))
;;   (asm))

;; (define-primif (if-< (v1 v2) (else-label))
;;   (asm))

;; (define-primop (identity (res) (a))
;;   (asm))

;; (define-primif (if-char? (a) (else-label))
;;   (asm))

;; (define-primop (integer->char (res) (a))
;;   (asm))

;; (define-primop (char->integer (res) (a))
;;   (asm))

;; (define-primif (if-bytevec? (a) (else-label))
;;   (asm))

;; (define-primop (make-bytevec (res) (n))
;;   (alloc-size
;;    #t)
;;   (asm))
  
;; (define-primop (bytevec-length-8 (res) (a))
;;   (asm))
  
;; (define-primop (bytevec-ref-u8 (res) (vec idx))
;;   (asm))

;; (define-primop (bytevec-set-u8 (res) (vec idx val))
;;   (asm))

;; (define-primop (bytevec-length-16 (res) (a))
;;   (asm))

;; (define-primop (bytevec-ref-u16 (res) (vec idx))
;;   (asm))

;; (define-primop (bytevec-set-u16 (res) (vec idx val))
;;   (asm))
  
;; (define-primif (if-code? (a) (else-label))
;;   (asm))

;; (define-primop (code-insn-length (res) (c))
;;   (asm))
  
;; (define-primop (code-lit-length (res) (c))
;;   (asm))

;; (define-primop (make-code (res) (insn-length lit-length))
;;   (alloc-size
;;    #t)
;;   (asm))

(pk 'asm-vm)
