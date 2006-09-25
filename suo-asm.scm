;;;; suo-asm -- machine independent parts of the assembler

;;; Dumping suo objects into a u32vector

(define all-suo-symbols '())
(define all-suo-keywords '())

(define suo-bootinfo-marker (list 'bootinfo))

(define (integer->limbs n)
  (define (->limbs x)
    (if (zero? x)
	'()
	(cons (remainder x base)
	      (->limbs (quotient x base)))))
  (let* ((l (->limbs n))
	 (v (make-u8vector (* 2 (length l)))))
    (do ((i 0 (+ i 2))
	 (l l (cdr l)))
	((null? l))
      (u8vector-set! v i      (quotient (car l) #x100))
      (u8vector-set! v (1+ i) (remainder (car l) #x100)))
    v))

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
       ((record? obj)
	(let* ((type (record-type obj))
	       (fields (record->list obj))
	       (ptr (alloc obj (1+ (length fields)))))
	  (if (not (eqv? (length fields) (record-ref type 0)))
	      (error "inconsistent record"))
	  (if (and (eq? type variable-type)
		   (eq? (if #f #f) (record-ref obj 0))
		   (not (topexp-variable-init? obj)))
	      (pk 'undefined (record-ref obj 1)))
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
	(let ((suo-str (record string-type
			       (apply u8vector
				      (map char->integer (string->list obj))))))
	  (emit suo-str)
	  ;; register the original OBJ as required
	  (hashq-set! ptr-hash obj (hashq-ref ptr-hash suo-str))))
       ((symbol? obj)
	(let ((suo-sym (record symbol-type (symbol->string obj))))
	  (set! all-suo-symbols (cons suo-sym all-suo-symbols))
	  (emit suo-sym)
	  ;; register the original OBJ as required
	  (hashq-set! ptr-hash obj (hashq-ref ptr-hash suo-sym))))
       ((keyword? obj)
	(let ((suo-keyword (record keyword-type (keyword->symbol obj))))
	  (set! all-suo-keywords (cons suo-keyword all-suo-keywords))
	  (emit suo-keyword)
	  ;; register the original OBJ as required
	  (hashq-set! ptr-hash obj (hashq-ref ptr-hash suo-keyword))))
       ((code? obj)
	(let* ((insns (code-insns obj))
	       (insn-words (u32vector-length insns))
	       (literals (code-literals obj))
	       (literal-words (vector-length literals))
	       (ptr (alloc obj (+ 1 insn-words literal-words))))
	  (apply emit-words ptr
		 (+ #x80000000
		    (* insn-words (* 256 16))
		    (* literal-words 16)
		    15)
		 (append (u32vector->list insns)
			 (map asm (vector->list literals))))))
       ((integer?)
	;; bignum
	(let ((suo-bignum (record bignum-type (integer->limbs obj))))
	  (emit suo-bignum)
	  ;; register the original OBJ as required
	  (hashq-set! ptr-hash obj (hashq-ref ptr-hash suo-bignum))))
       (else
	(error "unsupported value: " obj))))

    (define (asm obj)
      (cond
       ((eq? obj suo-bootinfo-marker)
	#f)
       ((fixnum? obj)
	(+ (* 4 obj)
	   (cond ((and (<= 0 obj) (<= obj fixnum-max)) 0)
		 ((and (<= fixnum-min obj) (<= obj -1)) #x100000000)
		 (else
		  (error "integer is not fixnum: " obj)))
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
    (for-each (lambda (idx)
		(emit-words idx (asm (cons all-suo-symbols
					   all-suo-keywords))))
	      bootinfo-idxs)

    (let ((mem2 (make-u32vector idx)))
      (do ((i 0 (1+ i)))
	  ((= i idx))
	(u32vector-set! mem2 i (u32vector-ref mem i)))
      mem2)))
