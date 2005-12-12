;;;; suo-cross -- suo objects in plain Scheme

;;; Representation of suo objects as Scheme values

;; fixnum, characters, singletons, pairs, vectors, strings - directly
;; records - record-record (no bit records)
;; bytevectors - u8vector
;; code - code-record

(define suo:record-record-type
  (make-record-type 'record-record '(desc fields)))

(define (suo:record? obj)
  ((record-predicate suo:record-record-type) obj))

(define (suo:record desc . fields)
  ((record-constructor suo:record-record-type) desc (apply vector fields)))

(define (suo:record-desc rec)
  ((record-accessor suo:record-record-type 'desc) rec))

(define (suo:record-fields rec)
  ((record-accessor suo:record-record-type 'fields) rec))

(define (suo:record-ref rec idx)
  (vector-ref (suo:record-fields rec) idx))

(define (suo:record-set! rec idx val)
  (vector-set! (suo:record-fields rec) idx val))

(define (suo:record-set-desc! rec desc)
  ((record-modifier suo:record-record-type 'desc) rec desc))

(define suo:code-record-type
  (make-record-type 'code-record '(insns literals)))

(define (suo:code? obj)
  ((record-predicate suo:code-record-type) obj))

(define (suo:code insns literals)
  ((record-constructor suo:code-record-type) insns literals))

(define (suo:code-insns code)
  ((record-accessor suo:code-record-type 'insns) code))

(define (suo:code-literals code)
  ((record-accessor suo:code-record-type 'literals) code))


;;; Assembling suo objects into a u32vector

(define (assemble-object obj)
  (let ((mem (make-u32vector 1024))
	(idx 0)
	(ptr-hash (make-hash-table 1003)))

    (define (emit-byte byte)
      ;; little-endian
      (set! byte-word (+ (* 256 byte-word) byte))
      (if (= byte-idx 3)
	  (flush-ytes)
	  (set! byte-idx (1+ byte-idx))))

    (define (flush-bytes)
      (if (> byte-idx 0)
	  (begin
	    (emit-word byte-word)
	    (set! byte-idx 0)
	    (set! byte-word 0))))

    (define (alloc obj n-words)
      (let ((ptr idx))
	(hashq-set! ptr-hash obj ptr)
	(set! idx (+ idx n-words))
	ptr))

    (define (emit-words ptr . words)
      (do ((i ptr (1+ i))
	   (w words (cdr w)))
	  ((null? w))
	(u32vector-set! mem i (car w))))

    (define (emit obj)
      (cond
       ((pair? obj)
	(let ((ptr (alloc obj 2)))
	  (emit-words ptr (asm (car obj)) (asm (cadr obj)))))
       ((suo:record? obj)
	(let* ((desc (suo:record-desc obj))
	       (fields (vector->list (suo:record-fields obj)))
	       (ptr (alloc obj (1+ (length fields)))))
	  (if (not (eqv? (length fields) (suo:record-ref desc 0)))
	      (error "inconsistent record"))
	  (apply emit-words ptr
		 (+ (asm desc) 3)
		 (map asm fields))))
       ((vector? obj)
	(let ((ptr (alloc obj (1+ (vector-length obj)))))
	  (apply emit-words ptr
		 (+ #x80000000 (* (vector-length obj) 16) 3)
		 (map asm (vector->list obj)))))
       ((suo:code? obj)
	(let* ((insns (suo:code-insns obj))
	       (insn-words (u32vector-length insns))
	       (literals (suo:code-literals obj))
	       (literal-words (vector-length literals))
	       (ptr (alloc obj (+ 1 insn-words literal-words))))
	  (apply emit-words ptr
		 (+ #x80000000
		    (* insn-words (* 256 16))
		    (* literal-words 16)
		    15)
		 (append (u32vector->list insns)
			 (map asm (vector->list literals))))))
       (else
	(error "unsupported" obj))))

    (define (asm obj)
      (cond
       ((integer? obj)
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
	(let ((ptr (hashq-ref ptr-hash obj)))
	  (* 4 (or ptr
		   (begin (emit obj)
			  (hashq-ref ptr-hash obj))))))))

    (asm obj)
    (let ((mem2 (make-u32vector idx)))
      (do ((i 0 (1+ i)))
	  ((= i idx))
	(u32vector-set! mem2 i (u32vector-ref mem i)))
      mem2)))
