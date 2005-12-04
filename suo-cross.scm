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

(define (suo:record-ref rec idx)
  (vector-ref ((record-accessor suo:record-record-type 'fields) rec) idx))

(define (suo:record-set! rec idx val)
  (vector-set! ((record-accessor suo:record-record-type 'fields) rec) idx val))

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

(define (assemble-object obj mem)
  (let ((idx 0)
	(byte-idx 0)
	(byte-word 0)
	(ptr-hash (make-hash-table 1003)))

    (define (emit-word word)
      (u32vector-set! mem idx word)
      (set! idx (1+ idx))
      (1- idx))

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

    (define (emit obj)
      (cond
       ((pair? obj)
	(let* ((w1 (asm (car obj)))
	       (w2 (asm (cdr obj)))
	       (ptr (emit-word w1)))
	  (emit-word w2)
	  ptr))
       ((suo:record? obj)
	(let* ((desc (asm (suo:record-desc obj)))
	       (ws (map asm (vector->list (suo:record-fields obj))))
	       (ptr (emit-word (+ desc 3))))
	  (for-each emit-word ws)
	  ptr))
       ((vector? obj)
	(let* ((ws (map asm (vector->list obj)))
	       (ptr (emit-word (+ #x80000000 (* (vector-length obj) 16) 3))))
	  (for-each emit-word ws)
	  ptr))
       ((string? obj)
	(let ((ptr (emit-word (+ #x80000000 (* (string-length obj) 64) 7))))
	  (for-each emit-byte (map char->integer (string->list obj)))
	  (flush-bytes)
	  ptr))
       ((suo:code? obj)
	(let* ((insns (suo:code-insns obj))
	       (insn-words (u32vector-length insns))
	       (literals (suo:code-literals obj))
	       (literal-words (vector-length literals))
	       (literal-ws (map asm (vector->list literals)))
	       (ptr (emit-word (+ #x80000000
				  (* insn-words (* 256 16))
				  (* literal-words 16)
				  15))))
	  (do ((i 0 (1+ i)))
	      ((= i insn-words))
	    (emit-word (u32vector-ref insns i)))
	  (for-each emit-word literal-ws)
	  ptr))
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
       (else
	(let ((ptr (hashq-ref ptr-hash obj)))
	  (or ptr
	      (let ((ptr (* 4 (emit obj))))
		(hashq-set! ptr-hash obj ptr)
		ptr))))))

    (asm obj)))
