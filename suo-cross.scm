;;;; suo-cross -- suo objects in plain Scheme

;;; Representation of suo objects as Scheme values

;; fixnum, characters, singletons, pairs, vectors, strings - directly
;; records - record-record (no bit records)
;; bytevectors - u8vector
;; code - code-record

(define suo:record-record-type
  (make-record-type 'record-record '(desc fields)
		    (lambda (obj port) (display "#<suo-record>" port))))

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
  (make-record-type 'code-record '(insns literals)
		    (lambda (obj port) (display "#<suo-code>" port))))

(define (suo:code? obj)
  ((record-predicate suo:code-record-type) obj))

(define (suo:code insns literals)
  ((record-constructor suo:code-record-type) insns literals))

(define (suo:code-insns code)
  ((record-accessor suo:code-record-type 'insns) code))

(define (suo:code-literals code)
  ((record-accessor suo:code-record-type 'literals) code))

;;; The self-referential type of record types

(define suo:record-type-type (suo:record #f 2 'record-type))
(suo:record-set-desc! suo:record-type-type
		      suo:record-type-type)

;;; Assembling suo objects into a u32vector

(define all-suo-symbols '())

(define suo-bootinfo-marker (list 'bootinfo))

(define (assemble-object obj)
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
	       (reverse! (if (zero? i)
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
       ((string? obj)
	(let* ((len (string-length obj))
	       (words (quotient (+ len 3) 4))
	       (ptr (alloc obj (1+ words))))
	  (apply emit-words ptr
		 (+ #x80000000 (* len 16) 11)
		 (bytes->words (map char->integer (string->list obj))))))
       ((symbol? obj)
	(let ((suo-sym (suo:record suo:symbol-type (symbol->string obj))))
	  (set! all-suo-symbols (cons suo-sym all-suo-symbols))
	  (emit suo-sym)
	  ;; register the original OBJ as required
	  (hashq-set! ptr-hash obj (hashq-ref ptr-hash suo-sym))))
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
       ((eq? obj suo-bootinfo-marker)
	#f)
       ((integer? obj)
	(+ (* 4 obj)
	   (cond ((<= 0 obj (1- (expt 2 29))) 0)
		 ((<= (- (expt 2 29)) obj -1) #x100000000)
		 (else
		  (error "integer is not fixnum" obj)))
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
		(emit-words idx (asm all-suo-symbols)))
	      bootinfo-idxs)

    (let ((mem2 (make-u32vector idx)))
      (do ((i 0 (1+ i)))
	  ((= i idx))
	(u32vector-set! mem2 i (u32vector-ref mem i)))
      mem2)))
