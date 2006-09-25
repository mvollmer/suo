(define base #x10000)

(define b+ 536870912)

(define (->limbs x)
  (if (zero? x)
      '()
      (cons (remainder x base)
	    (->limbs (quotient x base)))))

(define (limbs-> x)
  (if (null? x)
      0
      (+ (car x)
	 (* base (limbs-> (cdr x))))))

(define (bignum-length b)
  (length (->limbs b)))

(define (bignum-ref b i)
  (let ((l (->limbs b)))
    (if (>= i (length l))
	0
	(list-ref l i))))

(define (quot-fixnum2 q1 q2 v)
  (pk 'quot-fixnum2 q1 q2 v)
  (quotient (+ (* q1 base) q2) v))

(define (quot-bignum q v)
  
  (define (quot1 q q-n v v-n)
    (pk 'quot1 (->limbs q) q-n (->limbs v) v-n)
    ;; compute one limb of q/v.
    ;; (q[n-1] q[n-2]) / v[n-1] is less than b.
    (let* ((ww (quot-fixnum2 (bignum-ref q (- q-n 1))
			     (bignum-ref q (- q-n 2))
			     (bignum-ref v (- v-n 1))))
	   (v-shift (* v (expt base (- q-n v-n 1))))
	   (r (* ww v-shift)))
      (let loop ((ww ww)
		 (r r))
	(pk 'ww? ww)
	(if (> r q)
	    (loop (- ww 1) (- r v-shift))
	    (cons ww (- q r))))))

  (pk 'q (->limbs q))
  (pk 'v (->limbs v))

  (let* ((v-n (bignum-length v))
	 (q-n (+ (bignum-length q) 1))
	 (w (make-list (max 1 (- q-n v-n)) 0)))
    (let loop ((q-n q-n)
	       (q q))
      (pk 'q (->limbs q) 'w w)
      (if (<= q-n v-n)
	  (cons (limbs-> w) q)
	  (let* ((vals (quot1 q q-n v v-n))
		 (ww (car vals))
		 (q (cdr vals)))
	    (pk 'ww ww)
	    (list-set! w (- q-n v-n 1) ww)
	    (loop (1- q-n) q))))))

(define (quot-bignum2 q v)
  (let ((d (quotient (1- base) (bignum-ref v (1- (bignum-length v))))))
    (pk 'd d)
    (let ((vals (quot-bignum (* q d) (* v d))))
      (cons (car vals) (/ (cdr vals) d)))))

;; worst case for base 100
;; (quot-bignum 1999900 199)
