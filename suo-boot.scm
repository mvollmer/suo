(display "Hello, Suo!")
(newline)

(define big+ #x806e0000)
;;(define big- -2154692608)

(define (<< a n)
  (bignum-shift-limbs a n))

(define (fac n)
  (if (< n 3) n (* n (fac (1- n)))))

(define count 0)

(define (test val1 val2)
  (if (= val1 val2)
      (begin 
	(set! count (1+ count))
	(pk 'PASS count)
	(values))
      (error "FAIL" val1 val2)))

(define (loop)
  (:primitive get-reg (res) (-7)
	      ((:primitive set-reg (unused) (-7 #f)
			   ((if res (res))))))
  (loop))

(define (loop)
  (:primitive get-reg (res) (-7)
	      ((if res (res))))
  (loop))

(set-wrong-num-args-hook)
(repl)
