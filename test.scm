(use-modules (ice-9 pretty-print)
	     (oop goops)
	     (ice-9 common-list))

(debug-enable 'debug)
(debug-enable 'backtrace)
(debug-set! stack 200000)
(read-enable 'positions)
(read-set! keywords 'prefix)

(load "suo-util.scm")
(load "suo-cross.scm")
(load "suo-asm-ppc.scm")
(load "suo-compiler.scm")
(load "suo-base.scm")

(define (write-image obj)
  (let* ((port (open-output-file "image"))
	 (mem (assemble-object obj)))
    (uniform-vector-write mem port)))

(define-suo (iota n)
  (if (zero? n)
      '()
      (cons n (iota (- n 1)))))

(define-suo (fac n)
  (apply * (iota n)))

(define-suo (tarai x y z)
  (if (<= x y)
      y
      (tarai (tarai (- x 1) y z)
	     (tarai (- y 1) z x)
	     (tarai (- z 1) x y))))

(define-suo (loop x)
  (loop (+ x 1)))

(define-suo (test)
  (call/cc (lambda (k)
	     (pk 1)
	     (k 2)
	     (pk 2)
	     (pk 3)
	     4)))

(set! cps-verbose #f)

(define-suo (call/v producer consumer)
  (:call/v producer consumer))

(define-suo (values . args)
  (call/cc (lambda (k) (apply k args))))

(set! cps-verbose #f)

(write-image (cons (cps-compile '(lambda ()
				   (:primop syscall 2 1 "Hello, World\n" 0 13)
				   (:primop syscall)))
		   (list #f)))
