(use-modules (ice-9 pretty-print)
	     (oop goops)
	     (ice-9 common-list))

(debug-enable 'debug)
(debug-enable 'backtrace)
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

(define-suo (*+ a b c)
  (if (zero? a)
      c
      (*+ (- a 1) b (+ c b))))

(define-suo (* a b)
  (*+ a b 0))

(define-suo (fac n)
  (if (= n 1)
      n
      (* n (fac (- n 1)))))

(write-image (cps-compile '(lambda ()
			     (pk (fac 20))
			     (:primop syscall))))
