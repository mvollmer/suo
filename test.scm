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

(define-suo (fac n)
  (if (= n 1)
      n
      (* n (fac (- n 1)))))

(define-suo (reduce f i l)
  (if (pair? l)
      (f (car l) (reduce f i (cdr l)))
      i))

(define-suo (iota n)
  (if (zero? n)
      '()
      (cons n (iota (- n 1)))))

(define-suo (tarai x y z)
  (if (<= x y)
      y
      (tarai (tarai (- x 1) y z)
	     (tarai (- y 1) z x)
	     (tarai (- z 1) x y))))

(set! cps-verbose #t)

(define-suo (list . elts)
  elts)

(write-image (cons (cps-compile '(lambda ()
				   (pk (list 1))
				   (:primop syscall)))
		   (list #f)))
