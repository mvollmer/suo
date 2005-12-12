(use-modules (ice-9 pretty-print)
	     (oop goops)
	     (ice-9 common-list))

(debug-enable 'debug)
(debug-enable 'backtrace)
(read-enable 'positions)
(read-set! keywords 'prefix)

(load "suo-util.scm")

(define-struct foo ()
  a b)

(pk (foo 1 2))

(load "suo-cross.scm")
(load "suo-asm.scm")
(load "suo-compiler.scm")

(define (write-image obj)
  (let* ((port (open-output-file "image"))
	 (mem (assemble-object obj)))
    (pkx mem)
    (uniform-vector-write mem port)))

(define do-exp '(lambda () (do ((i 0 (+ i 1))) ((= i 5)) (pk i))))
(define lambda-exp '(lambda (a) (lambda (b) (+ a b))))

(let ((cps (cps-convert '(lambda ()
			   (letrec ((loop (lambda ()
					    (:primitive syscall 1)
					    (loop))))
			     (loop)
			     (:primitive syscall 0))))))
  (let ((clos (cps-closure-convert cps)))
    (cps-print clos)
    (let ((regs (cps-register-allocate clos)))
      (cps-print regs)
      (write-image (cps-code-generate regs)))))
