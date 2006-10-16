(use-modules (oop goops)
	     (srfi srfi-39)
	     (ice-9 pretty-print)
	     (oop goops)
	     (ice-9 common-list))

(debug-enable 'debug)
(debug-enable 'backtrace)
(debug-set! stack 2000000)
(read-enable 'positions)
(read-set! keywords 'prefix)

(set! pk (lambda args
	   (display ";;;")
	   (for-each (lambda (elt)
		       (display " ")
		       (write elt))
		     args)
	   (newline)
	   (car (last-pair args))))

(load "suo-cross.scm")

(boot-load "suo-base.scm")
(boot-load "suo-asm-ppc.scm")
(boot-load "suo-util.scm")
(boot-load "suo-compiler.scm")

(define (write-image mem)
  (let* ((port (open-output-file "image")))
    (uniform-vector-write #u32(#xABCD0001 0 0) port)
    (uniform-vector-write mem port)))

(define (make-bootstrap-image exp)
  (let ((comp-exp (boot-eval
		   `(compile '(lambda ()
				,exp
				(primop syscall))))))
    (or (constant? comp-exp)
	(error "expected constant"))
    (write-image
     (dump-object (constant-value comp-exp)))))

(define (compile-compiler)
  (image-load "suo-base.scm")
  (image-load "suo-asm-ppc.scm")
  (image-load "suo-util.scm")
  (image-load "suo-compiler.scm")
  (image-load "suo-boot.scm")
  (make-bootstrap-image (image-expression)))

(define (compile-minimal)
  (boot-eval '(set cps-verbose #t))
  (make-bootstrap-image
   '(begin
      (primop syscall 9 -6 (lambda ()
 			     (primop syscall 0 255)
 			     (primop syscall)))
      (let loop ()
	((lambda a a) 1 2 3 4 5 6 7)
	(loop))
      12)))

(compile-compiler)
;;(compile-minimal)

(boot-eval '(dump-sigs-n-calls))
(check-undefined-variables)
