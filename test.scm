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

(define (write-image mem file)
  (let* ((port (open-output-file file)))
    (uniform-vector-write #u32(#xABCD0001 0 0) port)
    (uniform-vector-write mem port)))

(define (make-bootstrap-image exp file)
  (let ((comp-exp (boot-eval
		   `(compile '(lambda ()
				,exp
				(primop syscall))))))
    (or (constant? comp-exp)
	(error "expected constant"))
    (write-image (dump-object (constant-value comp-exp))
		 file)))

(define (compile-base)
  (image-load "suo-base.scm")
  (make-bootstrap-image (image-expression) "base"))

(define (compile-compiler)
  (image-load "suo-base.scm")
  (image-load "suo-asm-ppc.scm")
  (image-load "suo-util.scm")
  (image-load "suo-compiler.scm")
  (image-load "suo-boot.scm")
  (make-bootstrap-image (image-expression) "compiler"))

(define (compile-minimal)
  (boot-eval '(set! cps-verbose #t))
  (make-bootstrap-image
   '(begin
      (lambda (args body)
	(cons* :lambda args (expand-body body)))
      (bootinfo))
   "minimal"))

;;(compile-base)
(compile-compiler)
;;(compile-minimal)

(boot-eval '(dump-sigs-n-calls))
(check-undefined-variables)
