(use-modules (oop goops)
	     ;;(srfi srfi-39)
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

(suo:load-for-build "suo-base.scm")
(suo:load-for-build "suo-asm.scm")
(suo:load-for-build "suo-asm-ppc.scm")
(suo:load-for-build "suo-util.scm")
(suo:load-for-build "suo-compiler.scm")

(import-build-types)

(suo:load-for-image "suo-base.scm")
(suo:load-for-image "suo-asm.scm")
;;(suo:load-for-image "suo-asm-ppc.scm")
(suo:load-for-image "suo-boot.scm")

;;(suo:load-for-image "suo-test.scm")

(define (write-image mem)
  (let* ((port (open-output-file "image")))
    (uniform-vector-write mem port)))

(define (make-bootstrap-image exp)
  (let ((comp-exp (suo:eval-for-build
		   `(compile '(lambda ()
				,exp
				(primop syscall))))))
    (or (and (pair? comp-exp) (eq? (car comp-exp) :quote))
	(error "expected quote expression"))
    (write-image
     (suo:eval-for-build 
      `(dump-object (cons ',(cadr comp-exp) (list #f)))))))

(make-bootstrap-image (pk 'top (suo:toplevel-expression)))
