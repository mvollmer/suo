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

(define-suo (test)
  (let ((ch (input-char (current-input-port))))
    (cond (ch
	   (pk ch)
	   (test)))))
	   
(write-image (cons (cps-compile '(lambda ()
				   (init-ports)
				   (let ((t (make-record-type 3 "foo")))
				     (pk (record t 1 2 3)))
				   (sys:halt)))
		   (list #f)))

(for-each (lambda (c)
	    (let ((name (car c))
		  (val (cdr c)))
	      (if (and (suo:record? val)
		       (eq? (suo:record-desc val) suo:variable-descriptor)
		       (eq? (suo:record-ref val 0) (if #f #f)))
		  (pk 'unspec name))))
	  toplevel)
