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

(define-suo (read-line-1 chars)
  (let ((ch (input-char (current-input-port))))
    (cond ((and (not ch) (null? chars))
	   #f)
	  ((or (not ch) (eq? ch #\nl))
	   (reverse chars))
	  (else
	   (read-line-1 (cons ch chars))))))

(define-suo (read-line)
  (and=> (read-line-1 '()) list->string))

(define-suo (test)
  (let ((l (read-line)))
    (cond (l
	   (pk l)
	   (if (eq? (string->symbol l) 'foo)
	       (pk 'foo))
	   (test)))))

(define (all-variables)
  (remove-if-not (lambda (val)
		   (and (suo:record? val)
			(eq? (suo:record-desc val) suo:variable-type)))
		 (map cdr toplevel)))


(define-suo-variable toplevel (all-variables))

(define-suo (lookup-global sym)
  (let loop ((vars toplevel))
    (cond ((null? vars)
	   (begin))
	  ((eq? (record-ref (car vars) 1) sym)
	   (record-ref (car vars) 0))
	  (else
	   (loop (cdr vars))))))

(define-suo (eval exp)
  (cond ((symbol? exp)
	 (lookup-global exp))
	((pair? exp)
	 (let ((vals (map eval exp)))
	   (apply (car vals) (cdr vals))))
	(else
	 exp)))

(define-suo (repl)
  (display "> ")
  (call/v (lambda () (eval (read)))
	  (lambda vals
	    (for-each (lambda (v)
			(write v)
			(newline))
		      vals)))
  (repl))

(write-image (cons (cps-compile '(lambda (syms)
				   (init-ports)
				   (init-print)
				   (init-symbols syms)
				   (repl)
				   (sys:halt)))
		   (list #f suo-bootinfo-marker)))

(for-each (lambda (c)
	    (let ((name (car c))
		  (val (cdr c)))
	      (if (and (suo:record? val)
		       (eq? (suo:record-desc val) suo:variable-type)
		       (eq? (suo:record-ref val 0) (if #f #f)))
		  (pk 'unspec name))))
	  toplevel)
