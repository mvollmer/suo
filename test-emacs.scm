(define pending-events '())

(define (read-one)
  (let ((form (read)))
    (if (eof-object? form)
	(exit 0))
    (cond ((and (pair? form) (eq? (car form) 'event))
	   (set! pending-events (append pending-events (list (cdr form))))
	   #f)
	  (else
	   form))))

(define (read-response)
  (or (read-one)
      (read-response)))
	
(define (do-request req)
  (write req)
  (newline)
  (let ((res (read-response)))
    (if (and (pair? res) (eq? (car res) 'error))
	(begin 
	  (pk res)
	  (exit))     
	res)))

(define (event-loop)
  (if (null? pending-events)
      (let ((res (read-one)))
	(if res
	    (begin
	      (display ";; unexpected response ")
	      (write res)
	      (newline)))
	(event-loop))
      (let ((ev (car pending-events)))
	(set! pending-events (cdr pending-events))
	(handle-event ev)
	(event-loop))))

(define event-handlers (make-hash-table))

(define (register-handler obj tag handler)
  (hash-set! event-handlers (cons obj tag) handler))

(define (handle-event ev)
  (pk ev)
  (let* ((tag (car ev))
	 (obj (cadr ev))
	 (args (cddr ev))
	 (handler (hash-ref event-handlers (cons obj tag))))
    (if handler
	(apply handler args))))

(define (create-buffer name)
  (do-request `(create-buffer ,name)))

(define (show-buffer buffer)
  (do-request `(show-buffer ,buffer)))

(define (create-segment buffer pos props)
  (do-request `(create-segment ,buffer ,pos ,props)))

(define (hide-segment seg)
  (do-request `(hide-segment ,seg)))

(define (show-segment seg)
  (do-request `(show-segment ,seg)))

(define (set-text segment text)
  (do-request `(set-text ,segment ,text)))

(define (append-text segment text)
  (do-request `(append-text ,segment ,text)))

(define (clear-dirty segment)
  (do-request `(clear-dirty ,segment)))

(define (get-text segment)
  (do-request `(get-text ,segment)))

(define (define-key segment key event)
  (do-request `(define-key ,segment ,key ,event)))

(define (emacs-repl)
  (let* ((buffer     (create-buffer "*suo-repl*"))
	 (transcript (create-segment buffer 0 '(read-only t)))
	 (cmdline    (create-segment buffer 1 '())))

    (define (submit)
      (let ((str (get-text cmdline)))
	(cond ((equal? str "show")
	       (show-segment transcript))
	      ((equal? str "hide")
	       (hide-segment transcript)))
	(append-text transcript (string-append "\n" str))
	(set-text cmdline "")))
      
    (define-key cmdline "RET" 'submit)
    (register-handler cmdline 'key (lambda (arg) (submit)))
    (show-buffer buffer)
    (event-loop)))

(emacs-repl)
