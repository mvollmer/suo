;;;; suo.el - Emacs as a high level display server for Suo

;; This file implements a simple and very high level protocol for
;; using Emacs as a display server for Suo.  That is, instead of using
;; the X11 protocol to present it's interface to the user, Suo
;; connects to Emacs and uses the protocol implemented here.  (The
;; plan is that Suo will at one point be able to speak the X11
;; protocol...)
;;
;; The protocol is geared towards two main features: interacting in a
;; repl and edititing books.  The repl offers editing of a single Suo
;; form and the display of a transcript.  Books are documents of text
;; and code that are stored inside Suo and edited in Emacs.

;;; Concepts

;; The protocol consists of an exchange of s-expressions, restricted
;; to a common subset that both Emacs and Suo can understand easily.
;;
;; Suo sends requests, and Emacs sends back responses.
;;
;; There are also asynchronous events that can be send by Emacs at any
;; time.
;; 
;; Objects are identified by small integers that are chosen by Emacs.
;;
;; Objects can be buffers or segments.  (You might think of buffers as
;; 'screens' and segments as 'windows'.)
;;
;; As usual, all text is displayed inside an Emacs buffer.  The
;; protocol allows the creation and destruction of buffers.
;;
;; A buffer contains a sequence of segments.  Segments can not be
;; nested.  Segments contain text that has no further structure as far
;; as Emacs is concerned (except that it may have properties such as
;; color, face, etc).  Segments are also the source of input events
;; for Suo.
;;
;; There protocol requests for creation and destruction of segments,
;; to modify their text, etc.
;;
;; A segment is normally read-only, in which case Emacs does not allow
;; changes to its text.  When it is read-write, Emacs allows changes
;; and will maintain a 'dirty' flag for the segment.  Emacs can emit
;; an event when the segment becomes dirty, and Suo can request its
;; contents and optionally reset the dirty flag.  Suo can also ask for
;; all dirty segments of a buffer.
;;
;; A segment can have a depth associated with it.  Emacs uses this to
;; display an outline of the segments in a buffer, and for
;; showing/hiding segments.
;;
;; A segment can have a Emacs major mode associated with it.  This
;; mode is used to edit its content.
;;
;; Read/write segments can either be 'submit'-style or 'commit'-style.
;; Submit segments will send an event when the user presses RET in
;; them, commit segments send an event when C-c C-c is pressed.
;;
;; There is no support for 'live' text in a Suo segment yet, but that
;; might be added.  (Live text would be able to emit events when the
;; user interacts with it.  Also, it could remember what Suo object it
;; represents, to allow direct manipulation of them.)

;;; Protocol

;; Protocol requests are s-expressions encoded in ASCII.  Comments are
;; allowed.  When the connection is closed, all objects are destroyed,
;; but Emacs rescues the buffers.
;;
;; Every request has a response, and every request might produce a
;; error response of this form:
;;
;;   (error STRING)
;;
;; If no response form is indicated below, the it will be just the
;; symbol 'ok'.
;;
;; - (destroy ID)
;; 
;;   Do whatever is needed to destroy the object with identifier ID.
;;   If the object is still in use (i.e., a buffer that has not been
;;   closed yet or a segment that has not been removed yet), the
;;   contents of the object is saved so that the user can recover it.
;;
;; > (create-buffer NAME)
;; < ID
;;
;;   Create a new buffer with name NAME.  The buffer is initially
;;   empty (and no editing can be performed in it).
;;
;; > (create-segment BUFFER POS PROPS TEXT)
;; < ID
;;
;;   Creates a new segment in BUFFER at POS, with PROPS and TEXT.  A
;;   POS of -1 appends the segment at the end.  See 'insert-text' for
;;   a description of TEXT.
;;
;;   PROPS is a list with the following possible elements:
;;
;;     submit       - make this segment 'submit'-style editable
;;     commit       - make this segment 'commit'-style editable
;;     (depth . N)  - segment has depth N
;;     (mode . SYM) - the segment gets Emacs mode SYM for editing
;;
;;   If neither 'submit' nor 'commit' is specified, the segment is
;;   read-only.
;; 
;; > (move-segment SEGMENT POS)
;; < ok
;;
;;   Move segment SEGMENT to be at POS.  When POS is -1 it is moved to
;;   the end of its buffer.
;;
;; > (set-text SEGMENT TEXT)
;; > (append-text SEGMENT TEXT)
;; < ok
;;
;;   Set the text of segment SEGMENT to TEXT, or append it at the end.
;;
;;   TEXT can be a string, or it can be a list of the forms
;;
;;     (text STRING PROP...)
;;     (seq TEXT...)
;;
;;   PROP can be any text property that Emacs understands.
;;
;; > (get-text SEGMENT)
;; < STRING
;;
;;   Returns the text of SEGMENT as a string.  All properties are
;;   lost.
;;
;; > (clear-dirty SEGMENT)
;; < ok
;;
;;   Resets the dirty flag of SEGMENT.
;;
;; Events
;;
;;     (dirty SEGMENT)     - The dirty flag of SEGMENT has been raised.
;;     (destroyed BUFFER)  - A buffer has been deleted by the user.
;;     (submit SEGMENT)    - A segment has been submitted.

(require 'cl)

(defgroup suo nil
  "Emacs as the Suo display server."
  :group 'scheme)

;;; Running the sub-process

;(setq suo-command "netcat -l -p 6666 localhost")
(setq suo-command "guile -s test-emacs.scm")

(defvar suo-process nil)
(defvar suo-buffer nil)
(defvar suo-read-marker nil)

(defun run-suo ()
  (interactive)
  (if suo-process
      (error "suo already running"))
  (suo-reset-ids)
  (setq suo-buffer (get-buffer-create "*suo-process*"))
  (with-current-buffer suo-buffer
    (delete-region (point-min) (point-max))
    (setq suo-read-marker (point-marker)))
  (setq suo-process (start-process-shell-command "suo" suo-buffer
						 suo-command))
  (set-process-sentinel suo-process 'suo-sentinel)
  (set-process-filter suo-process 'suo-filter))

(defun kill-suo ()
  (interactive)
  (if suo-process
      (signal-process suo-process 15)))

(defun suo-sentinel (proc msg)
  (message "Suo: %s" msg)
  (suo-destroy-all-objects)
  (suo-reset-ids)
  (setq suo-process nil))

(defun suo-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (save-excursion
      (goto-char (process-mark proc))
      (insert string)
      (set-marker (process-mark proc) (point))))
  (suo-dispatch (process-buffer proc)))

(defun suo-read-form ()
  (condition-case err
      (with-current-buffer suo-buffer
	(save-excursion
	  (goto-char suo-read-marker)
	  (prog1
	      (read buffer)
	    (set-marker suo-read-marker (point)))))
      (end-of-file
       nil)))

;;; Requests, responses, and events

(defun suo-respond (response)
  (message "> %S" response)
  (process-send-string suo-process (prin1-to-string response))
  (process-send-string suo-process "\n"))

(defun suo-ok ()
  (suo-respond 'ok))

(defun suo-event (ev)
  (message "* %S" ev)
  (suo-respond (cons 'event ev)))

(defun suo-dispatch (buffer)
  (let (req)
    (while (setq req (suo-read-form))
      (message "< %S" req)
      (condition-case err
	  (let ((handler (get (car req) 'suo-handler)))
	    (or handler
		(error "unknown request: %s" (car req)))
	    (apply handler (cdr req)))
	(error
	 (suo-respond (list 'error (error-message-string err))))))))

(defmacro def-suo-req (name args &rest body)
  (let ((fun (intern (concat "suo-req-" (symbol-name name)))))
    `(progn
       (put ',name 'suo-handler ',fun)
       (defun ,fun ,args ,@body))))

;;; Objects and Ids

(defvar suo-next-id 0)
(defvar suo-objects nil)
(defvar suo-ids nil)

(defun suo-reset-ids ()
  (setq suo-next-id 0)
  (setq suo-objects (make-hash-table))
  (setq suo-ids (make-hash-table)))

(defun suo-make-id ()
  (prog1
      suo-next-id
    (incf suo-next-id)))
  
(defun suo-put (id obj)
  (puthash id obj suo-objects)
  (puthash obj id suo-ids))

(defun suo-get (id)
  (or (gethash id suo-objects)
      (error "no such object: %s" id)))

(defun suo-get-id (obj)
  (gethash obj suo-ids))

(defun suo-remove-id (id)
  (let ((obj (suo-get id)))
    (remhash id suo-objects)
    (remhash obj suo-ids)))

(defun suo-destroy-all-objects ()
  (maphash (lambda (id obj)
	     (cond ((bufferp obj)
		    (suo-destroy-buffer obj))
		   ((suo-segment-p obj)
		    (suo-destroy-segment obj))))
	   suo-objects))

;;; Buffers

(defun suo-buffer-killed ()
  (let ((id (suo-get-id (current-buffer))))
    (if id
	(suo-event `(killed ,id)))))

(defun suo-create-buffer (name)
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (add-hook 'kill-buffer-hook 'suo-buffer-killed nil t)
      (make-local-variable 'suo-segments)
      (setq suo-segments '()))
    buffer))

(defun suo-show-buffer (buffer)
  (switch-to-buffer buffer))

(defun suo-close-buffer (buffer)
  (kill-buffer buffer))

(defun suo-destroy-buffer (buffer)
  (if (buffer-name buffer)
      (with-current-buffer buffer
	(rename-buffer (concat (buffer-name buffer) " (destroyed)") t))))

;;; Segments

(defstruct suo-segment
  buffer min max over keymap dirtyp props)

(defun insert-into-list (list elt pos)
  (if (= pos 0)
      (cons elt list)
    (setcdr (nthcdr (- pos 1) list)
	    (cons elt (nthcdr pos list)))
    list))

(defun next-elt (list elt)
  (do ((l list (cdr l)))
      ((or (null l)
	   (eq (car l) elt))
       (if (cdr l) (cadr l) nil))))

;; Segments are separated by at least one character.  The max marker
;; of the first segment points to the first character of the separator
;; and has a insertion type of t.  The min marker of the second
;; segment points at the first character after the separator and has a
;; insertion type of nil.

(defconst suo-segment-separator "\n")

(defun suo-segment-modified (ov afterp beg end &optional pre)
  (if afterp
      (let ((seg (overlay-get ov 'suo-segment)))
	(cond ((and seg (not (suo-segment-dirtyp seg)))
	       (message "dirty")
	       (setf (suo-segment-dirtyp seg) t)
	       (let ((id (suo-get-id seg)))
		 (if id
		     (suo-event `(dirty ,id)))))))))

(defun suo-commit ()
  (interactive)
  (message "commit"))

(defun suo-create-segment (buffer pos props)
  (with-current-buffer buffer
    (let* ((pos (if (< pos 0) (length suo-segments) pos))
	   (next-seg (if (= pos (length suo-segments))
			 nil
		       (elt suo-segments pos)))
	   (min-marker (make-marker))
	   (max-marker (make-marker))
	   (seg (make-suo-segment :buffer buffer
				  :min min-marker
				  :max max-marker
				  :dirtyp nil
				  :props props)))

      (set-marker min-marker (if next-seg
				 (suo-segment-min next-seg)
			       (point-max)))
      (set-marker max-marker min-marker)
      (set-marker-insertion-type min-marker nil)
      (set-marker-insertion-type max-marker t)

      (save-excursion
	(goto-char min-marker)
	(insert suo-segment-separator)
	(add-text-properties min-marker (- max-marker 1)
			     '(face highlight
			       read-only t))
	(add-text-properties (- max-marker 1) max-marker
			     '(face highlight
			       read-only t
			       rear-nonsticky t))
	(if next-seg
	    (set-marker (suo-segment-min next-seg) max-marker))
	(set-marker max-marker min-marker)

	(let ((o (make-overlay min-marker (+ max-marker 1)
			       (suo-segment-buffer seg)
			       nil nil))
	      (k (make-sparse-keymap)))
	  (overlay-put o 'suo-segment seg)
	  (overlay-put o 'modification-hooks (list 'suo-segment-modified))
	  (overlay-put o 'keymap k)
	  (setf (suo-segment-over seg) o)
	  (setf (suo-segment-keymap seg) k)))

      (suo-segment-apply-props seg)

      (setq suo-segments (insert-into-list suo-segments seg pos))

      seg)))

(defun suo-define-key (seg key event)
  (define-key (suo-segment-keymap seg) (read-kbd-macro key)
    `(lambda ()
       (interactive)
       (suo-event '(key ,(suo-get-id seg) ,event)))))

(defun suo-destroy-segment (seg)
  (set-marker (suo-segment-min seg) nil)
  (set-marker (suo-segment-max seg) nil)
  (setf (suo-segment-buffer seg) nil))

(defun suo-hide-segment (seg)
  (overlay-put (suo-segment-over seg) 'invisible t))

(defun suo-show-segment (seg)
  (overlay-put (suo-segment-over seg) 'invisible nil))

(defun suo-segment-apply-props (seg)
  (if (plist-get (suo-segment-props seg) 'read-only)
      (add-text-properties (suo-segment-min seg)
			   (suo-segment-max seg)
			   '(read-only t))))

(defun suo-remove-segment (seg)
  (with-current-buffer (suo-segment-buffer seg)
    (save-excursion
      (let ((next-seg (next-elt suo-segments seg)))
	(let ((inhibit-read-only t))
	  (delete-region (suo-segment-min seg)
			 (if next-seg
			     (suo-segment-min next-seg)
			   (point-max))))
	(suo-segment-destroy seg)
	(setq suo-segments (delq seg suo-segments))))))

(defun suo-set-text (seg text)
  (with-current-buffer (suo-segment-buffer seg)
    (save-excursion
      (let ((inhibit-read-only t))
	(delete-region (suo-segment-min seg) (suo-segment-max seg))
	(goto-char (suo-segment-min seg))
	(insert text)
	(suo-segment-apply-props seg)))))

(defun suo-append-text (seg text)
  (with-current-buffer (suo-segment-buffer seg)
    (save-excursion
      (let ((inhibit-read-only t))
	(goto-char (suo-segment-max seg))
	(insert text)
	(suo-segment-apply-props seg)))))

(defun suo-get-text (seg)
  (with-current-buffer (suo-segment-buffer seg)
    (buffer-substring (suo-segment-min seg)
		      (suo-segment-max seg))))

(defun suo-clear-dirty (seg)
  (setf (suo-segment-dirtyp seg) nil))

;;; The protocol
  
(def-suo-req ping (&rest args)
  (suo-respond (cons 'pong args)))

(def-suo-req destroy (id)
  (let ((obj (suo-get id)))
    (cond ((bufferp obj)
	   (suo-destroy-buffer obj))
	  ((suo-segment-p obj)
	   (suo-destroy-segment obj))
	  (t
	   (error "undestructible: %s" obj)))
    (suo-remove-id id)
    (suo-ok)))

(def-suo-req create-buffer (name)
  (let ((id (suo-make-id)))
    (suo-put id (suo-create-buffer name))
    (suo-respond id)))

(def-suo-req show-buffer (buffer)
  (suo-show-buffer (suo-get buffer))
  (suo-ok))

(def-suo-req create-segment (buffer pos props)
  (let ((id (suo-make-id)))
    (let ((seg (suo-create-segment (suo-get buffer) pos props)))
      (suo-put id seg)
      (suo-respond id))))

(def-suo-req hide-segment (seg)
  (suo-hide-segment (suo-get seg))
  (suo-ok))

(def-suo-req show-segment (seg)
  (suo-show-segment (suo-get seg))
  (suo-ok))

(def-suo-req remove-segment (id)
  (suo-remove-segment (suo-get id))
  (suo-ok))

(def-suo-req set-text (seg text)
  (suo-set-text (suo-get seg) text)
  (suo-ok))

(def-suo-req append-text (seg text)
  (suo-append-text (suo-get seg) text)
  (suo-ok))

(def-suo-req get-text (seg)
  (suo-respond (suo-get-text (suo-get seg))))

(def-suo-req clear-dirty (seg)
  (suo-clear-dirty (suo-get seg))
  (suo-ok))

(def-suo-req define-key (seg key event)
  (suo-define-key (suo-get seg) key event)
  (suo-ok))
