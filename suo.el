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
;;     (text TEXT PROP...)
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

;;; Server

(defvar suo-server nil)
(defvar suo-clients nil)
(defvar suo-process nil)

(defun start-suo-server ()
  (interactive)
  (stop-suo-server)
  (setq suo-process (make-network-process :name "suo"
					  :server t
					  :family nil
					  :service 7000
					  :host 'local
					  :sentinel 'suo-sentinel
					  :filter 'suo-filter)))

(defun stop-suo-server ()
  (interactive)
  (if suo-process
      (delete-process suo-process))
  (setq suo-process nil))

(defun suo-sentinel (proc msg)
  (let ((status (process-status proc)))
    ;;(message "Suo: %s (%s)" (substring msg 0 -1) status)
    (cond ((eq status 'open)
	   (suo-client-open proc))
	  ((eq status 'closed)
	   (suo-client-closed (suo-client-from-proc proc))))))

(defun suo-filter (proc string)
  (suo-client-input (suo-client-from-proc proc) string))

;;; Clients

(defvar suo-next-client-id 0)
(defvar suo-clients nil)

(defstruct suo-client
  id proc pending-input)

(defun suo-client-open (proc)
  (let ((id suo-next-client-id))
    (incf suo-next-client-id)
    (message "Suo: client %s connected" id)
    (let ((client (make-suo-client :id id
				   :proc proc
				   :pending-input "")))
      (process-put proc 'suo-client client)
      (if (null suo-clients)
	  (suo-reset-ids))
      (setq suo-clients (cons client suo-clients))
      client)))

(defun suo-client-from-proc (proc)
  (or (process-get proc 'suo-client)
      (error "not a suo process: %s" proc)))

(defun suo-client-closed (client)
  (message "Suo: client %s disconnected" (suo-client-id client))
  (suo-destroy-client-objects client)
  (setq suo-clients (delq client suo-clients))
  (if (null suo-clients)
      (setq suo-next-client-id 0)))

(defun suo-client-input (client string)
  (let* ((input (concat (suo-client-pending-input client) string))
	 (form-and-pos (condition-case err
			   (read-from-string input)
			 (end-of-file
			  nil))))
    (cond (form-and-pos
	   (let ((form (car form-and-pos))
		 (pos (cdr form-and-pos)))
	     (setf (suo-client-pending-input client) (substring input pos))
	     (suo-dispatch client form)))
	  (t
	   (setf (suo-client-pending-input client) input)))))

(defun suo-client-output (client string)
  (process-send-string (suo-client-proc client) string))

;;; Objects and Ids

(defstruct suo-object
  client id)

(defvar suo-next-id 0)
(defvar suo-objects nil)

(defun suo-reset-ids ()
  (setq suo-next-id 0)
  (setq suo-objects (make-hash-table)))

(defun suo-make-id ()
  (prog1
      suo-next-id
    (incf suo-next-id)))
  
(defun suo-put (id client obj)
  (puthash id obj suo-objects)
  (setf (suo-object-id obj) id)
  (setf (suo-object-client obj) client))

(defun suo-get (id)
  (or (gethash id suo-objects)
      (error "no such object: %s" id)))

(defun suo-remove-obj (obj)
  (let ((id (suo-object-id obj)))
    (remhash id suo-objects)))

(defun suo-destroy-client-objects (client)
  (maphash (lambda (id obj)
	     (if (eq (suo-object-client obj) client)
		 (cond ((suo-buffer-p obj)
			(suo-destroy-buffer obj))
		       ((suo-segment-p obj)
			(suo-destroy-segment obj)))))
	   suo-objects))

;;; Requests, responses, and events

(defun suo-respond (client response)
  ;; (message "> %S" response)
  (suo-client-output client (prin1-to-string response))
  (suo-client-output client "\n"))

(defun suo-ok (client)
  (suo-respond client 'ok))

(defun suo-event (obj ev)
  (let ((id (suo-object-id obj)))
    ;; (message "* %s %S" id ev)
    (suo-respond (suo-object-client obj) (list 'event id ev))))

(defun suo-dispatch (client req)
  ;; (message "< %S" req)
  (condition-case err
      (let ((handler (get (car req) 'suo-handler)))
	(or handler
	    (error "unknown request: %s" (car req)))
	(apply handler client (cdr req)))
    (error
     (suo-respond client (list 'error (error-message-string err))))))

(defmacro def-suo-req (name args &rest body)
  (let ((fun (intern (concat "suo-req-" (symbol-name name)))))
    `(progn
       (put ',name 'suo-handler ',fun)
       (defun ,fun ,args ,@body))))

;;; Buffers

(defstruct (suo-buffer (:include suo-object))
  buffer segments keymap)

(defun suo-buffer-killed ()
  (if suo-current-buffer
      (suo-event suo-current-buffer 'killed)))

(defun suo-client-kill ()
  (interactive)
  (delete-process (suo-client-proc (suo-object-client suo-current-buffer))))

(defun suo-create-buffer (name)
  (let* ((keymap (make-sparse-keymap))
	 (emacs-buffer (generate-new-buffer name))
	 (buffer (make-suo-buffer :buffer emacs-buffer :keymap keymap)))
    (with-current-buffer emacs-buffer
      (use-local-map keymap)
      (make-local-variable 'suo-current-buffer)
      (setq suo-current-buffer buffer)
      (add-hook 'kill-buffer-hook 'suo-buffer-killed nil t)
      (make-local-variable 'after-change-functions)
      (setq after-change-functions (cons 'suo-modified-hook
					 after-change-functions))
      (make-local-variable 'indent-line-function)
      (make-local-variable 'suo-mode-name)
      (setq mode-line-format '("Suo: "
			       suo-mode-name
			       mode-line-position)))
    buffer))

(defun suo-show-buffer (buffer)
  (switch-to-buffer (suo-buffer-buffer buffer)))

(defun suo-close-buffer (buffer)
  (kill-buffer (suo-buffer-buffer buffer)))

(defun suo-destroy-buffer (buffer)
  (let ((emacs-buffer (suo-buffer-buffer buffer)))
    (if (buffer-name emacs-buffer)
	(with-current-buffer emacs-buffer
	  (setq buffer-read-only t)
	  (setq suo-current-buffer nil)
	  (rename-buffer (concat (buffer-name emacs-buffer)
				 " (destroyed)")
			 t)))))

;;; Segments

;; Segments are defined by an overlay.  A segment can never be
;; empty, it always ends in a read-only newline character.  This way,
;; the point can never be between segments: placing it before the
;; final newline puts it at the end of the segment that owns the
;; newline, placing it after puts it at the beginning of the next.
;; (Or into never-never land if there is no next segment).
;;
;; Thus, a segment is always at least one line high.  If you want a
;; empty segment to diasappear, you have to hide it explicitly.  (Note
;; that hiding a segment makes it impossible to type into it.)
;;
;; The min and max marker point to the boundaries of the segment,
;; excluding the final newline.  That is, the max marker points just
;; before the final newline.
;;
;; The overlay of the segment includes the final newline.  I't has a
;; 'suo-segment' property that points to the segment structure.  It
;; also has a 'keymap' property that receives the segment specific key
;; bindings.  The overlay also controls visibility and default
;; background color
;;
;; The characters of a read-only segment have a read-only text
;; property of t, naturally.
;;
;; The text properties of the final newline are non-rearsticky and
;; always read-only.  That way, it can not be removed and the segment
;; properties do not bleed into the next segment.  (However, the final
;; newline of the final segment could be made rear-sticky to prevent
;; typing into never never land.)

(defstruct (suo-segment (:include suo-object))
  buffer min max props overlay keymap dirtyp text-props)

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

(defun suo-modified-hook (beg end pre)
  (let ((seg (get-char-property beg 'suo-segment)))
    (cond (seg
	   (cond ((not (suo-segment-dirtyp seg))
		  (suo-set-dirty seg t)
		  (suo-event seg 'dirty)))
	   (let ((inhibit-read-only t)
		 (inhibit-modification-hooks t))
	     (suo-update-text-props seg))))))

(defun get-prop-names (plist)
  (if (null plist)
      nil
    (cons (car plist) (get-prop-names (cddr plist)))))

(defun suo-enter-leave-trampoline (pos func)
  (let ((seg (get-char-property pos 'suo-segment)))
    (if seg
	(funcall func seg))))

(defun suo-make-enter-func (func)
  `(lambda (old new) (suo-enter-leave-trampoline new ',func)))

(defun suo-make-leave-func (func)
  `(lambda (old new) (suo-enter-leave-trampoline old ',func)))

(defun suo-create-segment (buffer pos props)
  (with-current-buffer (suo-buffer-buffer buffer)
    (let* ((segments (suo-buffer-segments buffer))
	   (pos (if (< pos 0) (length segments) pos))
	   (next-seg (if (= pos (length segments))
			 nil
		       (elt segments pos)))
	   (min-marker (make-marker))
	   (max-marker (make-marker))
	   (keymap (make-sparse-keymap))
	   (seg (make-suo-segment :buffer buffer
				  :min min-marker
				  :max max-marker
				  :keymap keymap
				  :dirtyp nil
				  :props props)))

      (set-marker min-marker (if next-seg
				 (suo-segment-min next-seg)
			       (point-max)))
      (set-marker max-marker min-marker)
      (set-marker-insertion-type min-marker nil)
      (set-marker-insertion-type max-marker t)

      (let ((text-props '()))
	(if (plist-get (suo-segment-props seg) 'read-only)
	    (setq text-props (append '(read-only t) text-props)))
	(let* ((mode (plist-get (suo-segment-props seg) 'mode))
	       (enter-func (or (get mode 'suo-enter-func)
			       'suo-plain-mode))
	       (leave-func (or (get mode 'suo-leave-func)
			       'suo-mode-leave)))
	  (setq text-props
		(append `(point-entered ,(suo-make-enter-func enter-func)
			  point-left ,(suo-make-leave-func leave-func))
			text-props))
	  (setf (suo-segment-text-props seg) text-props)))

      (save-excursion
	(goto-char min-marker)
	(insert "\n")
	(if next-seg
	    (progn
	      (set-marker (suo-segment-min next-seg) max-marker)
	      (move-overlay (suo-segment-overlay next-seg)
			    (suo-segment-min next-seg)
			    (+ (suo-segment-max next-seg) 1))))
	(let ((inhibit-read-only t))
	  (put-text-property min-marker max-marker 'rear-nonsticky t)
	  (put-text-property min-marker max-marker
			     'front-sticky (get-prop-names 
					    (suo-segment-text-props seg)))
	  (add-text-properties min-marker max-marker
			       (suo-segment-text-props seg))
	  (put-text-property min-marker max-marker 'read-only t)
	  (set-marker max-marker min-marker)))

      (let ((overlay (make-overlay min-marker (+ max-marker 1)
				   nil nil nil)))
	(setf (suo-segment-overlay seg) overlay)
	(overlay-put overlay 'suo-segment seg)
	(overlay-put overlay 'keymap keymap)
	(overlay-put overlay 'face (plist-get props 'face)))

      (setf (suo-buffer-segments buffer)
	    (insert-into-list (suo-buffer-segments buffer) seg pos))

      seg)))

(defun suo-submerge (attr1 attr2)
  (while (not (null attr2))
    (let ((prop (car attr2))
	  (value (cadr attr2)))
      (if (not (plist-get attr1 prop))
	  (setq attr1 (append attr1 (list prop value))))
      (setq attr2 (cddr attr2))))
  attr1)

(defun suo-submerge-face-attrs (beg end attrs)
  ;; Go over the text between BEG and END and merge ATTRS into the
  ;; face property of each character.  Existing attributes take
  ;; precedence.
  (let ((pos beg))
    (while (< pos end)
      (let ((existing-attrs (get-text-property pos 'face))
	    (next-pos (next-single-property-change pos 'face nil end)))
	(put-text-property pos next-pos 'face (suo-submerge existing-attrs
							    attrs))
	(setq pos next-pos)))))

(defun suo-bind-key (buf-or-seg key)
  (let ((keymap (if (suo-segment-p buf-or-seg)
		    (suo-segment-keymap buf-or-seg)
		  (suo-buffer-keymap buf-or-seg))))
    (define-key keymap (read-kbd-macro key)
      `(lambda ()
	 (interactive)
	 (suo-event ',buf-or-seg ',key)))))

(defun suo-destroy-segment (seg)
  (set-marker (suo-segment-min seg) nil)
  (set-marker (suo-segment-max seg) nil)
  (setf (suo-segment-buffer seg) nil))

(defun suo-hide-segment (seg)
  (overlay-put (suo-segment-overlay seg) 'invisible t))

(defun suo-show-segment (seg)
  (overlay-put (suo-segment-overlay seg) 'invisible nil))

(defun suo-goto-segment (seg)
  (with-current-buffer (suo-buffer-buffer (suo-segment-buffer seg))
    (goto-char (suo-segment-min seg))))

(defun suo-remove-segment (seg)
  (let ((buffer (suo-segment-buffer seg)))
    (with-current-buffer (suo-buffer-buffer buffer)
      (save-excursion
	(let ((next-seg (next-elt (suo-buffer-segments buffer) seg)))
	  (let ((inhibit-read-only t)
		(inhibit-modification-hooks t))
	    (delete-region (suo-segment-min seg)
			   (if next-seg
			       (suo-segment-min next-seg)
			     (point-max))))
	  (suo-destroy-segment seg)
	  (setf (suo-buffer-segments buffer)
		(delq seg (suo-buffer-segments buffer))))))))

(defun suo-insert-structured-text (text)
  (cond ((stringp text)
	 (insert text))
	((eq (car text) 'text)
	 (let ((p (point)))
	   (suo-insert-structured-text (cadr text))
	   (suo-submerge-face-attrs p (point) (cddr text))))
	((eq (car text) 'seq)
	 (mapcar 'suo-insert-structured-text (cdr text)))))

(defun suo-update-text-props (seg)
  (let ((min (suo-segment-min seg))
	(max (+ (suo-segment-max seg) 1)))
    (add-text-properties min max (suo-segment-text-props seg))))

(defun suo-refresh-segment ()
  (interactive)
  (let ((seg (get-char-property (point) 'suo-segment)))
    (if seg
	(with-current-buffer (suo-buffer-buffer (suo-segment-buffer seg))
	  (let ((inhibit-read-only t)
		(inhibit-modification-hooks t))
	    (suo-update-text-props seg)
	    (move-overlay (suo-segment-overlay seg)
			  (suo-segment-min seg)
			  (+ (suo-segment-max seg) 1)))))))
 
(defun suo-set-text (seg text)
  (with-current-buffer (suo-buffer-buffer (suo-segment-buffer seg))
    (save-excursion
      (let ((inhibit-read-only t)
	    (inhibit-modification-hooks t))
	(delete-region (suo-segment-min seg) (suo-segment-max seg))
	(goto-char (suo-segment-min seg))
	(suo-insert-structured-text text)
  	(suo-update-text-props seg)))))

(defun suo-append-text (seg text)
  (with-current-buffer (suo-buffer-buffer (suo-segment-buffer seg))
    (save-excursion
      (let ((inhibit-read-only t)
	    (inhibit-modification-hooks t))
	(goto-char (suo-segment-max seg))
	(suo-insert-structured-text text)
	(suo-update-text-props seg)))))

(defun suo-get-text (seg)
  (with-current-buffer (suo-buffer-buffer (suo-segment-buffer seg))
    (buffer-substring-no-properties (suo-segment-min seg)
				    (suo-segment-max seg))))

(defun suo-get-text-if-dirty (seg)
  (if (suo-segment-dirtyp seg)
      (suo-get-text seg)
    nil))

(defun suo-set-dirty (seg val)
  (when (not (eq (suo-segment-dirtyp seg) val))
    (setf (suo-segment-dirtyp seg) val)
    (suo-mode-refresh seg)))

;;; Segment modes

;; Segment modes are implemented by having one of the modes functions
;; be called when a segment is entered.

(defun suo-generic-mode (seg name)
  (setq suo-mode-name name)
  (force-mode-line-update)
  (let ((active-face (or (and (suo-segment-dirtyp seg)
			      (plist-get (suo-segment-props seg)
					 'active-dirty-face))
			 (plist-get (suo-segment-props seg) 'active-face))))
    (if active-face
	(overlay-put (suo-segment-overlay seg) 'face active-face))))
  
(defun suo-code-mode (seg)
  (setq indent-line-function 'lisp-indent-line)
  (suo-generic-mode seg "code   "))

(put 'code 'suo-enter-func 'suo-code-mode)
(put 'code 'suo-leave-func 'suo-mode-leave)

(defun suo-plain-mode (seg)
  (setq indent-line-function 'indent-relative)
  (suo-generic-mode seg "plain  "))

(put 'plain 'suo-enter-func 'suo-plain-mode)
(put 'plain 'suo-leave-func 'suo-mode-leave)

(defun suo-mode-leave (seg)
  (overlay-put (suo-segment-overlay seg)
	       'face (or (and (suo-segment-dirtyp seg)
			      (plist-get (suo-segment-props seg) 'dirty-face))
			 (plist-get (suo-segment-props seg) 'face))))

(defun suo-mode-refresh (seg)
  (with-current-buffer (suo-buffer-buffer (suo-segment-buffer seg))
    (let* ((current-seg (get-char-property (point) 'suo-segment))
	   (mode (plist-get (suo-segment-props seg) 'mode))
	   (func (get mode (if (eq current-seg seg)
			       'suo-enter-func 
			     'suo-leave-func))))
      (if func
	  (funcall func seg)))))

;;; The protocol
  
(def-suo-req ping (client &rest args)
  (suo-respond client (cons 'pong args)))

(def-suo-req destroy (client id)
  (let ((obj (suo-get id)))
    (cond ((suo-buffer-p obj)
	   (suo-destroy-buffer obj))
	  ((suo-segment-p obj)
	   (suo-destroy-segment obj))
	  (t
	   (error "undestructible: %s" obj)))
    (suo-remove-obj obj)
    (suo-ok client)))

(def-suo-req create-buffer (client name)
  (let ((id (suo-make-id)))
    (suo-put id client (suo-create-buffer name))
    (suo-respond client id)))

(def-suo-req show-buffer (client buffer)
  (suo-show-buffer (suo-get buffer))
  (suo-ok client))

(def-suo-req create-segment (client buffer pos props)
  (let ((id (suo-make-id)))
    (let ((seg (suo-create-segment (suo-get buffer) pos props)))
      (suo-put id client seg)
      (suo-respond client id))))

(def-suo-req hide-segment (client seg)
  (suo-hide-segment (suo-get seg))
  (suo-ok client))

(def-suo-req show-segment (client seg)
  (suo-show-segment (suo-get seg))
  (suo-ok client))

(def-suo-req goto-segment (client seg)
  (suo-goto-segment (suo-get seg))
  (suo-ok client))

(def-suo-req remove-segment (client id)
  (suo-remove-segment (suo-get id))
  (suo-ok client))

(def-suo-req set-text (client seg text)
  (suo-set-text (suo-get seg) text)
  (suo-ok client))

(def-suo-req append-text (client seg text)
  (suo-append-text (suo-get seg) text)
  (suo-ok client))

(def-suo-req get-text (client seg)
  (suo-respond client (suo-get-text (suo-get seg))))

(def-suo-req get-text-if-dirty (client seg)
  (suo-respond client (suo-get-text-if-dirty (suo-get seg))))

(def-suo-req set-dirty (client seg val)
  (suo-set-dirty (suo-get seg) val)
  (suo-ok client))

(def-suo-req bind-key (client seg key)
  (suo-bind-key (suo-get seg) key)
  (suo-ok client))
