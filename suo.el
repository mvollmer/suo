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
;; Suo sends requests, and Emacs sends back events.  Some events
;; happen asynchronously and some are in direct response to a request.
;; Emacs maintains two buffers where it collects requests and queues
;; events.  Suo is allowed to block, but Emacs is not.
;; 
;; Objects are identified by small integers that are assigned by Suo.
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
;; color, face, etc).
;;
;; In addition to outputting text, segments are also the source of
;; input events for Suo.
