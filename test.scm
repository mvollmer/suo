(debug-enable 'debug)
(debug-enable 'backtrace)
(read-enable 'positions)

(load "suo-cross.scm")
(load "suo-asm.scm")

(define suo:false (assemble-object #f #t))

(let* ((mem (make-u32vector 16 suo:false))
       (port (open-output-file "image"))
       (code-1 (assemble-instructions '(sys '0 (go (reg 1)))))
       (code-2 (assemble-instructions `(copy ',code-1 (reg 1) (go (reg 1)))))
       (ptr (assemble-object code-2 mem)))
  (pkx mem)
  (let ((box (u32vector ptr)))
    (uniform-vector-write box port)
    (uniform-vector-write mem port)))
