(require-extension tcp6)
(cond-expand
  (chicken-5 (import (chicken io)))
  (else))

(define-values (i o) (tcp-connect "localhost" 4242))
(write-line "Goodbye!" o)
(print (read-line i))
