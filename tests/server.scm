(require-extension tcp6)
(cond-expand 
  (chicken-5 (import (chicken io)))
  (else))

(define l (tcp-listen 4242))
(define-values (i o) (tcp-accept l))
(write-line "Hello!" o)
(print (read-line i))
(close-input-port i)
(close-output-port o)
