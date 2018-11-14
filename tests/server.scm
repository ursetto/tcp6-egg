;; Simple server that writes a line and reads a response. 
;; No threading is done.
(require-extension tcp6)
(cond-expand 
  (chicken-5 (import (chicken io))
             (import (chicken time posix))
             (import (chicken format)))
  (else (use posix)))

(define l (tcp-listen 4242))
(let loop ()
    (define-values (i o) (tcp-accept l))
    (write-line (sprintf "Server says hello at ~A!" (seconds->string)) 
                o)
    (print (read-line i))
    (close-input-port i)
    (close-output-port o)
    (loop))
