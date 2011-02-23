(include "feature-test.scm")

#> #include "socket.h" <#

(declaration-prefix HAVE_)
(registration-prefix "")

(define-foreign-features
  AF_UNIX)

(write-feature-syntax)
