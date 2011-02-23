(include "feature-test.scm")

#> #include "socket.h" <#

(definition-prefix HAVE_)
(registration-prefix "")

(defreg-foreign-features
  AF_UNIX)

(write-feature-syntax)
