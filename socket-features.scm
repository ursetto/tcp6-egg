(use feature-test)

#> #include "socket.h" <#

(declaration-prefix HAVE_)
(registration-prefix "")

(define-foreign-features
  AF_UNIX
  IPV6_V6ONLY
  IPPROTO_IPV6
  )
