(module udp6
  (udp-socket? udp-bound? udp-connected? udp-open-socket
   udp-open-socket* udp-bind! udp-connect! udp-send udp-sendto
   udp-recv udp-recvfrom udp-close-socket udp-bound-port
   ;; udp-set-multicast-interface udp-join-multicast-group
   )

(include "udp6.scm")

)
