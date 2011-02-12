(module udp6
  (udp-socket? udp-bound? udp-connected? udp-open-socket
   udp-open-socket*
   udp-bind! udp-connect! ;; udp-bind udp-connect
   udp-send udp-sendto
   udp-recv udp-recvfrom udp-close-socket udp-bound-port
   ;; udp-set-multicast-interface udp-join-multicast-group
   )

(import scheme (only chicken use include))
(use socket)
(include "udp6.scm")

)
