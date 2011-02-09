(module tcp6

(tcp-close
 tcp-listen tcp-connect tcp-connect/ai
 tcp-accept tcp-accept-ready? tcp-listener? tcp-addresses
 tcp-abandon-port tcp-port-numbers
 tcp-listener-port tcp-listener-fileno tcp-listener-socket
 tcp-buffer-size
 tcp-read-timeout tcp-write-timeout tcp-accept-timeout tcp-connect-timeout

 tcp-bind-ipv6-only)

(import scheme (only chicken include use))
(use socket)
(include "tcp6.scm")
)
