(module tcp6

(tcp-close
 tcp-listen tcp-connect tcp-connect/ai
 tcp-accept tcp-accept-ready? tcp-listener? tcp-addresses
 tcp-abandon-port tcp-port-numbers
 tcp-listener-port tcp-listener-fileno tcp-listener-socket
 tcp-buffer-size
 tcp-read-timeout tcp-write-timeout tcp-accept-timeout tcp-connect-timeout
 tcp-port->socket
 tcp-bind-ipv6-only)

(import scheme)
(cond-expand
 (chicken-4 (import (only chicken include)))
 (else (import (chicken base))))
(include "tcp6.scm")
)
