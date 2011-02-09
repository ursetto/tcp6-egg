(module socket
(af/inet
 af/inet6 af/unix
 sock/stream sock/dgram sock/raw
 ipproto/tcp ipproto/udp
 ai/canonname ai/numerichost ai/passive
 ni/numerichost ni/numericserv ni/dgram ni/namereqd ni/nofqdn

 integer->address-family integer->protocol-type integer->socket-type
 address-family->integer protocol-type->integer socket-type->integer

 sockaddr-family sockaddr-address sockaddr-port sockaddr-path sockaddr->string

 address-information name-information inet-address
 addrinfo-flags addrinfo-family addrinfo-socktype addrinfo-protocol
 addrinfo-address addrinfo-canonname addrinfo?

 socket-connect-timeout socket-receive-timeout socket-send-timeout socket-accept-timeout
 socket socket-fileno socket-family socket-type socket-protocol socket?
 socket-connect! socket-connect/ai
 socket-bind!
 socket-listen!
 socket-accept
 socket-shutdown! shut/rd shut/wr shut/rdwr
 socket-close!
 socket-name socket-peer-name

 socket-receive! socket-receive-from!
 socket-receive-ready? socket-accept-ready?
 socket-send! socket-send-all! socket-send-to!
 socket-send-size socket-send-buffer-size socket-receive-buffer-size

 ;; options
 set-socket-reuseaddr!     ;; temp, for tcp
 set-socket-v6only!        ;; temp, for tcp

 ;; ports
 socket-i/o-ports
 socket-i/o-port->socket
 socket-abandon-port!

 ;; misc
 parse-inet-address
)

(import (only chicken include))
(include "socket.scm")

)
