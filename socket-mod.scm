(module socket
(af/inet
 af/inet6 af/unix af/unspec
 sock/stream sock/dgram sock/raw ;; sock/seqpacket
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

 socket-receive! socket-receive-from! socket-receive socket-receive-from
 socket-receive-ready? socket-accept-ready?
 socket-send! socket-send-all! socket-send-to!
 socket-send-size socket-send-buffer-size socket-receive-buffer-size

 ;; ports
 socket-i/o-ports
 socket-i/o-port->socket
 socket-abandon-port!

 ;; misc
 parse-inet-address


 ;; socket options
  so/reuseaddr so/debug so/acceptconn so/keepalive so/dontroute
  so/broadcast so/linger so/oobinline so/sndbuf so/rcvbuf
  so/sndlowat so/rcvlowat so/sndtimeo so/rcvtimeo so/error so/type
  tcp/nodelay
  ip/options ip/hdrincl ip/tos ip/ttl ip/recvopts ip/recvretopts ip/retopts
  ip/multicast-if ip/multicast-ttl ip/multicast-loop ip/add-membership ip/drop-membership
  ipv6/v6only
  sol/socket ipproto/ip ipproto/ipv6 ipproto/icmp
  get-socket-option set-socket-option!
  so-reuse-address? so-debug? so-error so-type
  so-accept-connections? so-keep-alive? so-dont-route? so-broadcast? so-oob-inline?
  so-send-buffer so-receive-buffer so-send-low-water so-receive-low-water
  tcp-no-delay?
  ip-header-included? ip-type-of-service ip-time-to-live
  ipv6-v6-only?

 
)

(import (only chicken include))
(include "socket.scm")

)
