(module socket
(af/inet
 af/inet6 af/unix af/unspec
 sock/stream sock/dgram sock/raw ;; sock/seqpacket
 ipproto/tcp ipproto/udp
 ai/canonname ai/numerichost ai/passive
 ai/numericserv ai/addrconfig ai/v4mapped ai/all ai/mask ai/default  ;; 0 if not implemented
 ni/numerichost ni/numericserv ni/dgram ni/namereqd ni/nofqdn

 integer->address-family integer->protocol-type integer->socket-type
 address-family->integer protocol-type->integer socket-type->integer

 sockaddr? sockaddr-family sockaddr-address
 sockaddr-port sockaddr-path sockaddr->string

 address-information name-information inet-address unix-address
 addrinfo-flags addrinfo-family addrinfo-socktype addrinfo-protocol
 addrinfo-address addrinfo-canonname addrinfo?

 socket-connect-timeout socket-receive-timeout socket-send-timeout socket-accept-timeout
 socket socket-fileno socket-family socket-type socket-protocol socket?
 socket-connect socket-connect/ai
 socket-bind
 socket-listen
 socket-accept socket-accept-ready?
 socket-shutdown shut/rd shut/wr shut/rdwr
 socket-close socket-close*
 socket-name socket-peer-name

 socket-receive! socket-receive-from! socket-receive socket-receive-from
 socket-receive-ready?
 socket-send socket-send-all socket-send-to
 socket-send-size socket-send-buffer-size socket-receive-buffer-size

 ;; ports
 socket-i/o-ports
 socket-i/o-port->socket
 socket-abandon-port

 ;; misc
 parse-inet-address

 ;; socket options
 so/reuseaddr so/debug so/acceptconn so/keepalive so/dontroute
 so/broadcast so/linger so/oobinline so/sndbuf so/rcvbuf
 so/sndlowat so/rcvlowat so/sndtimeo so/rcvtimeo so/error so/type
 so/useloopback so/reuseport so/timestamp
 so/exclusiveaddruse ;win
 
 tcp/nodelay tcp/maxseg tcp/nopush tcp/noopt tcp/keepalive

 ip/options ip/hdrincl ip/tos ip/ttl ip/mtu ip/mtu-discover
 ip/pktinfo ip/recverr ip/recvtos ip/recvttl ip/router-alert
 ip/recvopts ip/recvretopts ip/retopts ip/recvdstaddr
 ip/multicast-if ip/multicast-ttl ip/multicast-loop
 ip/add-membership ip/drop-membership

 ipv6/v6only ipv6/addrform ipv6/mtu ipv6/mtu-discover
 ipv6/multicast-hops ipv6/multicast-if ipv6/multicast-loop ipv6/pktinfo 
 ipv6/rthdr ipv6/authhdr ipv6/dstopts ipv6/hopopts ipv6/flowinfo ipv6/hoplimit
 ipv6/recverr ipv6/router-alert ipv6/unicast-hops ipv6/nexthop
 ipv6/port-range ipv6/join-group ipv6/leave-group ipv6/checksum
 
 sol/socket ipproto/ip ipproto/ipv6 ipproto/icmp
 
 get-socket-option set-socket-option
 so-reuse-address? so-debug? so-error so-type
 so-accept-connections? so-keep-alive? so-dont-route? so-broadcast? so-oob-inline?
 so-send-buffer so-receive-buffer so-send-low-water so-receive-low-water
 tcp-no-delay? tcp-max-segment-size tcp-no-push? tcp-no-options? tcp-keep-alive
 ip-header-included? ip-type-of-service ip-time-to-live
 ipv6-v6-only?

 
)
;; NB importing scheme here so eval inside read-syntax works in included
;; files.  The include seems to reset the eval environment to what's imported
;; in this module by the include point.
(import scheme (only chicken include))
(include "socket.scm")

)
