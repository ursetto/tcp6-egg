;; UNIX sockets not supported because they do not exist on Windows (though we could test for this)

;; socket-accept should perhaps return connected peer address
;; not all errors close the socket (probably should)

(use foreigners)
(use srfi-4)

(foreign-declare "
#include <errno.h>
#ifdef _WIN32
/* Unconditionally require Windows XP (0x501) for getaddrinfo.  It is not known
   how to autodetect missing getaddrinfo support.  These funcs are supported
   on W2k and even earlier with native SDK (Wspiapi.h), but MinGW does not support them. */
#define _WIN32_WINNT 0x501

# if (defined(HAVE_WINSOCK2_H) && defined(HAVE_WS2TCPIP_H))
#  include <winsock2.h>
#  include <ws2tcpip.h>
# else
#  include <winsock.h>
# endif
/* Beware: winsock2.h must come BEFORE windows.h */
# define socklen_t       int
static WSADATA wsa;
# define fcntl(a, b, c)  0
# define EWOULDBLOCK     0
# define EINPROGRESS     0

#ifndef SHUT_RD
# define SHUT_RD SD_RECEIVE
#endif
#ifndef SHUT_WR
# define SHUT_WR SD_SEND
#endif
#ifndef SHUT_RDWR
# define SHUT_RDWR SD_BOTH
#endif

# define typecorrect_getsockopt(socket, level, optname, optval, optlen)	\\
    getsockopt(socket, level, optname, (char *)optval, optlen)

/* On Windows < Vista, getnameinfo is broken, erroring out if it cannot resolve the
   service to a name.  Try to detect this case and rerun the call with NUMERICSERV set
   to avoid the error.  Note: we should check Windows version but do not. */
int WSAAPI skt_getnameinfo(const struct sockaddr *sa, socklen_t salen, char *node,
  DWORD nodelen, char *service, DWORD servicelen, int flags) {
    int rc = getnameinfo(sa,salen,node,nodelen,service,servicelen,flags);
    int err = WSAGetLastError();      /* rc *might* not be gai error, though it should be */
    if (rc != 0 && !(flags & NI_NUMERICSERV) && err == WSANO_DATA) {
        rc = getnameinfo(sa,salen,node,nodelen,service,servicelen,flags | NI_NUMERICSERV);
        err = WSAGetLastError();
    }
    return rc ? err : 0;
}

#else
# include <fcntl.h>
# include <sys/types.h>
# include <sys/socket.h>
# include <sys/time.h>
# include <netinet/in.h>
# include <unistd.h>
# include <netdb.h>
# include <signal.h>
# define closesocket     close
# define INVALID_SOCKET  -1
# define typecorrect_getsockopt getsockopt
# define skt_getnameinfo getnameinfo
#endif

#define ECONNREFUSED WSAECONNREFUSED
#define ETIMEDOUT WSAETIMEDOUT
/* May need to test WSAEHOSTDOWN as well */
#define ENETUNREACH WSAENETUNREACH
#define EHOSTUNREACH WSAEHOSTUNREACH

#ifdef ECOS
#include <sys/sockio.h>
#endif

")

;;; constants

(define-foreign-enum-type (address-family int)
  (address-family->integer integer->address-family)
  ((af/unspec AF_UNSPEC) AF_UNSPEC)
  ((af/inet AF_INET) AF_INET)
  ((af/inet6 AF_INET6) AF_INET6)
  ((af/unix AF_UNIX) AF_UNIX))
(define af/inet AF_INET)
(define af/inet6 AF_INET6)
(define af/unix AF_UNIX)

(define-foreign-enum-type (socket-type int)
  (socket-type->integer integer->socket-type)
  ((sock/stream SOCK_STREAM) SOCK_STREAM)
  ((sock/dgram  SOCK_DGRAM)  SOCK_DGRAM)
  ((sock/raw    SOCK_RAW)    SOCK_RAW))
(define sock/stream SOCK_STREAM)
(define sock/dgram  SOCK_DGRAM)
(define sock/raw    SOCK_RAW)

(define-foreign-enum-type (protocol-type int)
  (protocol-type->integer integer->protocol-type)
  ((ipproto/tcp IPPROTO_TCP)  IPPROTO_TCP)
  ((ipproto/udp IPPROTO_UDP)  IPPROTO_UDP))
(define ipproto/tcp IPPROTO_TCP)
(define ipproto/udp IPPROTO_UDP)

(define-foreign-variable AI_CANONNAME int "AI_CANONNAME")
(define-foreign-variable AI_NUMERICHOST int "AI_NUMERICHOST")
(define-foreign-variable AI_PASSIVE int "AI_PASSIVE")
(define ai/canonname AI_CANONNAME)
(define ai/numerichost AI_NUMERICHOST)
(define ai/passive AI_PASSIVE)
(define-foreign-variable NI_MAXHOST int "NI_MAXHOST")
(define-foreign-variable NI_MAXSERV int "NI_MAXSERV")

(define-foreign-variable NI_NUMERICHOST int "NI_NUMERICHOST")
(define-foreign-variable NI_NUMERICSERV int "NI_NUMERICSERV")
(define-foreign-variable NI_DGRAM int "NI_DGRAM")
(define-foreign-variable NI_NAMEREQD int "NI_NAMEREQD")
(define-foreign-variable NI_NOFQDN int "NI_NOFQDN")
(define ni/numerichost NI_NUMERICHOST)
(define ni/numericserv NI_NUMERICSERV)
(define ni/dgram NI_DGRAM)
(define ni/namereqd NI_NAMEREQD)
(define ni/nofqdn NI_NOFQDN)

;;;

(define-foreign-record-type (sa "struct sockaddr")
  (int sa_family sa-family))

(define-record sockaddr family blob)

(define (sa->sockaddr sa len)    ;; sa -- c-pointer; len -- length of sockaddr struct
  (make-sockaddr (sa-family sa)
                 (let ((b (make-blob len)))
                   ((foreign-lambda void C_memcpy scheme-pointer c-pointer int)
                    b sa len)
                   b)))

(define (sockaddr-len A)
  (blob-size (sockaddr-blob A)))
;; (define (sockaddr-path A)          ;; not supported on Windows
;;   ((foreign-lambda* c-string ((scheme-pointer sa))
;;      "switch (((struct sockaddr*)sa)->sa_family) {"
;;      "case AF_UNIX: C_return(((struct sockaddr_un*)sa)->sun_path);"
;;      "default: C_return(NULL); }"
;;      )
;;    (sockaddr-blob A)))
(define (sockaddr-path A)
  (error 'sockaddr-path "UNIX sockets are not supported"))

(define (sockaddr-address A)
  (let ((af (sockaddr-family A)))
    (cond ((or (= af AF_INET)
               (= af AF_INET6))
           (car (getnameinfo A (+ NI_NUMERICHOST NI_NUMERICSERV))))
          ((= af AF_UNIX)
           (sockaddr-path A))
          (else #f))))
(define (sockaddr-port A)
  ((foreign-lambda* scheme-object ((scheme-pointer sa))
     "switch (((struct sockaddr*)sa)->sa_family) {"
     "case AF_INET: C_return(C_fix(ntohs(((struct sockaddr_in*)sa)->sin_port)));"
     "case AF_INET6: C_return(C_fix(ntohs(((struct sockaddr_in6*)sa)->sin6_port)));"
     "default: C_return(C_SCHEME_FALSE); }")
   (sockaddr-blob A)))

(define-record-printer (sockaddr A out)
  (fprintf out "#<sockaddr ~S>"
           (sockaddr->string A)
           ;; (integer->address-family (sockaddr-family A))
           ))

;; Convert socket address/path to a compact string, mainly for display purposes.
(define (sockaddr->string A)
  (let ((af (sockaddr-family A)))
    (cond ((or (= af AF_INET)
               (= af AF_INET6))
           (let* ((ni (getnameinfo A (+ NI_NUMERICHOST NI_NUMERICSERV)))
                  (h (car ni))
                  (p (cdr ni)))
             (if (string=? p "0")
                 h
                 (if (= af AF_INET6)
                     (string-append "[" h "]" ":" p)
                     (string-append h ":" p)))))
          ((= af AF_UNIX)
           (sockaddr-path A))  ;; or reach directly into blob here
          (else
           #f))))

;; Intent of this is a direct call to getnameinfo ala inet_ntop, returning
;; a plain string; however, error handling is hard.
;; (define (sockaddr->ip-string A)
;;   (foreign-lambda* c-string ((scheme-pointer sa))
;;     ""
;;     ))

(define-foreign-record-type (ai "struct addrinfo")
  (constructor: alloc-ai)
  (destructor: free-ai)   ; similar name!
  (int ai_flags ai-flags set-ai-flags!)
  (int ai_family ai-family set-ai-family!)
  (int ai_socktype ai-socktype set-ai-socktype!)
  (int ai_protocol ai-protocol set-ai-protocol!)  
  (int ai_addrlen ai-addrlen)
  ((c-pointer sa) ai_addr ai-addr)  ;; non-null?
  (c-string ai_canonname ai-canonname)
  ((c-pointer ai) ai_next ai-next))

(define-record addrinfo
  flags family socktype protocol address canonname)
(define-record-printer (addrinfo a out)
  (fprintf out "#<addrinfo ~S ~S ~S ~S~A>"
           (sockaddr->string (addrinfo-address a))
           (integer->address-family (addrinfo-family a))
           (integer->socket-type (addrinfo-socktype a))
           (integer->protocol-type (addrinfo-protocol a))
           (cond ((addrinfo-canonname a)
                  => (lambda (cn) (sprintf " canonical: ~S" cn)))
                 (else ""))
           ;; (addrinfo-flags a)          ;; flag display isn't that interesting
           ))

(define (ai->addrinfo ai)           ;; construct addrinfo obj from ai ptr, with embedded sockaddr obj
  (make-addrinfo
   (ai-flags ai)
   (ai-family ai)
   (ai-socktype ai)
   (ai-protocol ai)
   (ai->sockaddr ai)
   (ai-canonname ai)))
(define (ai->sockaddr ai)           ;; direct construction of sockaddr object from ai pointer
  (and-let* ((addr (ai-addr ai)))
    (sa->sockaddr addr (ai-addrlen ai))))

(define (ai-list->addrinfo ai)      ;; construct addrinfo object list from ai linked list
  (let loop ((ai ai)
             (L '()))
    (if ai
        (loop (ai-next ai)
              (cons (ai->addrinfo ai) L))
        (reverse L))))

(define (alloc-null-ai)
  (let ((null! (foreign-lambda* void ((ai ai))
                 "memset(ai,0,sizeof(*ai));"
                 ))
        (ai (alloc-ai)))
    (null! ai)
    ai))
(define _getaddrinfo
    (foreign-lambda int getaddrinfo c-string c-string ai (c-pointer ai)))
(define freeaddrinfo
  (foreign-lambda void freeaddrinfo ai))
(define _getnameinfo
  (foreign-lambda int skt_getnameinfo scheme-pointer int scheme-pointer
                  int scheme-pointer int int))

(define gai_strerror (foreign-lambda c-string "gai_strerror" int))

(define-foreign-variable eai/noname int "EAI_NONAME")

;; FIXME: hints constructor is craaaap
;; Returns a c-pointer; must call freeaddrinfo on result once used.
(define (getaddrinfo/ai node service family socktype protocol flags)
  (let-location ((res c-pointer))
    (let ((hints #f))
      (define hints (alloc-null-ai))
      (when family (set-ai-family! hints family))
      (when socktype (set-ai-socktype! hints socktype))
      (when flags (set-ai-flags! hints flags))
      (when protocol (set-ai-protocol! hints protocol))
      (let ((rc (_getaddrinfo node service hints #$res)))
        (when hints (free-ai hints))
        (cond ((= 0 rc)
               res)
              ((= eai/noname rc)  ;; save exceptions for real errors
               #f)
              (else
               (when res (freeaddrinfo res))   ;; correct??
               (error 'getaddrinfo (gai_strerror rc) node)))))))
(define (getaddrinfo node service family socktype protocol flags)
  (let* ((ai (getaddrinfo/ai node service family socktype protocol flags))
         (addrinfo (ai-list->addrinfo ai)))
    (when ai (freeaddrinfo ai))
    addrinfo))

(define (address-information node #!key service family socktype protocol flags)
  (let ((service (if (integer? service) (number->string service) service)))
    (getaddrinfo node service family socktype protocol flags)))

;; Constructor for socket address object from IP address string & SERVICE number.
;; The usual way to create such an address is via address-information; this is
;; a more efficient shortcut.
;; FIXME: The name is suspect.
(define (inet-address ip #!optional service)   ;; Not sure if optional service makes sense.
  (let ((service (and service (number->string service))))
    (and-let* ((ai (getaddrinfo/ai ip service #f #f #f AI_NUMERICHOST))  ;; + AI_NUMERICSERV
               (saddr (ai->sockaddr ai)))
      (freeaddrinfo ai)
      saddr)))

;; ADDR is either a SOCKADDR object, or an IPv4 or IPv6 string.
;; Converts returned port to numeric if possible.  Does not convert 0 to #f though.
;; Note: Should add AI_NUMERICSERV to getaddrinfo call, but it may not be portable.
;; Note: Perhaps service should be mandatory.
;; Note: (car (name-information addr flags: ni/numerichost)) ==
;;         (sockaddr-address (inet-sockaddr addr)), so there is some redundancy.
(define (name-information addr #!key (service #f) (flags 0))
  (define (massage ni)
    (cond ((string->number (cdr ni))
           => (lambda (p) (cons (car ni) p)))
          (else ni)))
  (cond
   ((sockaddr? addr)
    (massage (getnameinfo addr flags)))         ; service ignored
   (else
    (let ((port (cond ((not service) #f)
                      ((integer? service) service)
                      ((string->number service))
                      (else (error 'name-information "service must be a numeric value or #f"
                                   service)))))
      (let ((saddr (inet-address addr port)))
        (unless saddr
          (error 'name-information "invalid internet address" addr port))
        (massage (getnameinfo saddr flags)))))))

(define (getnameinfo saddr flags)
  (let* ((sa (sockaddr-blob saddr))
         (salen (sockaddr-len saddr)))
    (let ((node (make-string NI_MAXHOST))
          (serv (make-string NI_MAXSERV)))
      (let ((rc (_getnameinfo sa salen
                              node NI_MAXHOST
                              serv NI_MAXSERV flags)))
        (cond ((= rc 0)
               (cons (substring node 0 (string-index node #\nul))
                     (substring serv 0 (string-index serv #\nul))))
              (else
               (error 'getnameinfo (gai_strerror rc))))))))



;;; socket operations

(define socket-startup
  (foreign-lambda* bool () "
#ifdef _WIN32
     C_return(WSAStartup(MAKEWORD(1, 1), &wsa) == 0);
#else
     signal(SIGPIPE, SIG_IGN);
     C_return(1);
#endif
"))

(unless (socket-startup)   ;; hopefully, this is safe to run multiple times
  (network-error 'socket-startup "cannot initialize socket code"))

(define socket-connect-timeout)
(define socket-read-timeout)
(define socket-write-timeout)
(define socket-accept-timeout)

(let ()
  (define ((check loc) x)
    (when x (##sys#check-exact x loc))
    x)
  (define minute (fx* 60 1000))
  (set! socket-read-timeout (make-parameter minute (check 'socket-read-timeout)))
  (set! socket-write-timeout (make-parameter minute (check 'socket-write-timeout))) 
  (set! socket-connect-timeout (make-parameter #f (check 'socket-connect-timeout))) 
  (set! socket-accept-timeout (make-parameter #f (check 'socket-accept-timeout))) )

;; Socket errors under windows probably require the use of WSAGetLastError &
;; WSA* error codes.  Note that non-blocking reads are disabled on Windows;
;; perhaps this could be fixed.
(define-foreign-variable errno int "errno")
(define-foreign-variable strerrno c-string "strerror(errno)")
(define-foreign-variable _invalid_socket int "INVALID_SOCKET")
(define-foreign-variable _ewouldblock int "EWOULDBLOCK")
(define-foreign-variable _einprogress int "EINPROGRESS")
(define-foreign-variable _econnrefused int "ECONNREFUSED")
(define-foreign-variable _etimedout int "ETIMEDOUT")
(define-foreign-variable _enetunreach int "ENETUNREACH")
(define-foreign-variable _ehostunreach int "EHOSTUNREACH")

(define-foreign-variable SHUT_RD int "SHUT_RD")
(define-foreign-variable SHUT_WR int "SHUT_WR")
(define-foreign-variable SHUT_RDWR int "SHUT_RDWR")
(define shut/rd SHUT_RD)
(define shut/wr SHUT_WR)
(define shut/rdwr SHUT_RDWR)

(define _close_socket (foreign-lambda int "closesocket" int))
(define strerror (foreign-lambda c-string "strerror" int))

(define _make_socket_nonblocking
  (foreign-lambda* bool ((int fd))
    "int val = fcntl(fd, F_GETFL, 0);"
    "if(val == -1) C_return(0);"
    "C_return(fcntl(fd, F_SETFL, val | O_NONBLOCK) != -1);"))

(define select-for-read
  (foreign-lambda* int ((int fd))
    "fd_set in;
     struct timeval tm;
     int rv;
     FD_ZERO(&in);
     FD_SET(fd, &in);
     tm.tv_sec = tm.tv_usec = 0;
     rv = select(fd + 1, &in, NULL, NULL, &tm);
     if(rv > 0) { rv = FD_ISSET(fd, &in) ? 1 : 0; }
     C_return(rv);") )

(define select-for-write
  (foreign-lambda* int ((int fd))
    "fd_set out;
     struct timeval tm;
     int rv;
     FD_ZERO(&out);
     FD_SET(fd, &out);
     tm.tv_sec = tm.tv_usec = 0;
     rv = select(fd + 1, NULL, &out, NULL, &tm);
     if(rv > 0) { rv = FD_ISSET(fd, &out) ? 1 : 0; }
     C_return(rv);") )

(define-inline (network-error where msg . args)
  (apply ##sys#signal-hook #:network-error where msg args))
(define-inline (network-error/errno where msg . args)
  (##sys#update-errno)
  (apply ##sys#signal-hook #:network-error where
         (string-append msg " - " strerrno)
         args))

(define (block-for-timeout! where timeout fd type #!optional cleanup)  ;; #f permitted for WHERE
  (when timeout
    (##sys#thread-block-for-timeout!
     ##sys#current-thread
     (+ (current-milliseconds) timeout)))
  (##sys#thread-block-for-i/o! ##sys#current-thread fd type)
  (##sys#thread-yield!)
  (when (##sys#slot ##sys#current-thread 13)
    (if cleanup (cleanup))
    (##sys#signal-hook
     #:network-timeout-error
     where "operation timed out" timeout fd)))

(define (get-socket-error s)
  ;; http://cr.yp.to/docs/connect.html describes alternative ways to retrieve
  ;; non-blocking socket errors.
  (define _getsockerr
    (foreign-lambda* int ((int socket))
      "int err;"
      "int optlen = sizeof(err);"
      "if (typecorrect_getsockopt(socket, SOL_SOCKET, SO_ERROR, &err, (socklen_t *)&optlen) == -1)"
      "C_return(-1);"
      "C_return(err);"))
  (let ((err (_getsockerr s)))
    (cond ((fx= err 0) #f)
          ((fx> err 0) err)
          (else
           (_close_socket s)
           (network-error/errno 'get-socket-error "unable to obtain socket error code")))))

;; Silly parsing of inet address string into host and port.
;; Note: if unparsable into host/port, return full string as hostname, and let caller deal with it.
;; If host or port is empty, returns #f for that field.
(define (parse-inet-address str)
  (let ((len (string-length str)))
    (if (= len 0)
	(values "" #f)
	(if (char=? (string-ref str 0) #\[)
	    (let ((j (string-index str #\] 1)))
	      (if j
		  (let* ((host (substring str 1 j))
			 (host (if (string=? host "") #f host)))
		    (if (= (fx+ j 1) len)
			(values host #f)      ;; bracketed address w/o port
			(if (char=? (string-ref str (fx+ j 1)) #\:)
			    (let* ((port (substring str (fx+ j 2)))
				   (port (if (string=? port "") #f port)))
			      (values host port)) ;; bracketed address w/ port
			    (values str #f))))
		  (values str #f)))
	    (let ((j (string-index str #\:)))
	      (if j
		  (let ((k (string-index str #\: (fx+ j 1))))
		    (if k
			(values str #f)   ;; a bare IPv6 address
			(let* ((host (substring str 0 j))
			       (host (if (string=? host "") #f host))
			       (port (substring str (fx+ j 1)))
			       (port (if (string=? port "") #f port)))
			  (values host port)))) ;; IPv4 address w/port
		  (values str #f)) ;; an IPv4 address sans port
	      )))))

(define-syntax non-nil
  (syntax-rules ()
    ((_ a)
     (let ((x a))
       (if (or (not x) (null? x)) #f x)))
    ((_ a . rest)
     (let ((x a))
       (if (or (not x) (null? x))
           (non-nil . rest)
           x)))))

(define-record socket fileno family type protocol)  ;; NB socket? conflicts with Unit posix
(define-inline (%socket-fileno so)
  (##sys#slot so 1))

(define-record-printer (socket s out)
  (fprintf out "#<socket fd:~S ~S ~S>"
           (socket-fileno s)
           (non-nil (integer->address-family (socket-family s)) (socket-family s))
           (non-nil (integer->socket-type (socket-type s)) (socket-type s))
           #;(non-nil (integer->protocol-type (socket-protocol s)) (socket-protocol s))
           ))

(define (socket family socktype protocol)
  (define _socket (foreign-lambda int "socket" int int int))
  (let ((s (_socket family socktype protocol)))
    (when (eq? _invalid_socket s)
      (network-error/errno 'socket "cannot create socket"
                           (non-nil (integer->address-family family) family)
                           (non-nil (integer->socket-type socktype) socktype)
                           (non-nil (integer->protocol-type protocol) protocol)))
    (make-socket s family socktype protocol)))


;; FIXME Terrible name and implementation.  Maybe this should be called a "transient" connect error
;; -- i.e. one that could be retried
(define (nonfatal-connect-error where msg . args)
  (abort
   (make-composite-condition
    (make-property-condition 'exn 'location where 'message msg 'arguments args)
    (make-property-condition 'i/o)
    (make-property-condition 'net 'nonfatal #t))))

;; note: consider including errno
(define-inline (nonfatal-connect-error/errno where msg . args)
  (##sys#update-errno)
  (apply nonfatal-connect-error where (string-append msg " - " strerrno)
         args))

(define nonfatal-connect-exception?
  (condition-property-accessor 'net 'nonfatal #f))

;; Returns a special "nonfatal" error if connection failure was due to refusal,
;; network down, etc.; in which case, another address could be tried.  (The socket
;; is still closed, though.)
(define (socket-connect! so saddr)
  (define _connect (foreign-lambda int "connect" int scheme-pointer int))
  (define (refused? err)
    (or (eq? err _econnrefused) (eq? err _etimedout)
        (eq? err _enetunreach) (eq? err _ehostunreach)))
  (let ((s (socket-fileno so))
        (timeout (socket-connect-timeout)))
    (unless (_make_socket_nonblocking s)
      (network-error/errno 'socket-connect! "unable to set socket to non-blocking" so))
    (when (eq? -1 (_connect s (sockaddr-blob saddr) (sockaddr-len saddr)))
      (if (eq? errno _einprogress)
          (let loop ()
            (let ((f (select-for-write s)))
              (when (eq? f -1)
                (network-error/errno 'socket-connect! "select failed" so))
              (unless (eq? f 1)
                (block-for-timeout! 'socket-connect! timeout s #:all
                                    (lambda () (_close_socket s))) ;; close socket (abort) on timeout
                (loop))
              (cond ((get-socket-error s)
                     => (lambda (err)
                          (_close_socket s)
                          ((if (refused? err)
                               nonfatal-connect-error
                               network-error)
                           'socket-connect! (string-append "cannot initiate connection - "
                                                           (strerror err))
                           so saddr))))))
          (begin
            (_close_socket s) ;; Note: may update errno.
            ((if (refused? errno)
                nonfatal-connect-error/errno
                network-error/errno)
              'socket-connect! "cannot initiate connection" so saddr))))
    ;; perhaps socket address should be stored in socket object
    (void)))


;; (socket-bind! s (addrinfo-address (car (address-information "127.0.0.1" service: 9112 socktype: sock/stream flags: ai/passive))))
;; ... is verbose; perhaps could be streamlined.

(define (socket-bind! so saddr)
  (define _bind (foreign-lambda int "bind" int scheme-pointer int))
  (let ((b (_bind (socket-fileno so) (sockaddr-blob saddr) (sockaddr-len saddr))))
    (if (eq? -1 b)
        (network-error/errno 'socket-bind! "cannot bind to socket" so saddr)
        (void))))

;; Listening on datagram socket throws an OS error.
(define (socket-listen! so backlog)
  (define _listen (foreign-lambda int "listen" int int))
  (let ((l (_listen (socket-fileno so) backlog)))
    (when (eq? -1 l)
      (network-error/errno 'socket-listen! "cannot listen on socket" so))))

(define (socket-close! so)
  (let ((s (socket-fileno so)))
    (when (fx= -1 (_close_socket s))
      (network-error/errno 'socket-close! "could not close socket" so))))

;; Returns a socket object representing the accepted connection.
;; Does not currently return the socket address of the remote, although it could;
;; alternatively you can get it from getpeername.
(define (socket-accept so)
  (define _accept (foreign-lambda int "accept" int c-pointer c-pointer))
  (let ((s (socket-fileno so))
        (to (socket-accept-timeout)))
    (let restart ()
      (if (eq? 1 (select-for-read s))
          (let ((s (_accept s #f #f)))
            (when (eq? -1 s)
              (network-error/errno 'socket-accept "could not accept from listener" so))
            (unless (_make_socket_nonblocking s)
              (network-error/errno 'socket-accept "unable to set socket to non-blocking" s))
            ;; iffy
            (make-socket s (socket-family so) (socket-type so) (socket-protocol so)))
          (begin
            (block-for-timeout! 'socket-accept to s #:input)
            (restart))))))

;; Returns number of bytes received.  If 0, and socket is sock/stream, peer has shut down his side.
(define (socket-receive! so buf #!optional (start 0) (end #f) (flags 0))
  (let* ((buflen (cond ((string? buf) (string-length buf))
                       ((blob? buf) (blob-size buf))
                       (else
                        (network-error 'socket-receive!
                                       "receive buffer must be a blob or a string" so))))
         (end (or end buflen)))
    (##sys#check-exact start)    
    (##sys#check-exact end)
    (##sys#check-exact flags)
    (when (or (fx< start 0)
              (fx> end buflen)
              (fx< end start))
      (network-error 'socket-receive! "receive buffer offsets out of range" start end))
    (%socket-receive! so buf start (fx- end start) flags (socket-read-timeout))))

;; Variant of socket-receive! which does not check so, buf, start, or len and which takes
;; read timeout as parameter.  Basically for use in socket ports.
(define (%socket-receive! so buf start len flags timeout)
  (define _recv_offset (foreign-lambda* int ((int s) (scheme-pointer buf) (int start)
                                             (int len) (int flags))
                         "C_return(recv(s,((char*)buf)+start,len,flags));"))
  (let ((s (%socket-fileno so)))
    (let restart ()
      (let ((n (_recv_offset s buf start len flags)))
        (cond ((eq? -1 n)
               (cond ((eq? errno _ewouldblock)
                      (block-for-timeout! 'socket-receive! timeout s #:input)
                      (restart))
                     (else
                      (network-error/errno 'socket-receive! "cannot read from socket" so))))
              (else n))))))

(define (socket-receive-ready? so)
  (let ((f (select-for-read (socket-fileno so))))
    (when (eq? -1 f)
      (network-error/errno 'socket-receive-ready? "unable to check socket for input" so))
    (eq? 1 f)))
(define socket-accept-ready? socket-receive-ready?)

(define (socket-send! so buf #!optional (start 0) (end #f) (flags 0))
  (let* ((buflen (cond ((string? buf) (string-length buf))
                       ((blob? buf) (blob-size buf))
                       (else
                        (network-error 'socket-send!
                                       "send buffer must be a blob or a string" so))))
         (end (or end buflen)))
    (##sys#check-exact start)
    (##sys#check-exact end)
    (##sys#check-exact flags)
    (when (or (fx< start 0)
              (fx> end buflen)
              (fx< end start))
      (network-error 'socket-send! "send buffer offsets out of range" start end))
    (%socket-send! so buf start (fx- end start) flags (socket-write-timeout))))
(define (%socket-send! so buf start len flags timeout)
  (define _send_offset (foreign-lambda* int ((int s) (scheme-pointer buf) (int start)
                                             (int len) (int flags))
                         "C_return(send(s,((char*)buf)+start,len,flags));"))
  (let ((s (%socket-fileno so)))
    (let retry ((len len) (start start))
      (let ((n (_send_offset s buf start len flags)))
        (cond ((eq? -1 n)
               (cond ((eq? errno _ewouldblock)
                      (block-for-timeout! 'socket-send! timeout s #:output)
                      (retry len start))
                     (else
                      (network-error/errno 'socket-send! "cannot send to socket" so))))
              (else n))))))

(define-foreign-variable +maximum-string-length+ int "C_HEADER_SIZE_MASK")  ;; horrible
;; NB.  Possible the chunking functionality belongs in %socket-send.  We *could* limit
;; the packet size there.
(define (%socket-send-all! so buf start slen flags timeout chunksz)
  (let ((chunksz (or chunksz +maximum-string-length+)))
    (let loop ((len slen) (start start))
      (let* ((count (fxmin chunksz len))
             (n (%socket-send! so buf start count flags timeout)))
        (if (fx< n len)
            (loop (fx- len n) (fx+ start n))
            slen))))) ;; void?

;; Socket output chunk size for send-all.  For compatibility with Unit TCP; maybe not necessary.
;; If #f, attempt to send as much as possible.  Only question is whether it is safe to exceed
;; the socket send buffer size, which may (according to Microsoft pages) cause stalling until
;; delayed ACKs come back.
(define socket-send-size (make-parameter 16384)) 

(define (socket-send-all! so buf #!optional (start 0) (end #f) (flags 0))
  (let* ((buflen (cond ((string? buf) (string-length buf))
                       ((blob? buf) (blob-size buf))
                       (else
                        (network-error 'socket-send-all!
                                       "send buffer must be a blob or a string" so))))
         (end (or end buflen)))
    (##sys#check-exact start)
    (##sys#check-exact end)
    (##sys#check-exact flags)
    (when (or (fx< start 0)
              (fx> end buflen)
              (fx< end start))
      (network-error 'socket-send-all! "send buffer offsets out of range" start end))
    (%socket-send-all! so buf start (fx- end start) flags
                       (socket-write-timeout)
                       (socket-send-size))))

(define (socket-shutdown! so how)  ;; how: shut/rd, shut/wr, shut/rdwr
  (define _shutdown (foreign-lambda int "shutdown" int int))
  (if (eq? -1 (_shutdown (socket-fileno so) how))
      (network-error/errno 'socket-shutdown! "unable to shutdown socket" so how)
      (void)))

(define (socket-name so)
  (define _free (foreign-lambda void "C_free" c-pointer))
  (let-location ((len int))
    (let ((sa (_getsockname (socket-fileno so) (location len))))
      (if (fx= sa -1)
          (network-error/errno 'socket-name "unable to get socket name" so)
          (let ((addr (sa->sockaddr sa len)))
            (_free sa)
            addr)))))

(define (socket-peer-name so)
  (define _free (foreign-lambda void "C_free" c-pointer))
  (let-location ((len int))
    (let ((sa (_getpeername (socket-fileno so) (location len))))
      (if (fx= sa -1)
          (network-error/errno 'socket-peer-name "unable to get socket peer name" so)
          (let ((addr (sa->sockaddr sa len)))
            (_free sa)
            addr)))))

(define _getsockname
  (foreign-lambda* c-pointer ((int s) ((c-pointer int) len))
    "struct sockaddr_storage *ss;"
    "ss = (struct sockaddr_storage *)C_malloc(sizeof(*ss));"
    "*len = sizeof(*ss);"
    "if (getsockname(s, (struct sockaddr *)ss, (socklen_t *)len) != 0) C_return(NULL);"
    "C_return(ss);"))
(define _getpeername
  (foreign-lambda* c-pointer ((int s) ((c-pointer int) len))
    "struct sockaddr_storage *ss;"
    "ss = (struct sockaddr_storage *)C_malloc(sizeof(*ss));"
    "*len = sizeof(*ss);"
    "if (getpeername(s, (struct sockaddr *)ss, (socklen_t *)len) != 0) C_return(NULL);"
    "C_return(ss);"))
