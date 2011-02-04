;; tcp-connect should try all addresses.  Either catch socket-connect! exception,
;; or change socket-connect! to allow multiple addresses.  e.g. (socket-connect! so addr1 addr2)...
;; tcp-connect should be allowed to connect to only 1 address (or addrinfo list)

;; added tcp-bind-ipv6-only param; if af/inet6, will set IPV6_V6ONLY option on socket
;; tcp-listen accepts service name string
;; tcp-connect accepts service name string (may issue SRV request)

;; creating a 'socket port (slot 7) will allow posixunix to call ##sys#tcp-port->fileno,
;; which will bomb

;;;; tcp.scm - Networking stuff
;
; Copyright (c) 2008-2011, The Chicken Team
; Copyright (c) 2000-2007, Felix L. Winkelmann
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following
; conditions are met:
;
;   Redistributions of source code must retain the above copyright notice, this list of conditions and the following
;     disclaimer. 
;   Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following
;     disclaimer in the documentation and/or other materials provided with the distribution. 
;   Neither the name of the author nor the names of its contributors may be used to endorse or promote
;     products derived from this software without specific prior written permission. 
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS
; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
; AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
; OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
; POSSIBILITY OF SUCH DAMAGE.


;(declare
 
  (use extras ;; scheduler
       )
  ;; (export tcp-close tcp-listen tcp-connect tcp-connect/ai tcp-accept tcp-accept-ready? ##sys#tcp-port->fileno tcp-listener? tcp-addresses
  ;;         tcp-abandon-port tcp-listener-port tcp-listener-fileno tcp-port-numbers tcp-buffer-size tcp-listener-socket
  ;;         tcp-read-timeout tcp-write-timeout tcp-accept-timeout tcp-connect-timeout)
  (foreign-declare #<<EOF
#include <errno.h>
#ifdef _WIN32
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
# define typecorrect_getsockopt(socket, level, optname, optval, optlen)	\
    getsockopt(socket, level, optname, (char *)optval, optlen)
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
#endif

#ifndef SD_RECEIVE
# define SD_RECEIVE      0
# define SD_SEND         1
#endif

#ifdef ECOS
#include <sys/sockio.h>
#endif

#ifndef h_addr
# define h_addr  h_addr_list[ 0 ]
#endif

static char addr_buffer[ 20 ];
EOF
) 

;;(include "common-declarations.scm")

;; (register-feature! 'tcp)

(define-foreign-variable errno int "errno")
(define-foreign-variable strerror c-string "strerror(errno)")

(define-foreign-type sockaddr* (pointer "struct sockaddr"))
(define-foreign-type sockaddr_in* (pointer "struct sockaddr_in"))

(define-foreign-variable _af_inet int "AF_INET")
(define-foreign-variable _sock_stream int "SOCK_STREAM")
(define-foreign-variable _sock_dgram int "SOCK_DGRAM")
(define-foreign-variable _sockaddr_size int "sizeof(struct sockaddr)")
(define-foreign-variable _sockaddr_in_size int "sizeof(struct sockaddr_in)")

(define ##net#close (foreign-lambda int "closesocket" int))

(define ##net#getsockname 
  (foreign-lambda* c-string ((int s))
    "struct sockaddr_storage ss;"
    "char ip[NI_MAXHOST];"
    "int len = sizeof(ss);"
    "if(getsockname(s, (struct sockaddr *)&ss, (socklen_t *)&len) != 0) C_return(NULL);"
    "if(getnameinfo((struct sockaddr *)&ss, len, ip, sizeof(ip), NULL, 0, NI_NUMERICHOST)) C_return(NULL);"
    "C_return(ip);"))

(define ##net#getsockport
  (foreign-lambda* int ((int s))
    "struct sockaddr_storage ss;"
    "int len = sizeof(ss);"
    "if(getsockname(s, (struct sockaddr *)&ss, (socklen_t *)(&len)) != 0) C_return(-1);"
    "switch (((struct sockaddr*)&ss)->sa_family) {"
    "case AF_INET: C_return(ntohs(((struct sockaddr_in*)&ss)->sin_port));"
    "case AF_INET6: C_return(ntohs(((struct sockaddr_in6*)&ss)->sin6_port));"
    "default: C_return(-1); }"))

(define ##net#getpeerport
 (foreign-lambda* int ((int s))
   "struct sockaddr_storage ss;"
   "int len = sizeof(ss);"
   "if(getpeername(s, (struct sockaddr *)&ss, (socklen_t *)(&len)) != 0) C_return(-1);"
    "switch (((struct sockaddr*)&ss)->sa_family) {"
    "case AF_INET: C_return(ntohs(((struct sockaddr_in*)&ss)->sin_port));"
    "case AF_INET6: C_return(ntohs(((struct sockaddr_in6*)&ss)->sin6_port));"
    "default: C_return(-1); }"))

(define ##net#getpeername 
  (foreign-lambda* c-string ((int s))
    "struct sockaddr_storage ss;"
    "char ip[NI_MAXHOST];"
    "int len = sizeof(ss);"
    "if(getpeername(s, (struct sockaddr *)&ss, ((socklen_t *)&len)) != 0) C_return(NULL);"
    "if(getnameinfo((struct sockaddr *)&ss, len, ip, sizeof(ip), NULL, 0, NI_NUMERICHOST)) C_return(NULL);"
    "C_return(ip);") )

(define-inline (network-error where msg . args)
  (apply ##sys#signal-hook #:network-error where msg args))
(define-inline (network-error/errno where msg . args)
  (##sys#update-errno)
  (apply ##sys#signal-hook #:network-error where
         (string-append msg " - " strerrno)
         args))

;; Force tcp4 for (tcp-listen port) when v6only enabled.  This will fail
;; on an IPv6-only system.  Assume when host is unspecified, the first addrinfo
;; result on a dual-stack system is "::".  If it is "0.0.0.0", IPv6 will be disabled.
(define (bind-tcp-socket port host)
  (let* ((family (if (and (not host) (tcp-bind-ipv6-only))
		     af/inet #f))
	 (ai (address-information host service: port family: family
				  socktype: sock/stream flags: ai/passive)))
    (when (null? ai)
      (network-error 'tcp-listen "node or service lookup failed" host port))
    (let* ((ai (car ai))
	   (addr (addrinfo-address ai)))
    (let* ((so (socket (addrinfo-family ai) (addrinfo-socktype ai) 0))
	   (s (socket-fileno so)))
    ;; PLT makes this an optional arg to tcp-listen. Should we as well?
     (when (eq? -1 ((foreign-lambda* int ((int socket)) 
		      "int yes = 1; 
                      C_return(setsockopt(socket, SOL_SOCKET, SO_REUSEADDR, (const char *)&yes, sizeof(int)));") 
		    s) )
       (network-error/errno 'tcp-listen "error setting SO_REUSEADDR" so))
     (when (= (addrinfo-family ai) af/inet6)
       (when (eq? -1 ((foreign-lambda* int ((int socket) (bool flag))
			"#ifdef IPV6_V6ONLY\n"
			"C_return(setsockopt(socket, IPPROTO_IPV6, IPV6_V6ONLY, (const char *)&flag, sizeof(flag)));\n"
			"#else\n"
			"C_return(0);\n" ;; silently fail
			"#endif\n")
		      s (tcp-bind-ipv6-only)))
	 (network-error/errno 'tcp-listen "error setting IPV6_V6ONLY" so)))
     (socket-bind! so addr)
     so))))

(define-constant default-backlog 10)

(define-record-type tcp6-listener
  (make-tcp6-listener socket)
  tcp-listener?
  (socket tcp-listener-socket))

(define (tcp-listen port #!optional (w default-backlog) host)
  (let ((so (bind-tcp-socket port host)))
    (socket-listen! so w)
    (make-tcp6-listener so)))

(define (tcp-listener-fileno tcpl)
  (socket-fileno (tcp-listener-socket tcpl)))

(define (tcp-close tcpl)
  (socket-close! (tcp-listener-socket tcpl)))

(define-constant +input-buffer-size+ 1024)
(define-constant +output-chunk-size+ 8192)

(define tcp-buffer-size (make-parameter #f))
(define tcp-read-timeout (make-parameter (* 60 1000)))
(define tcp-write-timeout (make-parameter (* 60 1000)))
(define tcp-connect-timeout (make-parameter #f))
(define tcp-accept-timeout (make-parameter #f))
(define tcp-bind-ipv6-only (make-parameter #f))

(define ##net#io-ports
  (let ((tbs tcp-buffer-size))
    (lambda (so)
      (let* ((fd (socket-fileno so))
	     (buf (make-string +input-buffer-size+))
	     (data (vector fd #f #f buf 0))
	     (buflen 0)
	     (bufindex 0)
	     (iclosed #f) 
	     (oclosed #f)
	     (outbufsize (tbs))
	     (outbuf (and outbufsize (fx> outbufsize 0) ""))
	     (tmr (tcp-read-timeout))
	     (tmw (tcp-write-timeout))
	     (output-chunk-size (socket-send-size))
	     (read-input
	      (lambda ()
		(let ((n (%socket-receive! so buf 0 +input-buffer-size+ 0 tmr)))
		  (set! buflen n)
		  (##sys#setislot data 4 n)
		  (set! bufindex 0))))
	     (in
	      (make-input-port
	       (lambda ()
		 (when (fx>= bufindex buflen)
		   (read-input))
		 (if (fx>= bufindex buflen)
		     #!eof
		     (let ((c (##core#inline "C_subchar" buf bufindex)))
		       (set! bufindex (fx+ bufindex 1))
		       c) ) )
	       (lambda ()
		 (or (fx< bufindex buflen)
		     (socket-receive-ready? so)))
	       (lambda ()
		 (unless iclosed
		   (set! iclosed #t)
		   (unless (##sys#slot data 1)
		     (socket-shutdown! so shut/rd))
		   (when oclosed
		     (socket-close! so))))
	       (lambda ()
		 (when (fx>= bufindex buflen)
		   (read-input))
		 (if (fx< bufindex buflen)
		     (##core#inline "C_subchar" buf bufindex)
		     #!eof))
	       (lambda (p n dest start)	; read-string!
		 (let loop ((n n) (m 0) (start start))
		   (cond ((eq? n 0) m)
			 ((fx< bufindex buflen)
			  (let* ((rest (fx- buflen bufindex))
				 (n2 (if (fx< n rest) n rest)))
			    (##core#inline "C_substring_copy" buf dest bufindex (fx+ bufindex n2) start)
			    (set! bufindex (fx+ bufindex n2))
			    (loop (fx- n n2) (fx+ m n2) (fx+ start n2)) ) )
			 (else
			  (read-input)
			  (if (eq? buflen 0) 
			      m
			      (loop n m start) ) ) ) ) )
	       (lambda (p limit)	; read-line
		 (let loop ((str #f)
			    (limit (or limit (##sys#fudge 21))))
		   (cond ((fx< bufindex buflen)
			  (##sys#scan-buffer-line
			   buf 
			   (fxmin buflen limit)
			   bufindex
			   (lambda (pos2 next)
			     (let* ((len (fx- pos2 bufindex))
				    (dest (##sys#make-string len)))
			       (##core#inline "C_substring_copy" buf dest bufindex pos2 0)
			       (set! bufindex next)
			       (cond ((eq? pos2 limit) ; no line-terminator, hit limit
				      (if str (##sys#string-append str dest) dest))
				     ((eq? pos2 next) ; no line-terminator, hit buflen
				      (read-input)
				      (if (fx>= bufindex buflen)
					  (or str "")
					  (loop (if str (##sys#string-append str dest) dest)
						(fx- limit len)) ) )
				     (else 
				      (##sys#setislot p 4 (fx+ (##sys#slot p 4) 1))
				      (if str (##sys#string-append str dest) dest)) ) ) ) ) )
			 (else
			  (read-input)
			  (if (fx< bufindex buflen)
			      (loop str limit)
			      #!eof) ) ) ) )
	       ;; (lambda (p)		; read-buffered
	       ;;   (if (fx>= bufindex buflen)
	       ;;       ""
	       ;;       (let ((str (##sys#substring buf bufpos buflen)))
	       ;;         (set! bufpos buflen)
	       ;;         str)))
	       ) )
	     (output
	      (lambda (str)
		(%socket-send-all! so str 0 (string-length str) 0 tmw output-chunk-size)))
	     (out
	      (make-output-port
	       (if outbuf
		   (lambda (s)
		     (set! outbuf (##sys#string-append outbuf s))
		     (when (fx>= (##sys#size outbuf) outbufsize)
		       (output outbuf)
		       (set! outbuf "") ) )
		   (lambda (s) 
		     (when (fx> (##sys#size s) 0)
		       (output s)) ) )
	       (lambda ()
		 (unless oclosed
		   (set! oclosed #t)
		   (when (and outbuf (fx> (##sys#size outbuf) 0))
		     (output outbuf)
		     (set! outbuf "") )
		   (unless (##sys#slot data 2)      ;; #t if abandoned
		     (socket-shutdown! so shut/wr))
		   (when iclosed
		     (socket-close! so))))
	       (and outbuf
		    (lambda ()
		      (when (fx> (##sys#size outbuf) 0)
			(output outbuf)
			(set! outbuf "") ) ) ) ) ) )
	(##sys#setslot in 3 "(tcp)")
	(##sys#setslot out 3 "(tcp)")
	(##sys#setslot in 7 'socket)
	(##sys#setslot out 7 'socket)
	(##sys#set-port-data! in data)
	(##sys#set-port-data! out data)
	(values in out) ) ) ) )

(define (tcp-accept tcpl)
  (parameterize ((socket-accept-timeout (tcp-accept-timeout)))
    (let ((so (socket-accept (tcp-listener-socket tcpl))))
      (##net#io-ports so))))

(define (tcp-accept-ready? tcpl)
  (socket-accept-ready? (tcp-listener-socket tcpl)))

(define-inline (network-error where msg . args)
  (apply 
   ##sys#signal-hook #:network-error where msg args))

;; Sequentially connect to all addrinfo objects until one succeeds, as long
;; as the connection is retryable (e.g. refused, no route, or timeout).
;; Otherwise it will error out on non-recoverable errors.
;; Silently skips non-stream objects for user convenience.
;; Returns: I/O ports bound to the succeeding connection, or throws an error
;; corresponding to the last failed connection attempt.
(define (tcp-connect/ai ais)
  (define (%tcp-connect/ai ais)
    (let ((ais (filter (lambda (ai) (eq? (addrinfo-protocol ai) ipproto/tcp))
		       ais)))  ;; Filter first to preserve our "last exception" model.
      (when (null? ais)
	(network-error 'tcp-connect/ai "no addresses to connect to"))
      (parameterize ((socket-connect-timeout (tcp-connect-timeout)))
	(let loop ((ais ais))
	  (let* ((ai (car ais))
		 (addr (addrinfo-address ai))
		 (so (socket (addrinfo-family ai) (addrinfo-socktype ai) 0))
		 (s (socket-fileno so)))
	    (if (null? (cdr ais))
		(begin (socket-connect! so addr) so)
		(condition-case
		 (begin (socket-connect! so addr) so)
		 (e (exn i/o net timeout)
		    (loop (cdr ais)))
		 (e (exn i/o net)
		    (if (nonfatal-connect-exception? e)
			(loop (cdr ais))
			(signal e))))))))))
  (##net#io-ports (%tcp-connect/ai ais)))

(define (tcp-connect host . more)
  (let ((port (optional more #f)))
    (##sys#check-string host)
    (unless port
      (set!-values (host port) (parse-inet-address host))
      (unless port (network-error 'tcp-connect "no port specified" host)))
    (let ((ais (address-information host service: port protocol: ipproto/tcp)))  ;; or sock/stream?
      (when (null? ais)
	(network-error 'tcp-connect "node and/or service lookup failed" host port))
      (tcp-connect/ai ais))))

(define (##sys#tcp-port->fileno p)
  (let ((data (##sys#port-data p)))
    (if (vector? data)			; a meagre test, but better than nothing
	(##sys#slot data 0)
	(error '##sys#tcp-port->fileno "argument does not appear to be a TCP port" p))))

(define (tcp-addresses p)
  (##sys#check-port p 'tcp-addresses)
  (let ((fd (##sys#tcp-port->fileno p)))
    (values 
     (or (##net#getsockname fd)
	 (##sys#signal-hook 
	  #:network-error 'tcp-addresses
	  (##sys#string-append "cannot compute local address - " strerror) p) )
     (or (##net#getpeername fd)
	 (##sys#signal-hook
	  #:network-error 'tcp-addresses
	  (##sys#string-append "cannot compute remote address - " strerror) p) ) ) ) )

(define (tcp-port-numbers p)
  (##sys#check-port p 'tcp-port-numbers)
  (let ((fd (##sys#tcp-port->fileno p)))
    (values
     (or (##net#getsockport fd)
	 (##sys#signal-hook 
	  #:network-error 'tcp-port-numbers
	  (##sys#string-append "cannot compute local port - " strerror) p) )
     (or (##net#getpeerport fd)
	 (##sys#signal-hook
	  #:network-error 'tcp-port-numbers
	  (##sys#string-append "cannot compute remote port - " strerror) p) ) ) ) )

(define (tcp-listener-port tcpl)
  (let* ((fd (tcp-listener-fileno tcpl))
	 (port (##net#getsockport fd)) )
    (when (eq? -1 port)
      (##sys#signal-hook
       #:network-error 'tcp-listener-port (##sys#string-append "cannot obtain listener port - " strerror) 
       tcpl fd) )
    port) )

(define (tcp-abandon-port p)
  (##sys#check-port p 'tcp-abandon-port)
  (##sys#setislot
   (##sys#port-data p)
   (if (##sys#slot p 1) 2 1)
   #t) )
