;; UNIX sockets not supported because they do not exist on Windows (though we could test for this)

(use foreigners)
(use srfi-4)

(foreign-declare "
#include <sys/socket.h>
#include <netdb.h>
")

;;; constants

(define-foreign-enum-type (address-family int)
  (address-family->integer integer->address-family)
  ((af/unspec AF_UNSPEC) AF_UNSPEC)
  ((af/inet AF_INET) AF_INET)
  ((af/inet6 AF_INET6) AF_INET6)
  ((af/unix AF_LOCAL) AF_LOCAL))
(define af/inet AF_INET)
(define af/inet6 AF_INET6)
(define af/local AF_LOCAL)

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
(define-foreign-variable ni/maxhost int "NI_MAXHOST")
(define-foreign-variable ni/maxserv int "NI_MAXSERV")

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
;;      "case AF_LOCAL: C_return(((struct sockaddr_un*)sa)->sun_path);"
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
          ((= af AF_LOCAL)
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
          ((= af AF_LOCAL)
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
  (foreign-lambda int getnameinfo scheme-pointer int scheme-pointer int scheme-pointer int int))
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
(define (inet-sockaddr ip #!optional service)   ;; Not sure if optional service makes sense.
  (let ((service (and service (number->string service))))
    (and-let* ((ai (getaddrinfo/ai ip service #f #f #f AI_NUMERICHOST))  ;; + AI_NUMERICSERV
               (saddr (ai->sockaddr ai)))
      (freeaddrinfo ai)
      saddr)))

;; ADDR is either a SOCKADDR object, or an IPv4 or IPv6 string.
;; Converts returned port to numeric if possible.  Does not convert 0 to #f though.
;; Note: Should add AI_NUMERICSERV to getaddrinfo call, but it may not be portable.
;; Note: Perhaps service should be mandatory.
(define (name-information addr #!key (service #f) (flags 0))
  (cond
   ((sockaddr? addr)
    (getnameinfo addr flags))   ; service ignored
   (else
    (let ((port (if (integer? service) service (string->number service))))
      (let* ((ni (getnameinfo (inet-sockaddr addr port) flags)))
        (cond ((string->number (cdr ni))
               => (lambda (p) (cons (car ni) p)))
              (else ni)))))))

(define (getnameinfo saddr flags)
  (let* ((sa (sockaddr-blob saddr))
         (salen (sockaddr-len saddr)))
    (let ((node (make-string ni/maxhost))
          (serv (make-string ni/maxserv)))
      (let ((rc (_getnameinfo sa salen
                              node (string-length node)
                              serv (string-length serv) flags)))
        (cond ((= rc 0)
               (cons (substring node 0 (string-index node #\nul))
                     (substring serv 0 (string-index serv #\nul))))
              (else
               (error 'getnameinfo (gai_strerror rc))))))))

