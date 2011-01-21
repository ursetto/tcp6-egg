(use foreigners)
(use srfi-4)
(use hostinfo) ;; temporary -- for ip->string

(foreign-declare "
#include <sys/socket.h>
#include <netdb.h>
")

(define-foreign-enum-type (address-family int)
  (address-family->integer integer->address-family)
  ((af/unspec AF_UNSPEC) AF_UNSPEC)
  ((af/inet AF_INET) AF_INET)
  ((af/inet6 AF_INET6) AF_INET6))
(define af/inet AF_INET)
(define af/inet6 AF_INET6)

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
(define ni/numerichost NI_NUMERICHOST)

(define-foreign-record-type (sa "struct sockaddr")
  (int sa_family sa-family))

(define-record sockaddr
  family blob)

(define (sa->sockaddr sa len)
  (make-sockaddr (sa-family sa)
                 (let ((b (make-blob len)))
                   ((foreign-lambda void C_memcpy scheme-pointer c-pointer int)
                    b sa len)
                   b)))

;; (define (sa->sockaddr sa)
;;   (sa-family)
;;   )

;; ugly inet_ntop (only ugly because of the locative and MV return; getnameinfo is correct)
(define (sockaddr-address A)
  (let ((b (sockaddr-blob A)))
    (getnameinfo (make-locative b)
                 (blob-size b)
                 ni/numerichost)))

(define-record-printer (sockaddr A out)
  (fprintf out "#<sockaddr ~A>"
           (integer->address-family (sockaddr-family A))))

;; FIXME!! Port and flowinfo require network->host endian translation.
(define-foreign-record-type (sin "struct sockaddr_in")
  (int sin_family sin-family)
  (int sin_port sin-port)
  ((struct "in_addr") sin_addr sin-addr))

(define-foreign-record-type (sin6 "struct sockaddr_in6")
  (constructor: alloc-sin6)
  (destructor: free-sin6)
  ;; sin6_len is not universally provided
  (int sin6_family sin6-family)
;;(int sin6_port sin6-port)  
;;(unsigned-integer sin6_flowinfo sin6-flowinfo)
  ((struct "in6_addr") sin6_addr sin6-addr)
  (integer sin6_scope_id sin6-scope-id)
)
(define sin6-port
  (foreign-lambda* int ((sin6 s)) "C_return(ntohs(s->sin6_port));"))
(define sin6-flowinfo
  (foreign-lambda* unsigned-integer ((sin6 s)) "C_return(ntohl(s->sin6_flowinfo));"))

;; (define-foreign-record-type (in-addr "struct in_addr")
;;   (c-pointer s_addr in-addr-s))
(define-foreign-type in-addr (c-pointer (struct "in_addr")))
(define (in-addr-s a)
  ((foreign-lambda* c-pointer ((in-addr in))
     "C_return(&in->s_addr);")
   a))
(define-foreign-record-type (in6-addr "struct in6_addr")
  (c-pointer s6_addr in6-addr-s6))

(define (c-pointer->u8vector ptr len)
  (let ((bv (make-u8vector len))
        (memcpy (foreign-lambda bool "C_memcpy"
                                u8vector c-pointer integer)))  ;; scheme-pointer illegal
    (memcpy bv ptr len)
    bv))

(define (inet6-address a)
  (c-pointer->u8vector (in6-addr-s6 a) 16))
(define (inet-address a)
  (c-pointer->u8vector (in-addr-s a) 4))

;; ex. (inet6-address (sin6-addr (ai-addr (getaddrinfo "fe80::1%en0"))))
;; ex. (ip->string (inet6-address (sin6-addr (ai-addr (getaddrinfo "ipv6.3e8.org")))))
;;     path can be shortened, e.g. ((sockaddr_in6*)ai_addr)->sin6_addr.s6_addr

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
           (let ((F (addrinfo-family a)))
             (cond ((eqv? F af/inet6)
                    (ip->string (inet6-address (sin6-addr (addrinfo-address a)))))
                   ((eqv? F af/inet)
                    (ip->string (inet-address (sin-addr (addrinfo-address a)))))
                   (else '?)))
           (integer->address-family (addrinfo-family a))
           (integer->socket-type (addrinfo-socktype a))
           (integer->protocol-type (addrinfo-protocol a))
           (cond ((addrinfo-canonname a)
                  => (lambda (cn) (sprintf " canonical: ~S" cn)))
                 (else ""))
           ;; (addrinfo-flags a)          ;; flag display isn't that interesting
           ))

(define (ai->addrinfo ai)
  (make-addrinfo
   (ai-flags ai)
   (ai-family ai)
   (ai-socktype ai)
   (ai-protocol ai)
   ;; TMP Store sockaddr struct in a blob on the heap.
   (and (ai-addr ai)
        (let ((b (make-blob (ai-addrlen ai))))
          (move-memory! (ai-addr ai) b (blob-size b))
          (make-locative b)))
   (ai-canonname ai)))
(define (ai-list->addrinfo ai)        ;; note that #f -> '()
  (let loop ((ai ai)
             (L '()))
    (if ai
        (loop (ai-next ai)
              (cons (ai->addrinfo ai) L))
        (reverse L))))

#|
(define (debug-ai a)
  (and a
       (pp `((family ,(integer->address-family (ai-family a)))
             (socktype ,(integer->socket-type (ai-socktype a)))
             (protocol ,(integer->protocol-type (ai-protocol a)))
             ;;      (addrlen ,(ai-addrlen a))
             ,(let ((F (ai-family a)))
                (cond ((eqv? F af/inet6)
                       `(address ,(ip->string (inet6-address (sin6-addr (ai-addr a))))))
                      ((eqv? F af/inet)
                       `(address ,(ip->string (inet-address (sin-addr (ai-addr a))))))
                      (else `(address ?))))
             (flags ,(ai-flags a))
             ,@(let ((cn (ai-canonname a)))
                 (if cn `((canonname ,cn)) '()))))))
(define (debug-ai-list A)
  (let loop ((A A))
    (when A
      (debug-ai A)
      (loop (ai-next A)))))
|#

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
  (foreign-lambda int getnameinfo sa int scheme-pointer int scheme-pointer int int))
(define gai_strerror (foreign-lambda c-string "gai_strerror" int))

(define-foreign-variable eai/noname int "EAI_NONAME")

;; I dunno, maybe getaddrinfo should take a bare addrinfo hints struct
(define (getaddrinfo node #!key family socktype protocol flags service) ;; must call freeaddrinfo on result
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

(define (address-information node . keys)
  (let* ((ai (apply getaddrinfo node keys))
         (addrinfo (ai-list->addrinfo ai)))
    (when ai (freeaddrinfo ai)) 
    addrinfo))

(define (getnameinfo sa salen flags)
  (let ((node (make-string ni/maxhost))
        (serv (make-string ni/maxserv)))
    (let ((rc (_getnameinfo sa salen node (string-length node) serv (string-length serv) flags)))
      (cond ((= rc 0)
             (values (substring node 0 (string-index node #\nul))
                     (substring serv 0 (string-index serv #\nul))))
            (else
             (error 'getnameinfo (gai_strerror rc)))))))

#|

struct sockaddr_in6 {
 unsigned short  sin6_family;
 u_int16_t       sin6_port;
 u_int32_t       sin6_flowinfo;
 struct in6_addr sin6_addr;
 u_int32_t       sin6_scope_id;
};

struct addrinfo {
        int ai_flags;           /* input flags */
        int ai_family;          /* protocol family for socket */
        int ai_socktype;        /* socket type */
        int ai_protocol;        /* protocol for socket */
        socklen_t ai_addrlen;   /* length of socket-address */
        struct sockaddr *ai_addr; /* socket-address for socket */
        char *ai_canonname;     /* canonical name for service location */
        struct addrinfo *ai_next; /* pointer to next in list */
};


|#