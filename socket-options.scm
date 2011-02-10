;; TODO: Maybe so-reuse-address -> so-reuse-address?.  But setter so-reuse-address?-set! is weird.
;; Setter set-so-reuse-address?! is better, maybe.

;; (declare
;;  (hide get-boolean-option set-boolean-option
;;        get-integer-option set-integer-option
;;        set-readonly-option
;;        ##sys#check-boolean check-error
;;        setsockopt getsockopt setsockopt/int getsockopt/int
;;        ))

;; (use lolevel)

;; ;;; C header glue

;; #>
;; #include <errno.h>
;; #ifdef _WIN32
;; # if _MSC_VER > 1300
;; # include <winsock2.h>
;; # include <ws2tcpip.h>
;; # else
;; # include <winsock.h>
;; # endif
;; /* Beware: winsock2.h must come BEFORE windows.h */
;; # define socklen_t       int
;; # define typecorrect_getsockopt(socket, level, optname, optval, optlen)	\
;;     getsockopt(socket, level, optname, (char *)optval, optlen)
;; #else
;; # include <sys/types.h>
;; # include <sys/socket.h>
;; # include <sys/time.h>
;; # include <netinet/in.h>
;; # include <netinet/tcp.h>
;; # include <unistd.h>
;; # include <signal.h>
;; # define closesocket     close
;; # define INVALID_SOCKET  -1
;; # define typecorrect_getsockopt getsockopt
;; #endif

;; #ifndef SD_RECEIVE
;; # define SD_RECEIVE      0
;; # define SD_SEND         1
;; #endif

;; #ifdef ECOS
;; #include <sys/sockio.h>
;; #endif

;; <#

;; (define-foreign-variable errno int "errno")
;; (define-foreign-variable strerror c-string "strerror(errno)")

;;; Local macros

;;(require-library srfi-13) ;;?
(import-for-syntax srfi-13)

;; (local 'so/reuseaddr) => '_so_reuseaddr
(define-for-syntax (local s)
  (string->symbol
   (string-append "_" (string-translate (symbol->string s) "/" "_"))))

;; (c-name 'so/reuseaddr) => "SO_REUSEADDR"
;; (define-socket-int so/reuseaddr) =>
;;    (begin (define-foreign-variable _so_reuseaddr "SO_REUSEADDR")
;;           (define so/reuseaddr _so_reuseaddr))
(define-syntax define-socket-int
  (er-macro-transformer
   (lambda (e r c)
     (define (c-name sym)
       (string-translate (string-upcase (symbol->string sym)) "/" "_"))
     (let ((sym (cadr e))
           (str (cddr e)))
       (let ((str (if (pair? str) (car str) (c-name sym))))
         `(,(r 'begin)
           (,(r 'define-foreign-variable) ,(local sym) ,(r 'int) ,str)
           (,(r 'define) ,sym ,(local sym))))))))

(define-syntax define-socket-ints
  (er-macro-transformer
   (lambda (e r c)
     `(,(r 'begin)
       ,@(map (lambda (sym)
                (if (pair? sym)
                    `(,(r 'define-socket-int) ,(car sym) ,(cadr sym))
                    `(,(r 'define-socket-int) ,sym)))
              (cdr e))))))

;; (define-socket-option tcp-no-delay ipproto/tcp tcp/nodelay set-int get-int) =>
;;   (begin
;;     (define tcp-no-delay
;;       (getter-with-setter
;;         (lambda (s) (get-int s _ipproto_tcp _tcp_nodelay))
;;         (lambda (s v) (set-int s _ipproto_tcp _tcp_nodelay v))))
;;     (define tcp-no-delay-set!
;;       (lambda (s v) (set-int s _ipproto_tcp _tcp_nodelay v))))

(define-syntax define-socket-option
  (er-macro-transformer
   (lambda (e r c)
     (define (setter-symbol s)
       (string->symbol (string-append (symbol->string s) "-set!")))
     (let ((name (cadr e))
           (level (caddr e))
           (optname (cadddr e))
           (set (car (cddddr e)))
           (get (cadr (cddddr e))))
       `(,(r 'begin)
          (,(r 'define) ,name
           (getter-with-setter (,(r 'lambda) (s) (,get s ,(local level) ,(local optname)))
                               (,(r 'lambda) (s v) (,set s ,(local level) ,(local optname) v))))
          (,(r 'define) ,(setter-symbol name)
           (,(r 'lambda) (s v) (,set s ,(local level) ,(local optname) v))))))))

(define-syntax define-boolean-option
  (syntax-rules ()
    ((_ name level optname)
     (define-socket-option name level optname set-boolean-option get-boolean-option))))

(define-syntax define-integer-option
  (syntax-rules ()
    ((_ name level optname)
     (define-socket-option name level optname set-integer-option get-integer-option))))

;;; FFI

(define setsockopt (foreign-lambda int "setsockopt" int int int scheme-pointer int))
(define getsockopt (foreign-lambda int "typecorrect_getsockopt" int int int scheme-pointer c-pointer))

(define setsockopt/int
  (foreign-lambda* int ((int sock) (int level) (int name) (int val))
                   "return(setsockopt(sock, level, name, (const void *)&val, sizeof(val)));"))
(define getsockopt/int
  (foreign-lambda* int ((int sock) (int level) (int name) ((c-pointer int) ret))
                   "socklen_t sz = sizeof(*ret);"
                   "return(typecorrect_getsockopt(sock, level, name, ret, &sz));"))

;;; getters and setters

(define (##sys#check-boolean x . y)
  (unless (boolean? x)
    (##sys#signal-hook #:type-error
                       (and (pair? y) (car y))
                       "bad argument type: not a boolean" x)))
(define-inline (check-error err where)
  (let ((no errno))
    (when (fx= -1 err)
      (##sys#update-errno)
      (##sys#signal-hook #:network-error where (strerror no)))))

(define (set-integer-option s level name val)
  (##sys#check-exact val 'set-socket-option!)
  (let ((s (if (socket? s) (socket-fileno s) s)))
    (let ((err (setsockopt/int s level name val)))
      (check-error err 'set-socket-option!)
      (void))))

(define (set-boolean-option s level name val)
  (##sys#check-boolean val 'set-socket-option!)
  (set-integer-option s level name (if val 1 0)))
(define (get-boolean-option s level name)
  (not (= 0 (get-integer-option s level name))))

(define (get-integer-option s level name)
  (let ((s (if (socket? s) (socket-fileno s) s)))
    (let-location ((val int))
      (let ((err (getsockopt/int s level name (location val))))
        (check-error err 'get-socket-option!)
        val))))

(define (set-readonly-option s level name val)  ; don't get a symbol here, only an int -- fixme?
  (error 'set-socket-option! "socket option is read-only"))

;;; generic lowlevel interface

;; This interface is likely to change or go away completely.  Complex manipulation
;; might be easier done in C.

;; (set-socket-option! S ipproto/tcp tcp/nodelay 1)
;; (set-socket-option! S ipproto/tcp tcp/nodelay (make-string 4 #\x0))
;; (set-socket-option! S sol/socket so/rcvlowat (u32vector->blob/shared (u32vector #x01020304)))
;; (get-socket-option! S ipproto/tcp tcp/nodelay)

;; complex example

#|
(define (make-linger-storage)
  (make-blob (foreign-value "sizeof(struct linger)" int)))
(define (encode-linger-option state time)
  (let ((blob (make-linger-storage)))
    ((foreign-lambda* void ((scheme-pointer ptr) (int onoff) (int linger))
                      "struct linger *p = ptr;"
                      "p->l_onoff = onoff; p->l_linger = linger;")
     blob state time)
    blob))
(define (decode-linger-option blob)
  ; sanity checking recommended here
  (list ((foreign-lambda* int ((scheme-pointer p)) "return(((struct linger *)p)->l_onoff);") blob)
        ((foreign-lambda* int ((scheme-pointer p)) "return(((struct linger *)p)->l_linger);") blob)))

;; (set-socket-option! S sol/socket so/linger (encode-linger-option 1 100))
;; (decode-linger-option (get-socket-option! S sol/socket so/linger (make-linger-storage)))
|#

(define (set-socket-option! s level name val)
  (let ((s (if (socket? s) (socket-fileno s) s)))
    (cond ((boolean? val)
           (set-boolean-option s level name val))
          ((fixnum? val)
           (set-integer-option s level name val))
          ((blob? val)
           (check-error (setsockopt s level name val (blob-size val)) 'set-socket-option!))
          ((string? val)
           (check-error (setsockopt s level name val (string-length val)) 'set-socket-option!))
          (else
           (##sys#signal-hook #:type-error
                              'set-socket-option!
                              "bad option value" val)))))

;; TODO: Rather than preallocated storage buf, perhaps better to
;; specify a max length and return a newly allocated (sub)string of that.
;; Well, a blob is probably better.
;; TODO: Also remove ! when done.
(define (get-socket-option! s level name . storage)
  (if (null? storage)
      (get-integer-option s level name)
      (let ((buf (car storage)))
        (if (or (string? buf) (blob? buf))
            (let-location ((sz int))
              (set! sz (number-of-bytes buf))
              (let ((s (if (socket? s) (socket-fileno s) s)))
                (check-error (getsockopt s level name buf (location sz)) 'get-socket-option!))
              buf)
            (##sys#signal-hook #:type-error
                               'get-socket-option!
                               "bad value container" buf)))))

;;; socket integers

(define-socket-ints
;; socket options
  so/reuseaddr so/debug so/acceptconn so/keepalive so/dontroute
  so/broadcast so/linger so/oobinline so/sndbuf so/rcvbuf
  so/sndlowat so/rcvlowat so/sndtimeo so/rcvtimeo so/error so/type
; so/useloopback so/reuseport so/timestamp  

;; tcp options
  tcp/nodelay
; tcp/maxseg tcp/nopush tcp/noopt tcp/keepalive

;; ip options
  ip/options ip/hdrincl ip/tos ip/ttl ip/recvopts ip/recvretopts ip/retopts
; ip/recvdstaddr
  (ip/multicast-if "IP_MULTICAST_IF")
  (ip/multicast-ttl "IP_MULTICAST_TTL")
  (ip/multicast-loop "IP_MULTICAST_LOOP")
  (ip/add-membership "IP_ADD_MEMBERSHIP")
  (ip/drop-membership "IP_DROP_MEMBERSHIP")

;; ipv6 options
  ipv6/v6only

;; socket levels
  sol/socket ipproto/ip ipproto/ipv6 ipproto/icmp
; ipproto/tcp ipproto/udp            ;; already provided in socket.scm
)

;;; socket-level options

(define-boolean-option so-reuse-address sol/socket so/reuseaddr)
(define-boolean-option so-debug sol/socket so/debug)
(define-socket-option  so-accept-connections sol/socket so/acceptconn set-readonly-option get-boolean-option)
(define-boolean-option so-keep-alive sol/socket so/keepalive)
(define-boolean-option so-dont-route sol/socket so/dontroute)
(define-boolean-option so-broadcast sol/socket so/broadcast)
;(define-socket-option so-linger sol/socket so/linger set-linger-option get-linger-option)
(define-boolean-option so-oob-inline sol/socket so/oobinline)
(define-integer-option so-send-buffer sol/socket so/sndbuf)
(define-integer-option so-receive-buffer sol/socket so/rcvbuf)
(define-integer-option so-send-low-water sol/socket so/sndlowat)
(define-integer-option so-receive-low-water sol/socket so/rcvlowat)
;(define-socket-option so-receive-timeout sol/socket so/rcvtimeo set-timeval-option get-timeval-option)
;(define-socket-option so-send-timeout sol/socket so/sndtimeo set-timeval-option get-timeval-option)
(define-socket-option  so-error sol/socket so/error set-readonly-option get-integer-option)
(define-socket-option  so-type sol/socket so/type set-readonly-option get-integer-option)

;;; TCP options

(define-boolean-option tcp-no-delay ipproto/tcp tcp/nodelay)
;(define-integer-option tcp-max-segment-size ipproto/tcp tcp/maxseg)
;(define-boolean-option tcp-no-push ipproto/tcp tcp/nopush)
;(define-boolean-option tcp-no-options ipproto/tcp tcp/noopt)
;(define-integer-option tcp-keep-alive ipproto/tcp tcp/keepalive)

;;; IP options

;; Most of the IP option interface is currently unimplemented as it
;; seems to differ widely between systems.
(define-boolean-option ip-header-included ipproto/ip ip/hdrincl)
(define-integer-option ip-type-of-service ipproto/ip ip/tos)
(define-integer-option ip-time-to-live ipproto/ip ip/ttl)

(define-boolean-option ipv6-v6-only ipproto/ipv6 ipv6/v6only)
