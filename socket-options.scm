;;; Local macros

;;(require-library srfi-13) ;;?
(import-for-syntax srfi-13)

;; ;; (local 'ip/multicast-ttl) => '_ip_multicast_ttl
;; (define-for-syntax (local s)
;;   (string->symbol
;;    (string-append "_" (string-translate (symbol->string s) "/-" #\_))))
;; (local 'ip/multicast-ttl) => 'IP_MULTICAST_TTL
(define-for-syntax (local sym)
  (string->symbol
   (string-translate (string-upcase (symbol->string sym)) "/-" #\_)))
;; (c-name 'ip/multicast-ttl) => "IP_MULTICAST_TTL"
(define-for-syntax (c-name sym)
  (string-translate (string-upcase (symbol->string sym)) "/-" #\_))

;; (define-socket-int so/reuseaddr) =>
;;    (begin (define-foreign-variable SO_REUSEADDR "SO_REUSEADDR")
;;           (define so/reuseaddr (if (= SO_REUSEADDR -1) #f SO_REUSEADDR)))
(define-syntax define-socket-int
  (er-macro-transformer
   (lambda (e r c)
     (let ((sym (cadr e))
           (str (cddr e)))
       (let ((str (if (pair? str) (car str) (c-name sym)))
             (lname (local sym)))
         `(,(r 'begin)
           (,(r 'define-foreign-variable) ,lname ,(r 'int) ,str)
           (,(r 'define) ,sym ,lname)))))))

(define-syntax define-socket-ints
  (er-macro-transformer
   (lambda (e r c)
     `(,(r 'begin)
       ,@(map (lambda (sym)
                (if (pair? sym)
                    `(,(r 'define-socket-int) ,(car sym) ,(cadr sym))
                    `(,(r 'define-socket-int) ,sym)))
              (cdr e))))))

;; (define-optional-socket-int so/reuseaddr)
;;  => (cond-expand (SO_REUSEADDR
;;                   (define-foreign-variable SO_REUSEADDR int "SO_REUSEADDR")
;;                   (define so/reuseaddr SO_REUSEADDR))
;;                  (else
;;                   (define so/reuseaddr #f))))
(define-syntax define-optional-socket-int
  (er-macro-transformer
   (lambda (e r c)
     (let ((sym (cadr e))
           (str (cddr e)))
       (let ((str (if (pair? str) (->string (car str)) (c-name sym)))
             (lname (local sym)))
         (let ((feat (string->symbol str)))
           `(,(r 'cond-expand)
             (,feat 
              (,(r 'define-foreign-variable) ,lname ,(r 'int) ,str)
              (,(r 'define) ,sym ,lname))
             (,(r 'else)
              (,(r 'define) ,sym #f)))))))))

(define-syntax define-optional-socket-ints
  (er-macro-transformer
   (lambda (e r c)
     `(,(r 'begin)
       ,@(map (lambda (sym)
                (if (pair? sym)
                    `(,(r 'define-optional-socket-int) ,(car sym) ,(cadr sym))
                    `(,(r 'define-optional-socket-int) ,sym)))
              (cdr e))))))

;; (define-socket-option ipv6-v6-only? ipproto/ipv6 ipv6/v6only set-boolean-option get-boolean-option) =>
;; (begin
;;   (define ipv6-v6-only?
;;     (if (or (= _ipproto_ipv6 -1) (= _ipv6_v6only -1))
;;       (getter-with-setter
;;         (lambda (s)
;;           (unsupported-error 'ipv6-v6-only? "socket option or level unsupported"))
;;         (lambda (s v)
;;           (unsupported-error 'ipv6-v6-only? "socket option or level unsupported")))
;;       (getter-with-setter
;;         (lambda (s) (get-boolean-option s _ipproto_ipv6 _ipv6_v6only))
;;         (lambda (s v) (set-boolean-option s _ipproto_ipv6 _ipv6_v6only v))))))

;; When option or level undefined, define the procedure to simply
;; return a nice error.  We could pass an invalid option or level
;; (such as -1) through to get/setsockopt, but this is more meaningful
;; and safer.  (Note this does use the foreign-vars instead of
;; the constants, so it needs to test for -1 instead of #f.)

(define (unsupported-socket-option name)
  ;; Deduplicates code in define-socket-option.  More savings could
  ;; be achieved by not printing "name".
  (unsupported-error name "socket option unavailable on this platform"))

(define-syntax define-socket-option
  (er-macro-transformer
   (lambda (e r c)
     (let ((name (cadr e))
           (level (caddr e))
           (optname (cadddr e))
           (set (car (cddddr e)))
           (get (cadr (cddddr e))))
       `(,(r 'define) ,name
         (,(r 'getter-with-setter)
          (,(r 'lambda) (s) (,get ',name s ,(local level) ,(local optname)))
          (,(r 'lambda) (s v) (,set ',name s ,(local level) ,(local optname) v))))))))

(define-syntax define-boolean-option
  (syntax-rules ()
    ((_ name level optname)
     (define-socket-option name level optname set-boolean-option get-boolean-option))))

(define-syntax define-integer-option
  (syntax-rules ()
    ((_ name level optname)
     (define-socket-option name level optname set-integer-option get-integer-option))))

;; Like define-socket-option, but performs a feature test on the level and optname,
;; choosing whether option is supported at compile time instead of runtime.
;; Assumes foreign variables have been declared by define-optional-socket-ints.
(define-syntax define-optional-socket-option
  (er-macro-transformer
   (lambda (e r c)
     (define (feature-name x) (string->symbol (c-name x)))
     (let ((name (cadr e))
           (level (caddr e))
           (optname (cadddr e))
           (set (car (cddddr e)))
           (get (cadr (cddddr e)))
           (%unsup (gensym)))
       `(,(r 'define) ,name
         (,(r 'cond-expand)
          ((,(r 'and) ,(feature-name level) ,(feature-name optname))
           (,(r 'getter-with-setter)
            (,(r 'lambda) (s) (,get ',name s ,(local level) ,(local optname)))
            (,(r 'lambda) (s v) (,set ',name s ,(local level) ,(local optname) v))))
          (,(r 'else)
           (,(r 'let) ((,%unsup
                        (,(r 'lambda) _
                         (,(r 'unsupported-socket-option) ',name))))
            (,(r 'getter-with-setter) ,%unsup ,%unsup)))))))))

(define-syntax define-optional-boolean-option
  (syntax-rules ()
    ((_ name level optname)
     (define-optional-socket-option name level optname
       set-boolean-option get-boolean-option))))

(define-syntax define-optional-integer-option
  (syntax-rules ()
    ((_ name level optname)
     (define-optional-socket-option name level optname
       set-integer-option get-integer-option))))

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

(define-inline (check-boolean where x)
  (unless (boolean? x)
    (type-error where "bad argument type: not a boolean" x)))
(define-inline (check-error where err)
  (let ((no errno))
    (when (fx= -1 err)
      (if (or (fx= no _enoprotoopt)  ;; False + on  Win for e.g. sock/dgram when stream expected
              (fx= no _einval))      ;; Maybe incorrect level; but false + on dgram when stream expected
          (unsupported-error where (strerror no))
          (begin
            (##sys#update-errno)
            (##sys#signal-hook #:network-error where (strerror no)))))))

(define (set-integer-option where s level name val)
  (##sys#check-exact val where)
  (let ((s (if (socket? s) (socket-fileno s) s)))
    (let ((err (setsockopt/int s level name val)))
      (check-error where err)
      (void))))

(define (set-boolean-option where s level name val)
  (check-boolean where val)
  (set-integer-option where s level name (if val 1 0)))
(define (get-boolean-option where s level name)
  (not (= 0 (get-integer-option where s level name))))

(define (get-integer-option where s level name)
  (let ((s (if (socket? s) (socket-fileno s) s)))
    (let-location ((val int))
      (let ((err (getsockopt/int s level name (location val))))
        (check-error where err)
        val))))

(define (set-readonly-option where s level name val)
  (network-error where "socket option is read-only"))

;;; generic lowlevel interface

;; This interface is likely to change or go away completely.  Complex manipulation
;; might be easier done in C.

;; (set-socket-option S ipproto/tcp tcp/nodelay 1)
;; (set-socket-option S ipproto/tcp tcp/nodelay (make-string 4 #\x0))
;; (set-socket-option S sol/socket so/rcvlowat (u32vector->blob/shared (u32vector #x01020304)))
;; (get-socket-option S ipproto/tcp tcp/nodelay)

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

;; (set-socket-option S sol/socket so/linger (encode-linger-option 1 100))
;; (decode-linger-option (get-socket-option S sol/socket so/linger (make-linger-storage)))
|#

(define (set-socket-option s level name val)
  (cond ((not level)
         (unsupported-error 'set-socket-option "socket option level not supported"))
        ((not name)
         (unsupported-error 'set-socket-option "socket option not supported"))
        (else
         (let ((s (if (socket? s) (socket-fileno s) s)))
           (cond ((boolean? val)
                  (set-boolean-option 'set-socket-option s level name val))
                 ((fixnum? val)
                  (set-integer-option 'set-socket-option s level name val))
                 ((blob? val)
                  (check-error 'set-socket-option
                               (setsockopt s level name val (blob-size val))))
                 ((string? val)
                  (check-error 'set-socket-option
                               (setsockopt s level name val (string-length val))))
                 (else
                  (##sys#signal-hook #:type-error
                                     'set-socket-option
                                     "bad option value" val)))))))

;; Get socket option on socket S at socket level LEVEL with option name NAME.
;; If len is #f (the default) it assumes the option is an integer value.
;; Otherwise allocates temporary space of LEN bytes and copies the result into
;; a fresh blob of the length returned by the getsockopt() call; returns the blob.
;; If you know the correct length ahead of time, no copy is done.
;; (get-socket-option s sol/socket so/reuseaddr 1024) => #${04000000}
;; (get-socket-option s sol/socket so/reuseaddr)      => 4
(define (get-socket-option s level name #!optional len)
  (cond ((not level)
         (unsupported-error 'get-socket-option "socket option level not supported"))
        ((not name)
         (unsupported-error 'get-socket-option "socket option not supported"))
        ((not len)
         (get-integer-option 'get-socket-option s level name))
        (else
         (let ((buf (make-blob len)))
           (let-location ((sz int len))
             (let ((s (if (socket? s) (socket-fileno s) s)))
               ;; FIXME: Report unsupported error correctly
               (check-error 'get-socket-option (getsockopt s level name buf (location sz))))
             (if (= sz len)
                 buf
                 (let ((retbuf (make-blob sz)))
                   ((foreign-lambda void C_memcpy scheme-pointer scheme-pointer int)
                    retbuf buf sz)
                   retbuf)))))))

;;; socket integers

;; Optional socket ints must be defined as foreign features.
(define-optional-socket-ints
  so/useloopback so/reuseport so/timestamp so/exclusiveaddruse

  tcp/maxseg tcp/nopush tcp/noopt tcp/keepalive

  ip/mtu ip/mtu-discover
  ip/pktinfo ip/recverr ip/recvtos ip/recvttl ip/router-alert 
  ip/recvopts ip/recvretopts ip/retopts ip/recvdstaddr

  ;; NB There's probably a subset of IPv6 options these that we can require
  ;; when IPv6 is enabled (i.e. error out on if undefined).
  ipv6/v6only ipv6/addrform ipv6/mtu
  ipv6/mtu-discover ipv6/multicast-hops ipv6/multicast-if ipv6/multicast-loop ipv6/pktinfo 
  ipv6/rthdr ipv6/authhdr ipv6/dstopts ipv6/hopopts ipv6/flowinfo ipv6/hoplimit
  ipv6/recverr ipv6/router-alert ipv6/unicast-hops ipv6/nexthop
  ipv6/port-range ipv6/join-group ipv6/leave-group ipv6/checksum
  ;; ipv6/add-membership ipv6/drop-membership   ;; OBSOLETE synonyms for JOIN/LEAVE_GROUP
  ;; ipv6/options ipv6/recvopts ipv6/recvretopts ipv6/retopts ipv6/recvdstaddr ;; DEPRECATED

  ipproto/ipv6)

(define-socket-ints
;; socket options
  so/reuseaddr so/debug so/acceptconn so/keepalive so/dontroute
  so/broadcast so/linger so/oobinline so/sndbuf so/rcvbuf
  so/sndlowat so/rcvlowat so/sndtimeo so/rcvtimeo so/error so/type

;; tcp options
  tcp/nodelay

;; ip options
  ip/options ip/hdrincl ip/tos ip/ttl
  ip/multicast-if ip/multicast-ttl ip/multicast-loop
  ip/add-membership ip/drop-membership

;; ipv6 options
  
;; socket levels
  sol/socket ipproto/ip ipproto/icmp
; ipproto/tcp ipproto/udp            ;; already provided in socket.scm
)

;;; socket-level options

(cond-expand
 ((and windows SO_EXCLUSIVEADDRUSE)
  ;; Windows semantics of so/reuseaddr are basically nonsense,
  ;; so use so/exclusiveaddruse for correct semantics.  However,
  ;; this may fail without admin privs on WinXP<SP3 and Win2k<SP4,
  ;; so on failure fall back to so/reuseaddr (better than nothing).
  ;; Also, so/exclusiveaddruse may not be available, so we explicitly feature
  ;; test for it; define-socket-option expects the foreign var to be defined,
  ;; and define-optional-socket-option won't fall back to so/reuseaddr.
  (define (set-reuse-option where s level name val)
    (handle-exceptions exn
        (set-boolean-option where s level so/reuseaddr val)
      (set-boolean-option where s level name val)))
  (define (get-reuse-option where s level name)
    (handle-exceptions exn
        (get-boolean-option where s level so/reuseaddr)
      (get-boolean-option where s level name)))

  (define-socket-option so-reuse-address? sol/socket so/exclusiveaddruse
                        set-reuse-option get-reuse-option))
 (else
  (define-boolean-option so-reuse-address? sol/socket so/reuseaddr)))

(define-boolean-option so-debug? sol/socket so/debug)
(define-socket-option  so-accept-connections? sol/socket so/acceptconn set-readonly-option get-boolean-option)
(define-boolean-option so-keep-alive? sol/socket so/keepalive)
(define-boolean-option so-dont-route? sol/socket so/dontroute)
(define-boolean-option so-broadcast? sol/socket so/broadcast)
;(define-socket-option so-linger sol/socket so/linger set-linger-option get-linger-option)
(define-boolean-option so-oob-inline? sol/socket so/oobinline)
(define-integer-option so-send-buffer sol/socket so/sndbuf)
(define-integer-option so-receive-buffer sol/socket so/rcvbuf)
(define-integer-option so-send-low-water sol/socket so/sndlowat)
(define-integer-option so-receive-low-water sol/socket so/rcvlowat)
;(define-socket-option so-receive-timeout sol/socket so/rcvtimeo set-timeval-option get-timeval-option)
;(define-socket-option so-send-timeout sol/socket so/sndtimeo set-timeval-option get-timeval-option)
(define-socket-option  so-error sol/socket so/error set-readonly-option get-integer-option)
(define-socket-option  so-type sol/socket so/type set-readonly-option get-integer-option)

;;; TCP options

(define-boolean-option tcp-no-delay? ipproto/tcp tcp/nodelay)
(define-optional-integer-option tcp-max-segment-size ipproto/tcp tcp/maxseg)
(define-optional-boolean-option tcp-no-push? ipproto/tcp tcp/nopush)
(define-optional-boolean-option tcp-no-options? ipproto/tcp tcp/noopt)
(define-optional-integer-option tcp-keep-alive ipproto/tcp tcp/keepalive)

;;; IP options

;; Most of the IP option interface is currently unimplemented as it
;; seems to differ widely between systems.
;; TODO Multicast should be implemented if present.
(define-boolean-option ip-header-included? ipproto/ip ip/hdrincl)
(define-integer-option ip-type-of-service ipproto/ip ip/tos)
(define-integer-option ip-time-to-live ipproto/ip ip/ttl)

(define-optional-socket-option ipv6-v6-only? ipproto/ipv6 ipv6/v6only
  set-boolean-option get-boolean-option)
;;(define-boolean-option ipv6-v6-only? ipproto/ipv6 ipv6/v6only)

