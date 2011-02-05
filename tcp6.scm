;; tcp-connect should be allowed to connect to only 1 address (or addrinfo list)

;; added tcp-bind-ipv6-only param; if af/inet6, will set IPV6_V6ONLY option on socket
;; tcp-listen accepts service name string
;; tcp-connect accepts service name string (may issue SRV request)
;; tcp-connect connects to multiple addresses (or explicitly with tcp-connect/ai)

;; creating a 'socket port (slot 7) will allow posixunix to call ##sys#tcp-port->fileno,
;; which will bomb

;; On XP, you must do 'netsh interface ipv6 install' to activate ipv6.

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

;;(include "common-declarations.scm")

;; (register-feature! 'tcp)

(define-inline (tcp-error where msg . args)
  (apply ##sys#signal-hook #:network-error where msg args))

;; Force tcp4 for (tcp-listen port) when v6only enabled.  This will fail
;; on an IPv6-only system.  Assume when host is unspecified, the first addrinfo
;; result on a dual-stack system is "::".  If it is "0.0.0.0", IPv6 will be disabled.
(define (bind-tcp-socket port host)
  (let* ((family (if (and (not host) (tcp-bind-ipv6-only))
		     af/inet #f))
	 (ai (address-information host port family: family
				  type: sock/stream flags: ai/passive)))
    (when (null? ai)
      (tcp-error 'tcp-listen "node or service lookup failed" host port))
    (let* ((ai (car ai))
	   (addr (addrinfo-address ai)))
    (let* ((so (socket (addrinfo-family ai) (addrinfo-socktype ai) 0))
	   (s (socket-fileno so)))
      (set-socket-reuseaddr! so #t)
      (when (= (addrinfo-family ai) af/inet6)
        (set-socket-v6only! so (tcp-bind-ipv6-only)))
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
  (let ((ais (filter (lambda (ai) (eq? (addrinfo-protocol ai) ipproto/tcp))
                     ais))) ;; Filter first to preserve our "last exception" model.
    (parameterize ((socket-connect-timeout (tcp-connect-timeout)))
      (##net#io-ports (socket-connect/ai ais)))))

(define (tcp-connect host . more)
  (let ((port (optional more #f)))
    (##sys#check-string host)
    (unless port
      (set!-values (host port) (parse-inet-address host))
      (unless port (network-error 'tcp-connect "no port specified" host)))
    (let ((ais (address-information host port type: sock/stream)))  ;; protocol: problematic on WIN
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
    (let ((so (make-socket fd 0 0 0)))   ;; temporary -- until we get socket associated w/ port
      (values
       (sockaddr-address (socket-name so))
       (sockaddr-address (socket-peer-name so))))))

(define (tcp-port-numbers p)
  (##sys#check-port p 'tcp-port-numbers)
  (let ((fd (##sys#tcp-port->fileno p)))
    (let ((so (make-socket fd 0 0 0)))
      (values
       (sockaddr-port (socket-name so))
       (sockaddr-port (socket-peer-name so))))))

(define (tcp-listener-port tcpl)
  (let ((fd (tcp-listener-fileno tcpl)))
    (let ((so (make-socket fd 0 0 0)))
      (socket-port (socket-name so)))
    port))

(define (tcp-abandon-port p)
  (##sys#check-port p 'tcp-abandon-port)
  (##sys#setislot
   (##sys#port-data p)
   (if (##sys#slot p 1) 2 1)
   #t) )
