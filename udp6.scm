;; multicast unimplemented
;; FIXME Do we need to set v6only even on client connections?  And for TCP?

;;; Example:
;;;  csi> (use udp)
;;;  csi> (define s (udp-open-socket))
;;;  csi> (udp-bind! s #f 0)
;;;  csi> (udp-connect! s "localhost" 13)  ; daytime service
;;;  csi> (udp-send s "\n")
;;;  csi> (receive (n data from-host from-port) (udp-recvfrom s 64)
;;;         (print* n " bytes from " from-host ":" from-port ": " data))
;;;  26 bytes from 127.0.0.1:13: Wed Dec 24 11:53:14 2003
;;;  csi> (udp-close-socket s)
;;;  csi>


; ----------------------------TERMS OF USE--------------------------------
; Copyright (c) 2011, Jim Ursetto
; Copyright (c) 2003-2004, Category 5
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are
; met:
;
;   Redistributions of source code must retain the above copyright notice,
;   this list of conditions and the following disclaimer. Redistributions in
;   binary form must reproduce the above copyright notice, this list of
;   conditions and the following disclaimer in the documentation and/or
;   other materials provided with the distribution. Neither the name of the
;   author nor the names of its contributors may be used to endorse or
;   promote products derived from this software without specific prior
;   written permission.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
; THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
; PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
; ----------------------------TERMS OF USE--------------------------------

(import scheme chicken extras foreign srfi-1 srfi-18)


;;; error-signaling calls
(define udp-error
  (lambda args
    (##sys#update-errno)
    (apply ##sys#signal-hook #:network-error args)))

(define (udp-socket? s)
  (and (socket? s)
       (= (socket-type s) sock/dgram)))
(define (udp-bound? s)                     ;; does not check if sock/dgram
  (and (socket-name s)))
(define (udp-connected? s)                 ;; does not check if sock/dgram
  (and (socket-peer s)))

;;; udp-open-socket : -> udp-socket
(define (udp-open-socket #!optional (family 'inet))
  (let ((af (case family
              ((inet) af/inet) ((inet6) af/inet6)
              (else (udp-error "invalid address family" family)))))
    (socket af sock/dgram 0)))

;;; udp-open-socket* : -> udp-socket
;;; open a UDP socket and make it nonblocking
(define udp-open-socket* udp-open-socket)

;;; udp-bind! : udp-socket host-string port-number -> unspecified
;;; bind a socket to a local address (possibly INADDR_ANY) and port
;; If host #f, binds to the unspecified address.
(define (udp-bind! so host port)
  (let ((ais (address-information host port family: (socket-family so)
                                  type: sock/dgram flags: ai/passive)))
    (when (null? ais)
      (udp-error 'udp-bind! "node or service lookup failed" host port))
    (let* ((ai (car ais))
           (addr (addrinfo-address ai)))
      (socket-bind! so addr))))

(define (udp-bound-port so)
  (let ((addr (socket-name so)))
    (if addr
        (sockaddr-port addr)
        0)))

;;; udp-connect! : udp-socket host-string port -> unspecified
;;; "connect" a socket.  In the case of UDP this does nothing more than
;;; store a peer address in the kernel socket structure for use with
;;; later calls to send(2).

;; Host may be a string like host:port or [host]:port, in which case PORT should be #f.
(define (udp-connect! so host #!optional port)
  (unless port
    (set-values! (host port) (parse-inet-address hoststr))
    (unless port
      (udp-error 'udp-connect! "no port specified")))
  (let ((ais (address-information host port family: (socket-family so)
                                  type: sock/dgram)))
    (when (null? ais)
      (udp-error 'udp-connect! "node and/or service lookup failed"))
    (socket-connect! so (addrinfo-address (car ais)))))

;; Maybe add udp-connect and/or udp-connect/ai

;;; udp-send : udp-socket string [start end flags] -> unspecified
;;; send bytes in string to the peer for this socket as specified earlier
;;; with udp-connect!.  If the socket was not "connected", send(2) will
;;; raise an error.
(define udp-send socket-send!)

;;; udp-sendto : udp-socket host-string port-num string -> unspecified
;;; send bytes in string to host:port via udp-socket.

(define (udp-sendto so host port str)
  (let ((ais (address-information host port family: (socket-family so)
                                  type: sock/dgram)))
    (when (null? ais)
      (udp-error 'udp-sendto "node and/or service lookup failed" so host port))
    (let* ((ai (car ais))
           (saddr (addrinfo-address ai)))
      (socket-send-to! so str saddr))))

;;; udp-recv : udp-socket string [flags] -> [len packet]
;;; receive a packet and store the data in string, returning the
;;; length of the packet and the substring of len bytes.
(define udp-recv socket-receive)

;;; udp-recvfrom : udp-socket string -> [len packet host-string port-num]
;;; like recv but returns four values, including the length of the
;;; received packet and the host and port from which it was received.

(define (udp-recvfrom so len)
  (let-values (((str saddr) (socket-receive-from so len)))
    (values (string-length str) str
            (sockaddr-address saddr)
            (sockaddr-port saddr))))

;;; udp-close-socket : udp-socket -> bool
;;; close a socket.
(define (udp-close-socket so)
  (socket-close! so))


