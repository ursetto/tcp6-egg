(define-syntax R
  (lambda (e r c)
    (let ((F (->string (cadr e))))
      (let ((%begin (r 'begin))
            (%define-foreign-variable (r 'define-foreign-variable))
            (%bool (r 'bool)) (%quote (r 'quote))
            (%if (r 'if)) (%R! (r 'R!)) (%U! (r 'U!)))
        (let* ((cvar (string-append *ft:definition-prefix* F))
               (var (string->symbol cvar))
               (ft (string->symbol (string-append *ft:registration-prefix* F))))
          `(,%begin (,%define-foreign-variable ,var ,%bool ,cvar)
                    ((,%if ,var ,%R! ,%U!)
                     (,%quote ,ft))))))))
(define-syntax D
  (er-macro-transformer
   (lambda (e r c)
     (let ((d (->string (cadr e)))
           (dp *ft:definition-prefix*))
       `(,(r 'foreign-declare)
         ,(sprintf "#ifdef ~A\n#define ~A~A 1\n#else \n#define ~A~A 0\n#endif\n"
                   d dp d dp d))))))
(define-syntax DR
  (syntax-rules () ((DR F) (begin (D F) (R F)))))
(define-syntax DP
  (lambda (e r c)
    (set! *ft:definition-prefix* (->string (cadr e)))
    `(,(r 'begin))))
(define-syntax RP
  (lambda (e r c)
    (set! *ft:registration-prefix* (->string (cadr e)))
    `(,(r 'begin))))

(define *definition-prefix* "HAVE_")
(define *registration-prefix* "")

(define R! (lambda (f) (printf "(register-feature! '~S)\n" f)))
(define U! (lambda (f) (printf "(unregister-feature! '~S)\n" f)))

;; (define ?!
;;   (lambda ()
;;     (write 
;;      '(set-sharp-read-syntax! #\?
;;                               (lambda (p)
;;                                 (let ((ft (read p))
;;                                       (body (read p)))
;;                                   (if (feature? ft)
;;                                       body
;;                                       (values))))))
;;     (newline)))

;; FIXME: Should eval a cond-expand form
(define ?!
  (lambda ()
    (for-each (lambda (x) (write x) (newline))
              `(
                (set-sharp-read-syntax!
                  #\+ (lambda (p) (let ((ft (read p))
                                   (body (read p)))
                               (eval
                                `(cond-expand (,ft ',body)
                                              (else '(##core#undefined)))) ;; should be (values) if reader patched
                               )))
                (set-sharp-read-syntax!
                  #\- (lambda (p) (let ((ft (read p))
                                   (body (read p)))
                               (eval
                                `(cond-expand (,ft '(##core#undefined))   ;; should be (values) if reader patched
                                              (else ',body))))))
                (set-sharp-read-syntax!
                 #\? (lambda (p) (let* ((test (read p))
                                   (ft (car test))
                                   (con (cadr test))
                                   (alt (cddr test)))  ;; alt optional; maybe should not be
                              (eval
                               `(cond-expand (,ft ',con)
                                             (else
                                              ,(if (null? alt)
                                                   '(##core#undefined)
                                                   (list 'quote (car alt)))))))))))))
