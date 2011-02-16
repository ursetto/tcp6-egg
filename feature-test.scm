(define-syntax R
 (syntax-rules ()
  ((_ F) (begin (define-foreign-variable F bool)
                ((if F R! U!) 'F)))))
(define-syntax D
  (er-macro-transformer
   (lambda (e r c)
     (let ((d (->string (cadr e))))
       `(,(r 'foreign-declare)
         ,(sprintf "#ifdef ~A\n#define HAVE_~A 1\n#else \n#define HAVE_~A 0\n#endif\n"
                   d d d))))))
(define-syntax DR
  (lambda (e r c)
    (let ((d (->string (cadr e))))
      `(,(r 'begin)
        (,(r 'D) ,d)
        (,(r 'R) ,(string->symbol (string-append "HAVE_" d)))))))

(define R! (lambda (f) (printf "(register-feature! '~S)\n" f)))
(define U! (lambda (f) (printf "(unregister-feature! '~S)\n" f)))

