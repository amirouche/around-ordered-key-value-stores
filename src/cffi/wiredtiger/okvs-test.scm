(define-library (cffi wiredtiger okvs-test)

  (export database-open-close)
  (export set-and-get)
  (export set-overwrite-and-ref)
  (export range)
  (export lexicographic-range)
  (export prefix)
  (export prefix-offset-limit-reverse)
  (export in-transaction-with-database)
  (export search-startswith-out-of-range-key)

  (import (arew tests))
  (import (arew))
  (import (cffi wiredtiger okvs))
  (import (prefix (arew filename) filename:))
  (import (scheme base))
  (import (scheme generator))

  (begin

    (define-syntax-rule (with-directory name body ...)
      (begin
        (when (filename:exists? name)
          (filename:delete name))
        (filename:make name)
        (let ((out (begin body ...)))
          (filename:delete name)
          out)))

    (define-syntax-rule (test* expected computed)
      (test expected (with-directory "wt" computed)))

    (define database-open-close
      (test*
       #t
       (let ((okvs (okvs-open '((home . "wt") (create? . #t)))))
         (okvs-close okvs)
         #t)))

    (define set-and-get
      (test*
       #vu8(1 2 3 42)
       (let ((okvs (okvs-open '((home . "wt") (create? . #t)))))
         ;; set
         (let ((transaction (okvs-transaction-begin okvs)))
           (okvs-set! transaction #vu8(13 37) #vu8(1 2 3 42))
           (okvs-transaction-commit transaction))
         ;; get
         (let* ((transaction (okvs-transaction-begin okvs))
                (out (okvs-ref transaction #vu8(13 37))))
           (okvs-transaction-commit transaction)
           (okvs-close okvs)
           out))))

    (define set-overwrite-and-ref
      (test*
       #vu8(42)
       (let ((okvs (okvs-open '((home . "wt") (create? . #t)))))
         ;; set
         (let ((transaction (okvs-transaction-begin okvs)))
           (okvs-set! transaction #vu8(13 37) #vu8(1 2 3 42))
           (okvs-transaction-commit transaction))
         ;; overwrite
         (let ((transaction (okvs-transaction-begin okvs)))
           (okvs-set! transaction #vu8(13 37) #vu8(42))
           (okvs-transaction-commit transaction))
         ;; get
         (let* ((transaction (okvs-transaction-begin okvs))
                (out (okvs-ref transaction #vu8(13 37))))
           (okvs-transaction-commit transaction)
           (okvs-close okvs)
           out))))

    (define range
      (test*
       (list (cons #vu8(20 16) #vu8(2)) (cons #vu8(20 17) #vu8(3)))
       (let ((okvs (okvs-open '((home . "wt") (create? . #t)))))
         ;; set
         (let ((transaction (okvs-transaction-begin okvs)))
           (okvs-set! transaction #vu8(20 18) #vu8(4))
           (okvs-set! transaction #vu8(20 16) #vu8(2))
           (okvs-set! transaction #vu8(20 15) #vu8(1))
           (okvs-set! transaction #vu8(20 19) #vu8(5))
           (okvs-set! transaction #vu8(20 17) #vu8(3))
           (okvs-transaction-commit transaction))
         ;; get
         (let* ((transaction (okvs-transaction-begin okvs))
                (out (generator->list
                      (okvs-range transaction #vu8(20 16) #t #vu8(20 18) #f))))
           (okvs-close okvs)
           out))))

    (define lexicographic-range
      (test*
       (list (cons #vu8(20 16) #vu8(2)) (cons #vu8(20 17 01) #vu8(3)))
       (let ((okvs (okvs-open '((home . "wt") (create? . #t)))))
         ;; set
         (let ((transaction (okvs-transaction-begin okvs)))
           (okvs-set! transaction #vu8(20 18) #vu8(4))
           (okvs-set! transaction #vu8(20 16) #vu8(2))
           (okvs-set! transaction #vu8(20 15) #vu8(1))
           (okvs-set! transaction #vu8(20 19) #vu8(5))
           ;; #vu8(20 17 01) lexicographically less than #vu8(20 18)
           (okvs-set! transaction #vu8(20 17 01) #vu8(3))
           (okvs-transaction-commit transaction))
         ;; get
         (let* ((transaction (okvs-transaction-begin okvs))
                (out (generator->list
                      (okvs-range transaction #vu8(20 16) #t #vu8(20 18) #f))))
           (okvs-close okvs)
           out))))

    (define prefix
      (test*
       '((#vu8(20 16) . #vu8(2))
         (#vu8(20 16 1) . #vu8(2))
         (#vu8(20 17) . #vu8(3))
         (#vu8(20 17 1) . #vu8(2)))
       (let ((okvs (okvs-open '((home . "wt") (create? . #t)))))
         ;; set
         (let ((transaction (okvs-transaction-begin okvs)))
           (okvs-set! transaction #vu8(20 17 01) #vu8(2))
           (okvs-set! transaction #vu8(20 17) #vu8(3))
           (okvs-set! transaction #vu8(42 42) #vu8(5))
           (okvs-set! transaction #vu8(01 02) #vu8(1))
           (okvs-set! transaction #vu8(20 16) #vu8(2))
           (okvs-set! transaction #vu8(20 16 01) #vu8(2))
           (okvs-transaction-commit transaction))
         ;; get
         (let* ((transaction (okvs-transaction-begin okvs))
                (out (generator->list (okvs-prefix-range transaction #vu8(20)))))
           (okvs-close okvs)
           out))))

    (define prefix-offset-limit-reverse
      (test*
       '((#vu8(20 17) . #vu8(3))
         (#vu8(20 16 1) . #vu8(2)))
       (let ((okvs (okvs-open '((home . "wt") (create? . #t)))))
         ;; set
         (let ((transaction (okvs-transaction-begin okvs)))
           (okvs-set! transaction #vu8(20 17 01) #vu8(2))
           (okvs-set! transaction #vu8(20 17) #vu8(3))
           (okvs-set! transaction #vu8(42 42) #vu8(5))
           (okvs-set! transaction #vu8(01 02) #vu8(1))
           (okvs-set! transaction #vu8(20 16) #vu8(2))
           (okvs-set! transaction #vu8(20 16 01) #vu8(2))
           (okvs-transaction-commit transaction))
         ;; get
         (let* ((transaction (okvs-transaction-begin okvs))
                (out (generator->list (okvs-prefix-range transaction
                                                         #vu8(20)
                                                         '((offset . 1)
                                                           (limit . 2)
                                                           (reverse? #t))))))
           (okvs-close okvs)
           out))))

    (define query
      (lambda (transaction)
        (okvs-set! transaction #vu8(42) #vu8(42))
        (list (okvs-ref transaction #vu8(42))
              (okvs-ref transaction #vu8(42)))))

    (define in-transaction-with-database
      (test*
       (list #vu8(42) #vu8(42))
       (let ((okvs (okvs-open '((home . "wt") (create? . #t)))))
         (let ((out (okvs-in-transaction okvs query)))
           (okvs-close okvs)
           out))))

    (define search-startswith-out-of-range-key
      (test*
       '()
       (let ((keys '(#vu8(1 42 0 20 2 55 97 98 53 118 54 110 103 113 119 49 117 53 121 111 57 50 104 110 107 105 109 112 105 104 0 21 102 21 103)
                         #vu8(1 42 0 21 1 21 102 21 103 2 55 97 98 53 118 54 110 103 113 119 49 117 53 121 111 57 50 104 110 107 105 109 112 105 104 0)
                         #vu8(1 42 0 21 2 21 103 2 55 97 98 53 118 54 110 103 113 119 49 117 53 121 111 57 50 104 110 107 105 109 112 105 104 0 21 102))))
         (let ((okvs (okvs-open '((home . "wt") (create? . #t)))))
           ;; set
           (let ((transaction (okvs-transaction-begin okvs)))
             (let loop ((keys keys))
               (unless (null? keys)
                 (okvs-set! transaction (car keys) #vu8(2))
                 (loop (cdr keys))))
             (okvs-transaction-commit transaction))
           ;; get
           (let* ((transaction (okvs-transaction-begin okvs))
                  (prefix #vu8(1 42 0 20 2 57 98 57 55 54 97 104 97 104 50 51 113 110 52 102 121 97 99 49 53 120 99 118 48 100 0))
                  (out (generator->list (okvs-prefix-range transaction prefix))))
             (okvs-close okvs)
             out)))))

    ))
