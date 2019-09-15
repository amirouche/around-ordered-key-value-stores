(import (only (chezscheme) import))

(import (scheme base))
(import (scheme list))
(import (scheme generator))
(import (scheme process-context))
(import (scheme write))

(import (nstore))
(import (pack))
(import (cffi wiredtiger okvs))

(import (only (chezscheme) open-input-file))
(import (srfi :130)) ;; cursor-based string library, TODO: replace with (scheme textual)

(define triplestore
  (let ((engine (nstore-engine okvs-ref okvs-set! okvs-delete! okvs-prefix-range pack unpack)))
    (nstore engine (list 0) '(subject predicate object))))

;; TODO: configure eviction and cache
(define database (okvs-open '((home . "wt") (create? . #t))))

(define tab (list->string (list #\tab)))

(define (stream-relations filepath)
  (let ((port (open-input-file filepath '(unbuffered))))
    (let loop1 ((char (read-char port)))
      (lambda ()
        (if (eof-object? char)
            (values #f #f)
            (let loop2 ((char (read-char port))
                        (out (list char)))
              (if (char=? char #\newline)
                  (values (take (drop (string-split
                                       (list->string (reverse out)) tab) 1) 3)
                          (loop1 (read-char port)))
                  (loop2 (read-char port) (cons char out)))))))))

(define filepath (cadr (command-line)))

(let loop ((next (stream-relations filepath)))
  (call-with-values next
    (lambda (value next)
      (when value
        (display ".")
        (okvs-in-transaction database
                             (lambda (transaction)
                               (nstore-add! transaction triplestore
                                            (list (list-ref value 1)
                                                  (list-ref value 0)
                                                  (list-ref value 2)))))
        (loop next)))))

(okvs-close database)
