(import (only (chezscheme) import))

(import (scheme base))
(import (scheme list))
(import (scheme generator))
(import (scheme process-context))
(import (scheme write))

(import (nstore))
(import (pack))
(import (cffi wiredtiger okvs))
(import (fuzzy))

(import (only (chezscheme) open-input-file))
(import (srfi :130)) ;; cursor-based string library, TODO: replace with (scheme textual)

(define triplestore
  (let ((engine (nstore-engine okvs-ref okvs-set! okvs-delete! okvs-prefix-range pack unpack)))
    (nstore engine (list 0) '(subject predicate object))))

(define fuzz (fuzzy (list 1)))

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

(define (%concept->word char)
  (if (char=? char #\_)
      #\space
      char))

(define (concept->word concept)
  (list->string (map %concept->word (string->list (list-ref (string-split concept "/") 3)))))

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
                                                  (list-ref value 2)))
                               (let ((start-word (concept->word (list-ref value 1)))
                                     (end-word (concept->word (list-ref value 2))))
                                 (fuzzy-add! transaction
                                             fuzz
                                             start-word)
                                 (fuzzy-add! transaction
                                             fuzz
                                             end-word)
                                 ;; also store mapping between word and
                                 ;; concept to be able to retrieve
                                 ;; concepts given a word or expression.
                                 (nstore-add! transaction triplestore
                                              (list start-word
                                                    "fuzzy"
                                                    (list-ref value 1)))
                                 (nstore-add! transaction triplestore
                                              (list end-word
                                                    "fuzzy"
                                                    (list-ref value 2))))))
        (loop next)))))

(okvs-close database)
