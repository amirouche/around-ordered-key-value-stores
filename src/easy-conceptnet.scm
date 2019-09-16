(import (only (chezscheme) import))

(import (scheme base))
(import (scheme list))
(import (scheme generator))
(import (scheme hash-table))
(import (scheme mapping hash))
(import (scheme process-context))
(import (scheme write))

(import (arew http server))
(import (arew untangle))

(import (nstore))
(import (pack))
(import (cffi wiredtiger okvs))

(import (matchable))

(import (only (chezscheme) open-input-file))
(import (srfi :130)) ;; cursor-based string library, TODO: replace with (scheme textual)


;; TODO: configure eviction and cache
(define database (okvs-open '((home . "wt") (create? . #t))))

(define triplestore
  (let ((engine (nstore-engine okvs-ref okvs-set! okvs-delete! okvs-prefix-range pack unpack)))
    (nstore engine (list 0) '(subject predicate object))))

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

(define (index filepath)
  (let loop ((next (stream-relations filepath)))
    (call-with-values next
      (lambda (value next)
        (when value
          ;; XXX: only for debugging, watch "wt/" directory to know
          ;; whether the index command make progress or not (display
          ;; ".")
          (okvs-in-transaction database
                               (lambda (transaction)
                                 (nstore-add! transaction triplestore
                                              (list (list-ref value 1)
                                                    (list-ref value 0)
                                                    (list-ref value 2)))))
          (loop next))))))

(define (bytes->string lst)
  (list->string (map integer->char lst)))

(define (read-request-line port)
  (let loop ((char (read-u8 port))
             (out '()))
    (cond
     ((eof-object? char) (bytes->string (reverse out)))
     ((= char (char->integer #\return)) (read-u8 port) (bytes->string (reverse out)))
     (else (loop (read-u8 port) (cons char out))))))


(define text/plain (make-header-hash-table))
(hash-table-set! text/plain 'Content-Type "text/plain")

(define hello "
 .-----.---.-.-----.--.--.
 |  -__|  _  |__ --|  |  |
 |_____|___._|_____|___  |          __               __
 .----.-----.-----.----.-----.-----|  |_.-----.-----|  |_
 |  __|  _  |     |  __|  -__|  _  |   _|     |  -__|   _|
 |____|_____|__|__|____|_____|   __|____|__|__|_____|____|
                             |__|

You know for conceptnet...

www: https://github.com/amirouche/easy-conceptnet/

version: 0.0.0

")

(define (route/lookup-concept lang label)
  (let ((concept (string-append "/c/" lang "/" label)))
    (okvs-in-transaction database
      (lambda (transaction)
        (let ((generator (nstore-from transaction triplestore
                                      (list concept
                                            (nstore-var 'relation)
                                            (nstore-var 'target)))))
          (let loop ((binding (generator))
                     (out "source\trelation\ttarget\n"))
            (if (eof-object? binding)
                (values 200 text/plain out)
                (loop (generator)
                      (string-append out
                                     concept
                                     "\t"
                                     (hashmap-ref binding 'relation)
                                     "\t"
                                     (hashmap-ref binding 'target)
                                     "\n")))))))))

(define (handler request port)
  (let* ((line (read-request-line port))
         (path (drop (string-split (list-ref (string-split line " ") 1) "/") 1)))
    (match path
      (("") (values 200 text/plain "Hello, world!"))
      (("api") (values 200 text/plain hello))
      (("api" "lookup" "c" lang label) (route/lookup-concept lang label)))))

(define (serve port)
  (untangle (lambda ()
              (run-server "127.0.0.1" port handler))))

(define command (cadr (command-line)))

(cond
 ((string=? command "index")
  (index (caddr (command-line))))
 ((string=? command "serve")
  (serve (string->number (caddr (command-line)))))
 (else (error 'easy-conceptnet "unknown command")))




(okvs-close database)
