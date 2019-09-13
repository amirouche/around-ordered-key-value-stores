;; Copyright © 2019 Amirouche BOUBEKKI <amirouche at hyper dev>
;;
;; guile-wiredtiger is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) or version 3.
;;
;; guile-wiredtiger is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with guile-wiredtiger.  If not, see <http://www.gnu.org/licenses/>
;;
;;; Comment:
;;
;; - 2019/05: ported to chez from guile-wiredtiger
;;
(define-library (cffi wiredtiger okvs)

  (export okvs
          okvs?
          okvs-close
          okvs-debug
          okvs-transaction?
          okvs-transaction-metadata
          okvs-transaction-begin
          okvs-transaction-commit
          okvs-transaction-rollback
          okvs-in-transaction
          okvs-ref
          okvs-set!
          okvs-delete!
          okvs-range
          okvs-prefix)

  (import (scheme base)
          (only (scheme bytevector)
                u8-list->bytevector
                bytevector->u8-list)
          (scheme case-lambda)
          (scheme generator)
          (scheme comparator)
          (scheme hash-table)
          (only (chezscheme) make-mutex with-mutex)
          (only (scheme list) drop take reverse!)
          (prefix (cffi wiredtiger) wt:)
          (pack))

  (begin

    (define-record-type <okvs>
      (make-okvs cnx isolation sessions)
      okvs?
      (cnx okvs-cnx)
      (isolation okvs-isolation)
      (sessions okvs-sessions okvs-sessions!))

    (define-record-type <session>
      (make-session object cursor)
      session?
      (object session-object)
      (cursor session-cursor))

    (define-record-type <okvs-transaction>
      (make-transaction okvs session metadata)
      okvs-transaction?
      (okvs transaction-okvs)
      (session transaction-session)
      (metadata okvs-transaction-metadata))

    (define (string-join lst string)
      (let loop ((lst (cdr lst))
                 (out (list (car lst))))
        (if (null? lst)
            (apply string-append (reverse! out))
            (loop (cdr lst) (append (list (car lst) string) out)))))

    (define (connection-config cache create? memory? wal read-only? eviction-trigger eviction mmap)
      (let ((out '()))
        (when cache
          ;; cache is size number in bytes, convert to MB
          (let ((cache (exact (/ cache 1024))))
            (set! out (cons (string-append "cache_size=" (number->string cache) "MB") out))))
        (when create?
          (set! out (cons "create" out)))
        (when memory?
          (set! out (cons "in_memory" out)))
        (when wal
          (set! out (cons (string-append "log=(enabled=true,file_max="
                                         (number->string (exact (/ wal 1024)))
                                         "MB)")
                          out)))
        (when read-only?
          (set! out (cons "readonly=true" out)))
        (when eviction-trigger
          (set! out (cons (string-append "eviction_trigger="
                                         (number->string eviction-trigger))
                          out)))
        (when eviction
          (let ((min (car (assq eviction 'min)))
                (max (car (assq eviction 'max))))
            (let ((eviction '()))
              (when min
                (set! eviction (cons (string-append "threads_min="
                                                    (number->string min))
                                     eviction)))
              (when max
                (set! eviction (cons (string-append "threads_max="
                                                    (number->string max))
                                     eviction)))
              (set! out (cons (string-append "eviction=("
                                             (string-join eviction ",")
                                             ")")
                              out)))))
        (unless mmap
          (set! out (cons "mmap=false" out)))
        (string-join out ",")))

    (define NO-HOME-ERROR "CONFIG must be an alist with a 'home key that points to an existing directory")

    (define (session-config okvs)
      (if (okvs-isolation okvs)
          (string-append "isolation=\"" (symbol->string (okvs-isolation okvs)) "\"")
          ""))

    ;; TODO: move into okvs record
    (define okvs-mutex (make-mutex))

    (define (%get-or-create-session okvs)
      ;; XXX: caller has the reponsability to close or put back the session
      (if (null? (okvs-sessions okvs))
          ;; new session
          (let ((session (wt:session-open (okvs-cnx okvs) (session-config okvs))))
            (make-session session (wt:cursor-open session "table:okvs" "")))
          ;; re-use free session
          (let ((session (car (okvs-sessions okvs))))
            (okvs-sessions! okvs (cdr (okvs-sessions okvs)))
            session)))

    (define (get-or-create-session okvs)
      (with-mutex okvs-mutex (%get-or-create-session okvs)))

    (define (okvs-config config)
      (let ((home #f)
            (cache #f)
            (isolation #f)
            (create? #f)
            (memory? #f)
            (wal #f)
            (read-only? #f)
            (eviction-trigger #f)
            (eviction #f)
            (mmap #t) ;; XXX: default configuration
            )
        (let loop ((config config))
          (if (null? config)
              (values home cache isolation create? memory? wal read-only? eviction-trigger eviction mmap)
              (begin (case (caar config)
                       ((home) (set! home (cdar config)))
                       ((cache) (set! cache (cdar config)))
                       ((isolation) (set! isolation (cdar config)))
                       ((create?) (set! create? (cdar config)))
                       ((memory?) (set! memory? (cdar config)))
                       ((wal) (set! wal (cdar config)))
                       ((read-only?) (set! read-only? (cdar config)))
                       ((eviction-trigger) (set! eviction-trigger (cdar config)))
                       ((eviction) (set! eviction (cdar config)))
                       ((mmap) (set! mmap (cdar config))))
                     (loop (cdr config)))))))

    (define (%okvs config)
      "CONFIG must be an alist with 'home key that points to an existing
directory"
      (call-with-values (lambda () (okvs-config config))
        (lambda (home cache isolation create? memory? wal read-only? eviction-trigger eviction mmap)
          (unless home
            (error 'wiredtiger NO-HOME-ERROR config))
          (let ((cnx (wt:connection-open home
                                         (connection-config cache
                                                            create?
                                                            memory?
                                                            wal
                                                            read-only?
                                                            eviction-trigger
                                                            eviction
                                                            mmap))))
            (when create?
              (let ((session (wt:session-open cnx "")))
                (wt:session-create session "table:okvs")
                (wt:session-close session "")))
            (make-okvs cnx isolation '())))))

    (define okvs
      (case-lambda
        ((config) (%okvs config))
        (() (%okvs '()))))

    (define okvs-close
      (case-lambda
        ((okvs config) (okvs-close okvs))
        ((okvs) (wt:connection-close (okvs-cnx okvs) ""))))

    (define (okvs-debug okvs proc)
      (let* ((session (get-or-create-session okvs))
             (cursor (session-cursor session)))
        (wt:cursor-reset cursor)
        (let loop ((continue? (wt:cursor-next? cursor)))
          (when continue?
            (proc (wt:cursor-key-ref cursor)
                  (wt:cursor-value-ref cursor))
            (loop (wt:cursor-next? cursor))))
        (okvs-sessions! okvs
                        (cons session
                              (okvs-sessions okvs)))))

    (define default-comparator (make-default-comparator))

    (define (make-metadata)
      (make-hash-table default-comparator))

    (define (okvs-transaction-begin okvs . config)
      (let ((session (get-or-create-session okvs)))
        (wt:session-transaction-begin (session-object session) "")
        (make-transaction okvs session (make-metadata))))

    (define (okvs-transaction-commit transaction . config)
      ;; commit the transaction and put back the session into okvs
      (wt:session-transaction-commit (session-object (transaction-session transaction)) "")
      (wt:session-reset (session-object (transaction-session transaction)))
      (with-mutex okvs-mutex
                  (okvs-sessions! (transaction-okvs transaction)
                                  (cons (transaction-session transaction)
                                        (okvs-sessions (transaction-okvs transaction))))))

    (define (okvs-transaction-rollback transaction . config)
      ;; rollback the transaction and put back the session into okvs
      (wt:session-transaction-rollback (session-object (transaction-session transaction)) "")
      (wt:session-reset (session-object (transaction-session transaction)))
      (with-mutex okvs-mutex
                  (okvs-sessions! (transaction-okvs transaction)
                                  (cons (transaction-session transaction)
                                        (okvs-sessions (transaction-okvs transaction))))))

    (define (%okvs-in-transaction okvs proc failure success)
      (let loop ((index 5)) ;; good enough? TODO: add backoff strategy with sleep
        (when (zero? index)
          (error 'wiredtiger "to many retry"))
        (let ((transaction (okvs-transaction-begin okvs)))
          (guard (ex
                  ((and (pair? ex)
                        (eq? (car ex) 'wiredtiger)
                        (= (cdr ex) -31800)) ;; WT_ROLLBACK
                   (okvs-transaction-rollback transaction)
                   ;; maybe this will work next time?!
                   (loop (- index 1)))
                  (else ;; otherwise, it is an unknown error, no need
                        ;; to retry.
                   (okvs-transaction-rollback transaction)
                   (failure ex)))
            (call-with-values (lambda () (proc transaction))
              (lambda out
                (okvs-transaction-commit transaction)
                (apply success out)))))))

    (define okvs-in-transaction
      (case-lambda
        ((okvs proc) (okvs-in-transaction okvs proc raise values))
        ((okvs proc failure) (okvs-in-transaction okvs proc failure values))
        ((okvs proc failure success) (%okvs-in-transaction okvs proc failure success))))

    (define (okvs-ref transaction key)
      (let ((cursor (session-cursor (transaction-session transaction))))
        (if (not (wt:cursor-search? cursor key))
            (begin (wt:cursor-reset cursor) #f)
            (let ((value (wt:cursor-value-ref cursor)))
              (wt:cursor-reset cursor)
              value))))

    (define (okvs-set! transaction key value)
      (let ((cursor (session-cursor (transaction-session transaction))))
        (wt:cursor-insert cursor key value)))

    (define (okvs-delete! transaction key)
      (let ((cursor (session-cursor (transaction-session transaction))))
        (wt:cursor-remove cursor key)))

    (define (lexicographic-compare bytevector other)
      ;; Return -1 if BYTEVECTOR is before OTHER, 0 if equal
      ;; and otherwise 1
      (let ((end (min (bytevector-length bytevector)
                      (bytevector-length other))))
        (let loop ((index 0))
          (if (zero? (- end index))
              (if (= (bytevector-length bytevector)
                     (bytevector-length other))
                  0
                  (if (< (bytevector-length bytevector)
                         (bytevector-length other))
                      -1
                      1))
              (let ((delta (- (bytevector-u8-ref bytevector index)
                              (bytevector-u8-ref other index))))
                (if (zero? delta)
                    (loop (+ index 1))
                    (if (negative? delta)
                        -1
                        1)))))))

    (define (range-config config)
      (let ((limit #f)
            (reverse? #f)
            (offset #f))
        (let loop ((config config))
          (if (null? config)
              (values limit reverse? offset)
              (begin (case (caar config)
                       ((limit) (set! limit (cdar config)))
                       ((reverse?) (set! reverse? (cdar config)))
                       ((offset) (set! offset (cdar config))))
                     (loop (cdr config)))))))

    (define (%range cursor start-key start-include? end-key end-include? limit offset)
      (let ((continue? #t))
        (lambda ()
          (let loop ()
            (if (not continue?)
                (eof-object)
                (let ((key (wt:cursor-key-ref cursor)))
                  (case (lexicographic-compare key start-key)
                    ((-1) (error 'wiredtiger "Oops!"))
                    ((0) (if start-include?
                             (let ((value (wt:cursor-value-ref cursor)))
                               (set! continue? (wt:cursor-next? cursor))
                               (cons key value))
                             (begin
                               (set! continue? (wt:cursor-next? cursor))
                               (loop))))

                    ((1)
                     (case (lexicographic-compare key end-key)
                       ((-1) (let ((value (wt:cursor-value-ref cursor)))
                               (set! continue? (wt:cursor-next? cursor))
                               (cons key value)))
                       ((0) (if end-include?
                                (let ((value (wt:cursor-value-ref cursor)))
                                  (set! continue? #f)
                                  (cons key value))
                                (eof-object)))
                       ((1)
                        (eof-object)))))))))))

    (define (range transaction cursor start-key start-include? end-key end-include? limit offset)
      (let ((found (wt:cursor-search-near cursor start-key)))
        (cond
         ((not found) eof-object)
         ((eq? found 'before)
          (if (wt:cursor-next? cursor)
              (%range cursor start-key start-include? end-key end-include? limit offset)
              eof-object))
         (else
          (%range cursor start-key start-include? end-key end-include? limit offset)))))

    (define (%range-reverse cursor start-key start-include? end-key end-include? limit offset)
      (let ((continue? #t))
        (lambda ()
          (let loop ()
            (if (not continue?)
                (eof-object)
                (let ((key (wt:cursor-key-ref cursor)))
                  (case (lexicographic-compare key end-key)
                    ((1) (error 'wiredtiger "Oops!"))
                    ((0) (if end-include?
                             (let ((value (wt:cursor-value-ref cursor)))
                               (set! continue? (wt:cursor-prev? cursor))
                               (cons key value))
                             (begin
                               (set! continue? (wt:cursor-prev? cursor))
                               (loop))))
                    ((-1)
                     (case (lexicographic-compare key start-key)
                       ((1) (let ((value (wt:cursor-value-ref cursor)))
                              (set! continue? (wt:cursor-prev? cursor))
                              (cons key value)))
                       ((0) (if end-include?
                                (let ((value (wt:cursor-value-ref cursor)))
                                  (set! continue? #f)
                                  (cons key value))
                                (eof-object)))
                       ((-1)
                        (eof-object)))))))))))

    (define (range-reverse transaction cursor start-key start-include? end-key end-include? limit offset)
      (let ((found (wt:cursor-search-near cursor end-key)))
        (cond
         ((not found) eof-object)
         ((eq? found 'after)
          (if (wt:cursor-prev? cursor)
              (%range-reverse cursor start-key start-include? end-key end-include? limit offset)
              eof-object))
         (else
          (%range-reverse cursor start-key start-include? end-key end-include? limit offset)))))

    (define (generator-force-cursor-close cursor generator)
      (lambda ()
        (let ((out (generator)))
          (if (eof-object? out)
              (begin
                (wt:cursor-close cursor)
                out)
              out))))

    (define (%okvs-range transaction start-key start-include? end-key end-include? config)
      (let ((cursor (wt:cursor-open (session-object (transaction-session transaction)) "table:okvs" "")))
        (call-with-values (lambda () (range-config config))
          (lambda (limit reverse? offset)
            (if reverse?
                (let ((out (range-reverse transaction
                                          cursor
                                          start-key
                                          start-include?
                                          end-key
                                          end-include?
                                          limit
                                          offset)))
                  (when offset
                    (set! out (gdrop out offset)))
                  (when limit
                    (set! out (gtake out limit)))
                  (generator-force-cursor-close cursor out))
                (let ((out (range transaction
                                  cursor
                                  start-key
                                  start-include?
                                  end-key
                                  end-include?
                                  limit
                                  offset)))
                  (when offset
                    (set! out (gdrop out offset)))
                  (when limit
                    (set! out (gtake out limit)))
                  (generator-force-cursor-close cursor out)))))))

    (define okvs-range
      (case-lambda
        ((transaction start-key start-include? end-key end-include?)
         (%okvs-range transaction start-key start-include? end-key end-include? '()))
        ((transaction start-key start-include? end-key end-include? config)
         (%okvs-range transaction start-key start-include? end-key end-include? config))))

    (define (strinc bytevector)
      "Return the first bytevector that is not prefix of BYTEVECTOR"
      ;; See https://git.io/fj34F, TODO: OPTIMIZE
      (let ((bytes (reverse! (bytevector->u8-list bytevector))))
        ;; strip #xFF
        (let loop ((out bytes))
          (when (null? out)
            (error 'wiredtiger "Key must contain at least one byte not equal to #xFF." bytevector))
          (if (= (car out) #xFF)
              (loop (cdr out))
              (set! bytes out)))
        ;; increment first byte, reverse and return the bytevector
        (u8-list->bytevector (reverse! (cons (+ 1 (car bytes)) (cdr bytes))))))

    (define (okvs-prefix transaction prefix . config)
      (apply okvs-range transaction prefix #t (strinc prefix) #f config))

    ))
