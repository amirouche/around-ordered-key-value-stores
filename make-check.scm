#!/usr/bin/env scheme
(import (only (chezscheme) import)) ;; XXX: required to have 'import' in the env

(import (scheme base))
(import (scheme eval))
(import (scheme process-context))
(import (scheme write))

(import (srfi :130)) ;; cursor-based string library, TODO: replace with (scheme textual)

(import (only (chezscheme) interaction-environment))
(import (only (chezscheme) assert))
(import (only (chezscheme) compile-profile))
(import (only (chezscheme) profile-dump-html))
(import (only (chezscheme) display-condition))

(import (arew tests))

(define (guess-library-name filepath)
  ;; Try to guess the library name based on the FILEPATH.
  ;; e.g. "./src/foo/bar/baz-test.scm" -> (foo bar baz-test)
  (assert (string-suffix? ".scm" filepath))
  ;; (assert (string-suffix? ".test.scm" filepath))
  (cdr (map string->symbol (string-split (substring filepath 2 (- (string-length filepath) 4)) "/"))))

(define (library-exports* filepath library-name)
  ;; return the procedure defined in library LIBRARY-NAME at FILEPATH
  ;; XXX: hackish at best, there might be a better solution
  (let ((env (interaction-environment)))
    (let ((program `(begin
                      (import ,library-name)
                      (let ((exports (library-exports ',library-name)))
                        exports))))
      (let ((exports (eval program env)))
        (let ((program `(begin
                          (import ,library-name)
                          (map cons ',exports (list ,@exports)))))
          (eval program env))))))

(define (run-one pair)
  (display "*** ")
  (display (car pair))
  (newline)
  (guard (x (else (display-condition x) (newline)))
    (let ((result ((cdr pair))))
      (when (failure? result)
        (display "*** expected: ")
        (write (failure-expected result))
        (newline)
        (display "*** actual: ")
        (write (failure-actual result))
        (newline)))))

(define (run filepath)
  ;; run the tests found at FILEPATH
  (display "** running tests found in ")
  (display filepath)
  (newline)
  (let ((library-name (guess-library-name filepath)))
    (let ((tests (library-exports* filepath library-name)))
      (for-each run-one tests))))

(parameterize ([compile-profile 'source])
  (let ((args (cdr (command-line))))
    (if (null? args)
        (begin
          (display "* tests")
          (newline)
          (run "./src/smoke-test.scm")
          ;; (run "./src/arew/data/pack-test.scm")
          (run "./src/cffi/wiredtiger/okvs-test.scm")
          ;; (run "./src/arew/data/base/nstore-test.scm")
          ;; (run "./src/arew/data/base/fstore-test.scm")
          )

        (for-each run args))))

(profile-dump-html "profile/")
