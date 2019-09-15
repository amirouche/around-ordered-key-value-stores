;; Copyright © 2019 Amirouche BOUBEKKI <amirouche at hyper dev>
;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.
(define-library (fuzzy)

  (export fuzzy fuzzy? fuzzy-add! fuzzy-search)

  (import (only (srfi :145) assume))
  (import (scheme base))
  (import (scheme list))
  (import (scheme comparator))
  (import (scheme set))

  ;; cursor-based string library, TODO: replace with (scheme textual)
  (import (only (srfi :130) string-split))

  (import (pack))
  (import (cffi wiredtiger okvs))

  (begin

    (define-record-type <fuzzy>
      (fuzzy prefix)
      fuzzy?
      (prefix fuzzy-prefix))

    (define (%trigram token)
      (let ((indices (iota (- (string-length token) 2))))
        (map (lambda (index) (substring token index (+ index 3))) indices)))

    (define (trigram string)
      (let* ((tokens (string-split string " "))
             (tokens (map (lambda (x) (string-append "$" x "$")) tokens)))
        (append-map %trigram tokens)))

    (define (fuzzy-add! transaction fuzzy string)
      (let loop ((trigrams (trigram string)))
        (unless (null? trigrams)
          (okvs-set! transaction
                     (apply pack
                            (append (fuzzy-prefix fuzzy)
                                    (list (car trigrams)
                                          string)))
                     #vu8(0))
          (loop (cdr trigrams)))))

    (define (fuzzy-bag transaction fuzzy string)
      (let loop1 ((trigrams (trigram string))
                  (out (bag (make-default-comparator))))
        (if (null? trigrams)
            (bag->alist out)
            (let ((generator (okvs-prefix-range transaction
                                                (apply pack
                                                       (append (fuzzy-prefix fuzzy)
                                                               (list (car trigrams)))))))
              (let loop2 ((value (generator))
                          (out out))
                (if (eof-object? value)
                    (loop1 (cdr trigrams) out)
                    (loop2 (generator) (bag-adjoin! out (list-ref (pk (unpack (car value))) 2)))))))))

    (define (levenshtein s t)
      (define (%levenshtein s sl t tl)
        (cond ((zero? sl) tl)
              ((zero? tl) sl)
              (else
	       (min (+ (%levenshtein (cdr s) (- sl 1) t tl) 1)
                    (+ (%levenshtein s sl (cdr t) (- tl 1)) 1)
                    (+ (%levenshtein (cdr s) (- sl 1) (cdr t) (- tl 1))
		       (if (char=? (car s) (car t)) 0 1))))))
      (%levenshtein (string->list s)
		    (string-length s)
		    (string->list t)
		    (string-length t)))

    (define (fuzzy-search transaction fuzzy string)
      (let* ((out (fuzzy-bag transaction fuzzy string))
             (out (map (lambda (x) (cons (car x) (- (cdr x) (levenshtein string (car x)))))
                       out)))
        (sort (lambda (x y) (> (cdr x) (cdr y))) out)))

    ))
