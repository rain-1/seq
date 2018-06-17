#!r6rs

;; Author: Raymond Nicholson <rain1@airmail.cc>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.
;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

(library (rain-1 seq)
  (export
    elt?
    elt-get-elt
    cat?
    cat-get-seqs
    seq->dlist
    seq->list
    seq-length)
  (import (rnrs))

(define (fold kons knil lst)
  (if (null? lst)
      knil
      (kons (car lst)
	    (fold kons knil (cdr lst)))))

(define (elt? exp)
  (and (pair? exp)
       (eq? 'elt (car exp))
       (pair? (cdr exp))
       (null? (cddr exp))))
(define (elt-get-elt exp) (cadr exp))
(define (cat? exp) (and (pair? exp) (eq? 'cat (car exp))))
(define (cat-get-seqs exp) (cdr exp))

(define (seq->dlist seq tail)
  (cond ((elt? seq) (cons (elt-get-elt seq) tail))
	((cat? seq) (fold seq->dlist tail (cat-get-seqs seq)))
	(else (error 'seq->dlist "?" seq))))
(define (seq->list seq) (seq->dlist seq '()))

(define (seq-length^ seq tail)
  (cond ((elt? seq) (+ 1 tail))
	((cat? seq) (fold seq-length^ tail (cat-get-seqs seq)))
	(else (error 'seq-length^ "?" seq))))
(define (seq-length seq) (seq-length^ seq 0))

)
