#lang info

(define collection "profile")
(define deps '("base" "errortrace-lib"))
(define build-deps '("at-exp-lib"
                     "rackunit-lib"))

(define pkg-desc "implementation (no documentation) part of \"profile\"")

(define pkg-authors '(eli stamourv))

(define raco-commands
  '(("profile"
     profile/raco
     "profile execution time"
     #f)))

(define version "1.1")

(define license
  '(Apache-2.0 OR MIT))
