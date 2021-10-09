#lang info

(define collection 'multi)

(define build-deps '("base"
                     "scribble-lib"
                     "profile-lib"
                     "errortrace-doc"
                     "errortrace-lib"
                     "racket-doc"))
(define update-implies '("profile-lib"))

(define pkg-desc "documentation part of \"profile\"")

(define pkg-authors '(eli))

(define license
  '(Apache-2.0 OR MIT))
