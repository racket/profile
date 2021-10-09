#lang info

(define collection 'multi)

(define deps '("profile-lib"
               "profile-doc"))
(define implies '("profile-lib"
                  "profile-doc"))

(define pkg-desc "Libraries for statistical performance profiling")

(define pkg-authors '(eli))

(define license
  '(Apache-2.0 OR MIT))
