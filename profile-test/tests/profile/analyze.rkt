#lang racket/base

(require tests/eli-tester profile/structs profile/analyzer
         racket/match racket/list)

(define A '(A . #f))
(define B '(B . #f))
(define C '(C . #f))

(define (analyze cpu+lists)
  (profile->sexpr
   (analyze-samples
    (cons (car cpu+lists)
          (map (lambda (x) (append (take x 2) (reverse (drop x 2))))
               (reverse (cdr cpu+lists)))))))

(define (profile->sexpr prof)
  (define (node-id* node)
    (or (node-id node) (if (node-src node) '??? '*)))
  (define (edges->sexprs node get get-time)
    (for/list ([edge (get node)])
      `(,(node-id* (edge-caller edge)) -> ,(node-id* (edge-callee edge))
        time=  ,(get-time edge)
        total= ,(edge-total edge))))
  (define (node->sexpr node)
    `(,(node-id* node)
          total=   ,(node-total node)
          self=    ,(node-self node)
          callers: ,@(edges->sexprs node node-callers edge-caller-time)
          callees: ,@(edges->sexprs node node-callees edge-callee-time)
          threads= ,(node-thread-ids node)))
  `(total=   ,(profile-total-time    prof)
    samples= ,(profile-sample-number prof)
    cpu=     ,(profile-cpu-time      prof)
    thread-times= ,(profile-thread-times prof)
    ,@(map node->sexpr (cons (profile-*-node prof) (profile-nodes prof)))))

(provide analyze-tests)
(module+ main (analyze-tests))
(define (analyze-tests)
  (test

   (match (analyze `(10
                     [0 0 ,A]
                     [0 1 ,A]))
     [`(total= 2 samples= 2 cpu= 10 thread-times= ([0 . 2])
        [* total= 2 self= 0
           callers: [A -> * time= 2 total= 2]
           callees: [* -> A time= 2 total= 2]
           threads= ()]
        [A total= 2 self= 2
           callers: [* -> A time= 2 total= 2]
           callees: [A -> * time= 2 total= 2]
           threads= (0)])
      'ok]
     [bad (error 'test ">>> ~s" bad)])

   ;; demonstrates different edge-caller/lee-times
   (match (analyze `(10
                     [0 0 ,A ,B ,A ,B]
                     [0 1 ,A ,B ,A]))
     [`(total= 2 samples= 2 cpu= 10 thread-times= ([0 . 2])
        [* total= 2 self= 0
           callers: [A -> * time= 1 total= 1]
                    [B -> * time= 1 total= 1]
           callees: [* -> A time= 2 total= 2]
           threads= ()]
        [A total= 2 self= 1
           callers: [B -> A time= 2/2 total= 2]
                    [* -> A time= 2/2 total= 2]
           callees: [A -> B time= 3/2 total= 2]
                    [A -> * time= 1/2 total= 1]
           threads= (0)]
        [B total= 2 self= 1
           callers: [A -> B time= 2 total= 2]
           callees: [B -> A time= 3/2 total= 2]
                    [B -> * time= 1/2 total= 1]
           threads= (0)])
      'ok]
     [bad (error 'test ">>> ~s" bad)])

   (match (analyze `(10
                           [0 0 ,A ,B ,A ,B]
                           [0 1 ,A ,C ,A ,C]
                           [0 2 ,A ,C ,A ,C]
                           [0 3 ,A ,C ,A]))
     [`(total= 4 samples= 4 cpu= 10 thread-times= ([0 . 4])
        [* total= 4 self= 0
           callers: [A -> * time= 1 total= 1]
                    [C -> * time= 2 total= 2]
                    [B -> * time= 1 total= 1]
           callees: [* -> A time= 4 total= 4]
           threads= ()]
        [A total= 4 self= 1
           callers: [* -> A time= 4/2 total= 4]
                    [C -> A time= 3/2 total= 3]
                    [B -> A time= 1/2 total= 1]
           callees: [A -> C time= 5/2 total= 3]
                    [A -> B time= 2/2 total= 1]
                    [A -> * time= 1/2 total= 1]
           threads= (0)]
        [C total= 3 self= 2
           callers: [A -> C time= 3 total= 3]
           callees: [C -> A time= 2 total= 3]
                    [C -> * time= 1 total= 2]
           threads= (0)]
        [B total= 1 self= 1
           callers: [A -> B time= 1 total= 1]
           callees: [B -> A time= 1/2 total= 1]
                    [B -> * time= 1/2 total= 1]
           threads= (0)])
      'ok]
     [bad (error 'test ">>> ~s" bad)])

   ))

(provide merge-tests)
(module+ main (merge-tests))
(define (merge-tests)
  (define (workload n) (if (= n 0) '(0 1) (cartesian-product (workload (- n 1)) '(0 1))))
  (test
   (define p1 (let/ec ec (profile-thunk (lambda () (workload 20)) #:render (lambda (a b) (ec a)))))
   (define p2 (let/ec ec (profile-thunk (lambda () (workload 20)) #:render (lambda (a b) (ec a)))))
   (define pm (profile-merge p1 p2))
   (unless (= (profile-total-time pm) (+ (profile-total-time p1) (profile-total-time p2)))
     (error 'test "Total time does not match"))
   (unless (= (profile-cpu-time pm) (+ (profile-cpu-time p1) (profile-cpu-time p2)))
     (error 'test "Total time does not match"))))
