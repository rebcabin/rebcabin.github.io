@; scheme-r5rs

;; (define-syntax bugger-or
;;   (syntax-rules ()
;;     [(_) #f]
;;     [(_ e) e]
;;     [(_ e1 e2 e3 ...)
;;      (let ([t e1]) (if t t (or e2 e3 ...)))]))

;; (define-syntax echo
;;   (syntax-rules ()
;;     [(_ e) (begin (print e)
;;                   (newline)
;;                   e)]))


(define (separator banner)
  (display "----------------------------------------------------------------")
  (newline)
  (display banner)
  (newline))

;;  ___ ___ _  _  ___
;; | __/ __| || |/ _ \
;; | _| (__| __ | (_) |
;; |___\___|_||_|\___/
(separator "ECHO")
(define (echo str expr)
  (pp str)
  (newline)
  expr)

(define lambda-test (lambda () 42))
(pp (lambda-test))

;;; Test environment inspection in the Gambit debugger.
;;; https://gambitscheme.org/latest/manual/#index-_002ddebug_002denvironments

;;; Uncomment the following, load the file, and type ",e" in the
;;; REPL to see the binding environment. The chain is not
;;; explicitly revealed. Type ",b" to print the continuation frames.

;; (let ((x 42))
;;   (let ((y (- 1 1)))
;;     (* (/ x y) 2)))

(define (fact n)
  (if (< n 2)
      1
      (* n (fact (- n 1)))))

;;; After loading this file, type "(trace fact)<ENTER>(fact 5)<ENTER>" in the
;;; REPL.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(pp ((lambda (m)
       ((lambda (n) (* n n))
        m))
     42))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(pp ((lambda (m)
       ((lambda (n) (* n m)) ; DIFFERENT!
        m))
     42))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(pp
 (((lambda (m)
     (lambda (n) (* n m))) 42) 42))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define foo
  (lambda (m)
    (lambda (n) (* n m))))

(pp foo)
(pp (foo 42))
(pp ((foo 42) 42))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define foo
  (lambda (n) (* n m)))

(with-exception-handler
    pp
  (lambda () (pp (foo 43))))

(with-exception-handler
    pp
  (lambda () (pp ((foo 43) 42))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define foo
  (lambda (n)
    (lambda (n) (* n n))))

(pp foo)
(pp (foo 43))
(pp ((foo 43) 42))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(((lambda (f) f) square) 42)

(((lambda (f) f) (lambda (x) (* x x))) 42)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  ___  ___  _   _  _   ___ ___   ___  ___   ___ _____ ___
;; / __|/ _ \| | | |/_\ | _ \ __| | _ \/ _ \ / _ \_   _/ __|
;; \__ \ (_) | |_| / _ \|   / _|  |   / (_) | (_) || | \__ \
;; |___/\__\_\\___/_/ \_\_|_\___| |_|_\\___/ \___/ |_| |___/
(separator "SQUARE ROOTS")

(print 'A)
(display ": Square a named square root.")
(newline)
(define foo
  (lambda (sf)
    (lambda (n)
      (if (< n 1)
          1
          (* n ((sf sf) (- n 1)))))))
(pp ((foo foo) 6))

(print 'B)
(display ": Square an anonymous square root.")
(newline)
(pp (((lambda (sf)
        (lambda (n)
          (if (< n 1)
              1
              (* n ((sf sf) (- n 1))))))
      (lambda (sf)
        (lambda (n)
          (if (< n 1)
              1
              (* n ((sf sf) (- n 1)))))))
     6))

(print 'C)
(display ": Abstract business code f into
delayed application of the square.")
(newline)
(pp (((lambda (sf)
        ((lambda (f)
           (lambda (n)
             (if (< n 1)
                 1
                 (* n (f (- n 1))))))
         (lambda (m) ((sf sf) m))))
      (lambda (sf)
        ((lambda (f)
           (lambda (n)
             (if (< n 1)
                 1
                 (* n (f (- n 1))))))
         (lambda (m) ((sf sf) m)))))
     6))

(print 'D)
(display ": Abstract the squaring into function of domain code d.")
(newline)
(pp (((lambda (sf)
        ((lambda (d)
           (d (lambda (m) ((sf sf) m))))
         (lambda (f)
           (lambda (n)
             (if (< n 1)
                 1
                 (* n (f (- n 1))))))))
      (lambda (sf)
        ((lambda (d)
           (d (lambda (m) ((sf sf) m))))
         (lambda (f)
           (lambda (n)
             (if (< n 1)
                 1
                 (* n (f (- n 1)))))))))
     6))

(print 'E)
(display ": Involute function of domain code d and squaring.")
(newline)
(pp (((lambda (d)
        ((lambda (sf)
           (d (lambda (m) ((sf sf) m))))
         (lambda (sf)
           (d (lambda (m) ((sf sf) m))))))
      (lambda (f)
        (lambda (n)
          (if (< n 1)
              1
              (* n (f (- n 1)))))))
     6))

(print 'F)
(display ": Abstract second squaring into function of g.")
(newline)
(pp (((lambda (d)
        ((lambda (g) (g g))
         (lambda (sf)
           (d (lambda (m) ((sf sf) m))))))
      (lambda (f)
        (lambda (n)
          (if (< n 1)
              1
              (* n (f (- n 1)))))))
     6))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  _   _ ___  ___ ___ _    ___  _  _ ___
;; | | | | _ \/ __|_ _| |  / _ \| \| / __|
;; | |_| |  _/\__ \| || |_| (_) | .` \__ \
;;  \___/|_|  |___/___|____\___/|_|\_|___/
(separator "UPSILONS")

(define (fact-recursive f)
  "domain code; Receives from Y or LOOP a recursive version of
itself as parameter f. Domain code must return a business code, b,
of 1 parameter that closes over f as a free variable and may
invoke f, also a function of 1 business parameter."
  (lambda (n) (if (< n 1)
                  1
                  (* n (f (- n 1))))))

(define (Y1 d)
  "d is domain code, a function that receives business code as a
parameter f. Business code, b, is a function of 1 business
parameter, m. Business code may refer to itself through the
parameter f of d, a free variable in the body of b."
  ((lambda (g) (g g))
   (lambda (sf) (d (lambda (m) ((sf sf) m))))))

(display "Y1 on fact-recursive") (newline)
(pp ((Y1 fact-recursive) 6))

(define (fact-iter f)
  "domain code; Receives from Y or LOOP a recursive version of
itself as parameter f. Domain code must return a business code, b,
of 3 parameters that closes over f as a free variable and may
invoke f, also a function of 3 business parameters."
  (lambda (m c x)
    (if (> c x)
        m
        (f (* m c) (+ c 1) x))))

(define (Y3 d)
  "d is domain code, a function that receives business code as a
parameter f. Business code, b, is a function of 3 business
parameters, m, c, x. Business code may refer to itself through the
parameter f of d, a free variable in the body of b."
  ((lambda (g) (g g))
   (lambda (sf) (d (lambda (m c x)
                     ((sf sf) m c x))))))

(display "Y3 on fact-iter") (newline)
(pp ((Y3 fact-iter) 1 1 6))

(define (YN d)
  ((lambda (g) (g g))
   (lambda (sf) (d (lambda L
                     (apply (sf sf) L))))))

(display "YN on fact-iter") (newline)
(pp ((YN fact-iter) 1 1 6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  _    ___   ___  ___  ___
;; | |  / _ \ / _ \| _ \/ __|
;; | |_| (_) | (_) |  _/\__ \
;; |____\___/ \___/|_|  |___/
(separator "LOOPS")

(define (LOOP3 d)
  ;; P3 calls looper with new args:
  (define (P3 m+ c+ x+)
    (raise (list m+ c+ x+)))
  (define (looper m c x)
    (with-exception-handler
        ;; This is how P3 calls looper, indirectly:
        (lambda (e)
          (apply looper e))
        ;; Pass P3 to domain code. If domain code calls P3,
        ;; looper recurses, otherwise returns result below.
        (lambda ()  ; With-exception-handler requires thunk.
          ((d P3) m c x))))
  looper)

(display "exception-based LOOP3 on 3-arg print") (newline)
(pp ((LOOP3 (lambda (f)
              (lambda (m c x)
                (pp (list m c x)))))
     1 2 3))

(display "exception-based LOOP3 on fact-iter") (newline)
(pp ((LOOP3 fact-iter) 1 1 6))

;;; If d calls P3, then loop; else return ((d P3) m c x).
(define (LOOP3 d)
  (define (P3  m c x)
    (call/cc (lambda (k)
               (k ((d P3) m c x)))))
  P3)

(display "call/cc-based LOOP3 on fact-iter") (newline)
(pp ((LOOP3 fact-iter) 1 1 6))

(define (LOOPN d)
  (define (PN . L)
    (call/cc (lambda (k)
               (k (apply (d PN) L)))))
  PN)

(display "call/cc-based LOOPN on fact-iter") (newline)
(pp ((LOOPN fact-iter) 1 1 6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  _____ _   _  __
;; |_   _/_\ | |/ /
;;   | |/ _ \| ' <
;;   |_/_/ \_\_|\_\
(separator "TAK")

(define (itak c x y z)
  (let ((itakx (lambda (c) (itak c (- x 1) y z)))
        (itaky (lambda (c) (itak c (- y 1) z x)))
        (itakz (lambda (c) (itak c (- z 1) x y))))
    (if (< y x)
     (let* ((lx (itakx (+ c 1)))
            (ly (itaky (+ (car lx) 1)))
            (lz (itakz (+ (car ly) 1))))
       (itak (+ (car lz) 1) (cadr lx) (cadr ly) (cadr lz)))
     (list c z))))

(pp (itak 1  3  2  1))  ; (5 2)
(pp (itak 1 12  8  4))  ; (1733 5)
(pp (itak 1 18 12  6))  ; (63609 7)
;; (pp (itak 1 28 20 12))  ; (2493349 13)
