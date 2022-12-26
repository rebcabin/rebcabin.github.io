(define (tak x y z)
  (if (< y x)
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y))
      z))
 
;;; (tak 18 12 6) : haven't yet had the patience to run this to completion.
;;; (tak 12 8 4)  : about 20 secs on PIII-450 under MzScheme running DS interpreter
