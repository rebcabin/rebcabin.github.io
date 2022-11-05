(ns curried-python.core
  (:gen-class))

#_(defn self-apply-2 [g]
  (g g))

#_(defn yc2 [d]
  (defn lsf [sf]
    (defn dmn [m]
      (defn dn [n]
        (((sf sf) m) n))))
  (self-apply-2 lsf))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def yc
  (fn [d]
    ((fn [g] (g g))
     (fn [sf]
       (d (fn [n]
            ((sf sf) n)))))))

(def dfact
  (fn [f]
    (fn [n]
      (if (< n 1)
        1
        (* n (f (- n 1)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def yc2
  (fn [d]
    ((fn [g] (g g))
     (fn [sf]
       (d (fn [m] ;; m is of type MEMO = [ASSOC, Integer]
            (fn [n]
              (((sf sf) m) n))))))))

(def dafib
  (fn [f]
    (fn [a]   ;; an ASSOC :: int -> int
      (fn [n] ;; :: int
        (if (< n 2N)
          [a, 1N] ;; :: MEMO = [ASSOC, Integer]
          (let [n-1 (- n 1N)]
            (if (a n-1)    ;; membership check
              [a, (a n-1)] ;; early return
              (let [fim1 (f a)
                    m1 (fim1 n-1)
                    r1 (m1 1) ;; pick element 1 of m1
                    a1 (merge (m1 0) {n-1 r1})
                    n-2 (- n 2)]
                (if (a1 n-2)            ;; membership check
                  [a1, (+ r1 (a1 n-2))] ;; early return
                  (let [fim2 (f a1)
                        m2 (fim2 n-2)
                        r2 (m2 1) ;; pick elt. 1 of m2
                        a2 (merge (m2 0) {n-2 r2})]
                    [a2, (+ r1 r2)]))))))))))

(def dafib
  (fn [f]
    (fn [a]   ;; an ASSOC :: int -> int
      (fn [n] ;; :: int
        (if (< n 2N)
          [a, 1N] ;; :: MEMO = [ASSOC, Integer]
          ((fn [n-1]
             (if (a n-1)   ;; membership check
               [a, (a n-1)] ;; early return
               ((fn [fim1]
                  ((fn [m1]
                     ((fn [r1]
                        ((fn [a1]
                           ((fn [n-2]
                              (if (a1 n-2)           ;; membership check
                                [a1, (+ r1 (a1 n-2))] ;; early return
                                ((fn [fim2]
                                   ((fn [m2]
                                      ((fn [r2]
                                         ((fn [a2]
                                            [a2, (+ r1 r2)])
                                          (merge (m2 0) {n-2 r2})))
                                       (m2 1))) ;; pick elt. 1 of m2
                                    (fim2 n-2)))
                                 (f a1))))
                            (- n 2)))
                         (merge (m1 0) {n-1 r1})))
                      (m1 1)) )
                   (fim1 n-1)))
                (f a))))
           (- n 1N)))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (merge {1 "a", 2 "b"} {3, "c"})
  ((yc dfact) 6)
  ((((yc2 dafib) {}) 50N) 1)
  ([1 2] 0)
  (((((fn [d]
        ((fn [g] (g g))
         (fn [sf]
           (d (fn [m] ;; m is of type MEMO = [ASSOC, Integer]
                (fn [n]
                  (((sf sf) m) n)))))))
      (fn [f]
        (fn [a]   ;; an ASSOC :: int -> int
          (fn [n] ;; :: int
            (if (< n 2N)
              [a, 1N] ;; :: MEMO = [ASSOC, Integer]
              ((fn [n-1]
                 (if (a n-1)    ;; membership check
                   [a, (a n-1)] ;; early return
                   ((fn [fim1]
                      ((fn [m1]
                         ((fn [r1]
                            ((fn [a1]
                               ((fn [n-2]
                                  (if (a1 n-2)            ;; membership check
                                    [a1, (+ r1 (a1 n-2))] ;; early return
                                    ((fn [fim2]
                                       ((fn [m2]
                                          ((fn [r2]
                                             ((fn [a2]
                                                [a2, (+ r1 r2)])
                                              (merge (m2 0) {n-2 r2})))
                                           (m2 1))) ;; pick elt. 1 of m2
                                        (fim2 n-2)))
                                     (f a1))))
                                (- n 2)))
                             (merge (m1 0) {n-1 r1})))
                          (m1 1))) ;; pick elt. 1 of m1
                       (fim1 n-1)))
                    (f a))))
               (- n 1N))))))) {}) 50N) 1)

  (println "Hello, World!"))
