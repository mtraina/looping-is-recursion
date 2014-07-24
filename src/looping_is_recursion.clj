(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [base acc exp]
                 (cond
                  (= 0 exp) 1
                  (= 1 exp) acc
                  :else (recur base (* base acc)(dec exp))))]
    (helper base base exp)))

(defn last-element [a-seq]
  (let [helper (fn [a-seq]
                 (cond
                   (empty? a-seq) (first a-seq)
                   (= (count a-seq) 1) (first a-seq)
                   :else (recur (rest a-seq))))]
    (helper a-seq)))


(defn seq= [seq1 seq2]
  (let [helper (fn [seq1 seq2]
                 (cond
                  (and (empty? seq1)(empty? seq2)) true
                  (not= (count seq1)(count seq2)) false
                  (not= (first seq1)(first seq2)) false
                  :else (recur (rest seq1)(rest seq2))))]

    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [p pred
         the-seq a-seq
         index 0]
    (cond
     (empty? the-seq) nil
     (p (first the-seq)) index
     :else (recur p (rest the-seq) (inc index)))))            ;=> nil

(defn avg [a-seq]
  (loop [the-seq a-seq
         sum 0
         num-elem (count a-seq)]
    (cond
      (empty? the-seq) (/ sum num-elem)
      :else (recur (rest the-seq) (+ sum (first the-seq)) num-elem))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [the-seq a-seq
         the-set #{}]
    (if(empty? the-seq) the-set
      (recur (rest the-seq)(toggle the-set (first the-seq))))))

(defn fast-fibo [n]
;  (loop [m n
;         acc 0
;         m-1 (- n 1)
;         acc-1 0]
;    (cond
;     (= m 0) 0
;     (= m 1) 1
;     :else (recur (dec m)(+ acc acc-1)(dec m-1))))
 )

;(fast-fibo 0) ;=> 0
;(fast-fibo 1) ;=> 1
;(fast-fibo 2) ;=> 1
;(fast-fibo 3) ;=> 2
;(fast-fibo 4) ;=> 3
;(fast-fibo 5) ;=> 5
;(fast-fibo 6) ;=> 8

(defn cut-at-repetition [a-seq]
  [":("])

