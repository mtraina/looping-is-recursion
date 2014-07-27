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
     :else (recur p (rest the-seq) (inc index)))))

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
  (loop [fst 1
           snd 0
           iter 0]
    (cond
     (= n iter) snd
     :else (recur (+ fst snd) fst (inc iter))
 )))

(defn cut-at-repetition [a-seq]
  (loop [the-set #{}
           the-vec []
           the-seq a-seq]
    (cond
     (empty? the-seq) the-vec
     ((complement nil?)(get the-set (first the-seq))) the-vec
     :else (recur (conj the-set (first the-seq))(conj the-vec (first the-seq))(rest the-seq))
    )))
