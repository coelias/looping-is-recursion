(ns looping-is-recursion)

(defn power [base exp]
  (loop [acc 1 xp exp]
               (if (zero? xp)
                 acc
                 (recur (* acc base) (dec xp)))
               ))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2))
      true
   (and (= (first seq1) (first seq2)) (seq seq1) (seq seq2))
      (recur (rest seq1) (rest seq2))
   :else
     false))

(defn find-first-index [pred a-seq]
  (loop [acc 0 b-seq a-seq]
               (cond
                (empty? b-seq)
                nil
                (pred (first b-seq))
                acc
                :else
                (recur (inc acc) (rest b-seq)))))

(defn avg [a-seq]
  (loop [sum 0 n 0 b-seq a-seq]
      (if (empty? b-seq)
        (/ sum n)
        (recur (+ sum (first b-seq)) (inc n) (rest b-seq)))))

(defn parity [a-seq]
  (map first (filter #(pos? (mod (second %) 2)) (frequencies a-seq))))

(defn fast-fibo [n]
  (if (< n 2)
    n
    (loop [f 0 s 1 cur 2]
      (if (= cur n)
        (+ f s)
        (recur s (+ f s) (inc cur))
    ))))

(defn cut-at-repetition [a-seq]
  (loop [s #{} b-seq a-seq c-seq []]
    (if (or (empty? b-seq) (contains? s (first b-seq)))
     c-seq
     (recur (conj s (first b-seq)) (rest b-seq) (conj c-seq (first b-seq))))))

