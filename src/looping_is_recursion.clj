(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n x]
    (if (zero? x)
      acc
      (recur (* acc n) n (dec x))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [el s]
    (if (empty? s)
      el
      (recur (first s) (rest s))))]
    (helper (first a-seq) (rest a-seq))))

(defn seq= [seq1 seq2]
  (let [helper (fn [acc x y]
                  (cond
                    (and (empty? x) (empty? y)) acc
                    (= (first x) (first y)) (recur acc (rest x) (rest y))
                    :else false))]
    (helper true seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [p? pred
         acc 0
         s a-seq]
    (cond
     (empty? s) nil
     (p? (first s)) acc
     :else (recur p? (inc acc) (rest s)))))

(defn avg [a-seq]
  (loop [acc 0
         x (count a-seq)
         s a-seq]
         (if (empty? s)
          (/ acc x)
          (recur (+ acc (first s)) x (rest s)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [acc #{}
         s a-seq]
          (if (empty? s)
           acc 
           (recur (toggle acc (first s)) (rest s)))))

(defn fast-fibo [n]
  (loop [fbn1 0
         fbn2 1
         c 1]
         (cond 
          (< n 2) n
          (>= c n) fbn2
          :else (recur fbn2 (+ fbn1 fbn2) (inc c)))))

(defn cut-at-repetition [a-seq]
  (loop [acc []
         s a-seq]
         (if (empty? s)
          acc
          (if (contains? (into #{} acc) (first s))
            acc
            (recur (conj acc (first s)) (rest s))))))

