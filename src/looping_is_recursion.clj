(ns looping-is-recursion)

(defn power [base exp]
  (loop [acc 1
         b base
         e exp]
   (if (zero? e)
     acc
     (recur (* acc b) b (dec e)))))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (last-element (rest a-seq))))

(defn seq= [seq1 seq2]
  (if (and (= (first seq1) (first seq2))
           (not (= (count (filter empty? [seq1 seq2])) 1)))
    (if (every? empty? [seq1 seq2])
             true
             (seq= (rest seq1) (rest seq2)))
    false))

(defn find-first-index [pred a-seq]
   (loop [index 0
          p pred
          s a-seq]
     (let [elem (first s)]
       (cond
         (= nil elem) nil
         (pred elem) index
         :else (recur (inc index) p (rest s))))))

(defn avg [a-seq]
  (loop [accu 0
         number 0
         s a-seq]
    (let [elem (first s)]
      (if (not elem)
        (/ accu number)
        (recur (+ accu elem) (inc number) (rest s))))))

(defn parity [a-seq]
  (loop [odds #{}
         s a-seq]
    (let [elem (first s)]
      (if (not elem)
        odds
        (recur (if (contains? odds elem)
                 (disj odds elem)
                 (conj odds elem))
               (rest s))))))

(defn fast-fibo [x]
  (if (zero? x)
    0
    (loop [i 1
           value_minus 0
           value 1]
      (if (= i x)
        value
        (recur (inc i) value (+ value_minus value))))))

(defn cut-at-repetition [a-seq]
  (loop [current #{}
         acc []
         s a-seq]
    (let [elem (first s)]
      (cond
      (not elem) (seq acc)
      (contains? current elem) (seq acc)
      :else (recur (conj current elem)
                   (conj acc elem)
                   (rest s))))))
