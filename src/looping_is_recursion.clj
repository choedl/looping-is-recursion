(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [base exp acc]
                 (cond
                   (== 0 exp) 1
                   (== 1 exp) (* acc base)
                   :else      (recur base (dec exp) (* acc base))))]
    (helper base exp 1)))

(defn last-element [a-seq]
  (cond
    (empty? a-seq) 
      nil
    (empty? (rest a-seq)) 
      (first a-seq)
    :else 
      (last-element (rest a-seq))))


(defn seq= [a-seq b-seq]
  (let [do-seq= (fn [a b]
                  (cond
                     (and (empty? a) (empty? b))
                       true
                     (= (first a) (first b))
                       (recur (rest a) (rest b))
                     :else
                      false))]
    (if (== (count a-seq) (count b-seq))
      (do-seq= a-seq b-seq)
      false)))

(defn find-first-index [pred a-seq]
  (loop [i 0
         s a-seq]
    (cond
      (empty? s) nil
      (pred (first s)) i
      :else (recur (inc i) (rest s)))))


(defn avg [a-seq]
  (loop [n 0, sum 0, s a-seq]
    (if (empty? s)
      (/ sum n)
      (recur (inc n) (+ sum (first s)) (rest s)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
        (disj a-set elem)
        (conj a-set elem)))

(defn parity [a-seq]
  (loop [odd-elems #{}, s a-seq]
    (if (empty? s)
      odd-elems
      (recur
        (toggle odd-elems (first s))
        (rest s)))))

(defn fast-fibo[n]
  (if (== n 2)
    1
    (loop [i n, f1 0 f2 1]
      (if (== 0 i)
      f1
      (recur (dec i) f2 (+ f1 f2))))))

(defn cut-at-repetition [a-seq]
  (loop [found-elems #{} s a-seq acc []]
    (if (empty? s)
      acc
      (let [elem (first s)]
        (if (contains? found-elems elem)
          (recur found-elems (rest s) acc)
          (recur (conj found-elems elem) (rest s) (conj acc elem)))))))
