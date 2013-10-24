(ns looping-is-recursion)

(defn power [base exp]
  "Computes the mathematical expression base to the power of exp."
  (loop [value 1
         n exp]
    (if (zero? n)
      value
      (recur (* value base) (dec n)))))

(defn last-element [a-seq]
  "Returns the last element in a sequence."
  (loop [elem a-seq]
    (if (empty? (rest elem))
      (first elem)
      (recur (rest elem)))))

(defn seq= [seq1 seq2]
  "Compare sequences for equality."
  (loop [s1 seq1
         s2 seq2]
    (cond
     (and (empty? s1)
          (empty? s2))    true  ;; Both are empty: OK
     (or (empty? s1)
         (empty? s2))     false ;; Only one is empty: Not ok!
     (not (= (first s1)
             (first s2))) false ;; Firsts are not equal: Not ok!
     :else (recur (rest s1) (rest s2)))))

(defn find-first-index [pred a-seq]
  "Finds the first element of a sequence which returns true on predicate."
  (loop [elem a-seq
         i 0]
   (cond
    (empty? elem) nil
    (pred (first elem)) i
    :else (recur (rest elem) (inc i)))))

(find-first-index zero? [1 3 4 0 4 2])

(defn avg [a-seq]
  "Returns the average of elements in a sequence."
  (if (empty? a-seq)
    nil
    (loop [elem a-seq
           sum 0
           i 0]
      (cond
       (empty? elem) (/ sum i)
       :else (recur (rest elem)
                    (+ sum (first elem))
                    (inc i))))))

(defn toggle [a-set elem]
  "removes elem from a-set if a-set contains elem, and adds it to the set otherwise."
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(toggle #{} 1)

(defn parity [a-seq]
  "Takes in a sequence and returns a set of those elements that occur
  an odd number of times in the sequence."
  (loop [elem a-seq
         a-set #{}]
    (cond
     (empty? elem) a-set
     :else (recur (rest elem)
                  (toggle a-set (first elem))))))

(parity '(1 2 3 4 4 5 5))

(defn fast-fibo [n]
  "Calculates the nth fibonacci number FAST."
  (loop [f-2 0
         f-1 1
         i n]
    (cond
     (= i 0) f-2
     :else (recur f-1
                  (+ f-1 f-2)
                  (dec i)))))

(defn cut-at-repetition [a-seq]
  "Returns the beginning of a sequence until first duplicate."
  (loop [elem a-seq
         a-set #{}
         i 0]
    (cond
     (empty? elem)                  (take i a-seq)
     (contains? a-set (first elem)) (take i a-seq)
     :else                          (recur (rest elem)
                                           (conj a-set (first elem))
                                           (inc i)))))
