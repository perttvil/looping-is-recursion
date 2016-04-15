(ns looping-is-recursion)

(defn power [base exp]
  (let [
        helper (fn [base exp sum] (cond
                                      (== exp 0) 1
                                      (== exp 1) (* sum base)
                                      :else (recur base (dec exp) (* sum base))
                                      ))
        ]
    (helper base exp 1)
    ))

(defn last-element [a-seq]
  (let [
        helper (fn [a-seq elem] (cond
                                    (empty? a-seq) elem
                                    :else (recur (rest a-seq) (first a-seq))
                                    ))
        ]
    (helper a-seq nil)
    ))

(defn seq= [seq1 seq2]
  (cond (and (empty? seq1) (empty? seq2)) true
        (or (empty? seq1) (empty? seq2)) false
        (not= (count seq1) (count seq2)) false
        (not= (first seq1) (first seq2)) false
        :else (recur (rest seq1) (rest seq2))
        )
  )

(defn find-first-index [pred a-seq]
  (loop [seq1 a-seq
         index 0]
    (cond
      (empty? seq1) nil
      (pred (first seq1)) index
      :else (recur (rest seq1) (inc index))
      )
    )
  )

(defn avg [a-seq]
  (loop [seq1 a-seq
         sum 0
         number 0]
    (cond
      (and (empty? seq1) (zero? number)) nil
      (empty? seq1) (/ sum number)
      :else (recur (rest seq1) (+ sum (first seq1)) (inc number))
      )))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [seq1 a-seq
         x #{}]
    (cond
        (empty? seq1) x
        :else (recur (rest seq1) (toggle x (first seq1)))
      )
    ))

(defn fast-fibo [n]
  (loop [n2 1
         n1 1
         c 3]
    (cond
      (== n 0) 0
      (<= n 2) 1
      (== c n) (+ n2 n1)
      :else (recur n1 (+ n1 n2) (inc c))
      )
    )
  )

(defn cut-at-repetition [a-seq]
  (loop [seq1 a-seq
         a-set #{}
         result '()]
      (cond
        (empty? seq1) (reverse result)
        (contains? a-set (first seq1)) (reverse result)
        :else (recur
                (rest seq1)
                (conj a-set (first seq1))
                (conj result (first seq1))
                )
        )
    )
  )

