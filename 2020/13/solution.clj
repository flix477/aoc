(use '[clojure.string :only (split)])

(defrecord Solution [solution1 solution2])
(defrecord Notes [arrival bus-ids])
(defrecord Bus [id i t])

(defn parse-bus-ids [s]
  (map
    #(if (= "x" %) nil (Integer/parseInt %))
    (split s #",")))

(defn lines->Notes [lines]
  (->Notes
    (Integer/parseInt (first lines))
    (parse-bus-ids (second lines))))

(defn solve1 [notes]
  (apply *
    (apply min-key second
      (map
        #(vector % (- % (rem (:arrival notes) %)))
        (filter #(not= nil %) (:bus-ids notes))))))

(defn inc-t [bus]
  (update bus :t #(+ % (:id bus))))

(defn find-first [pred, xs]
  (first (filter pred xs)))

(defn subsequent-bus? [bs]
  (let [t (:t (first bs))]
    (every? #(= (:t %) (+ (:i %) t)) bs)))

(defn make-subsequent
  ([h, a, b & t]
    (let [ai (first a)
          ax (second a)
          bi (first b)
          bx (second b)]
    (cond
      (= 1 (- bx ax)) (apply recur (cons (concat h [a b]) t))
      (< bx ax) (recur h a [bi (+ bx bi)] t)
      :else (recur h [ai (+ ax ai)] b t)))))

(defn solve2 [notes]
  (filter #(not= (:id %) nil)
    (map-indexed #(->Bus %2 %1 0) (:bus-ids notes))))

(defn Notes->Solution [notes]
  (make-subsequent [2 0] [5 0]))

(println (with-open [rdr (clojure.java.io/reader "input.txt")]
  (Notes->Solution (lines->Notes (line-seq rdr)))))
