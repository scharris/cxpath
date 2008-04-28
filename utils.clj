
(in-ns 'cxpath)
(clojure/refer 'clojure)

(defn number? [x] (instance? java.lang.Number x))

(defn pass-fail-lists
  "Return a pair [passes fails] of lists, where passes are the items of the argument list that pass the argument
predicate, and fails are those that fail the prediate."
  [pred l]
    (loop [passes nil
           fails  nil
           l l]
      (if l
        (let [hd (first l)]
          (if (pred hd)
            (recur (cons hd passes)
                   fails
                   (rest l))
            (recur passes
                   (cons hd fails)
                   (rest l))))
        [(reverse passes) (reverse fails)])))


(defn or-predicates
  "or-predicates:: [Predicate] -> Predicate
Returns a function that applies the passed predicates successively to its argument, returning the first 
logically true value or false if no predicates produce a logically true value."
  [ps]
    (if (= 1 (count ps)) ; optimize single predicate case
      (first ps) 
      (fn [x]
          (loop [ps (seq ps)]
            (if ps
              (if-let px ((first ps) x)
                px
                (recur (rest ps)))
              false)))))

(defmacro cons-if*
  "If the test is true then evaluate item and return (cons item l), else just l."
  [test item l]
    `(if ~test (cons ~item ~l) ~l))

(defn cons-if
  "If the item is logically true then (cons item l), else l."
  [item l]
    (if item (cons item l) l))

(defn map-mapentries [f m]
  (reduce (fn [res [key val]] (let [[new-key new-val] (f key val)] (assoc res new-key new-val)))
          nil
          m))
