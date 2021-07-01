(ns automaton-clj.core
  (:require [clojure.string :as string]))

; 関数としての決定性有限オートマトン
(def characters #{\a \b})
(defn head [[c & _]] (characters c))
(defmulti q1 head)
(defmulti q2 head)
(defmulti q3 head)

(defmethod q1 \a [[_ & tail]] (q2 tail))
(defmethod q1 \b [[_ & tail]] (q1 tail))
(defmethod q1 :default [x] {:rejected :q1 :rest x})

(defmethod q2 \a [[_ & tail]] (q2 tail))
(defmethod q2 \b [[_ & tail]] (q3 tail))
(defmethod q2 :default [x] {:rejected :q2 :rest x})

(defmethod q3 \a [[_ & tail]] (q3 tail))
(defmethod q3 \b [[_ & tail]] (q3 tail))
(defmethod q3 :default [x]
  (if x
    {:rejected :q3 :rest (string/join x)}
    {:accepted :q3}))

; アルゴリズム的な決定性有限オートマトン
(defn dfa [charcters mapping acceptions]
  (let [index (zipmap charcters (range))]
    (fn [state [head & tail]]
      (loop [s state
             h head
             t tail]
        (cond
          (and (mapping s) (index h))
          (recur ((mapping s) (index h))
                 (first t)
                 (rest t))

          (and (nil? h) (acceptions s))
          {:accepted s}

          :else
          {:rejected s
           :rest (string/join (concat h t))})))))

(def automaton
  (dfa
   [\a \b]
   {:q1 [:q2 :q1]
    :q2 [:q2 :q3]
    :q3 [:q3 :q3]}
   #{:q3}))