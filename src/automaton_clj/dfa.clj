(ns automaton-clj.dfa
  (:require [clojure.string :as string]))

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
           :rest (string/join (concat [h] t))})))))

(def automaton
  (dfa
   [\a \b]
   {:q1 [:q2 :q1]
    :q2 [:q2 :q3]
    :q3 [:q3 :q3]}
   #{:q3}))