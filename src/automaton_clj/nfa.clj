(ns automaton-clj.nfa
  (:require [clojure.string :as string]
            [clojure.set :refer [subset?]]))

(defn- free-move [states mapping free-index]
  (loop [ss states]
    (let [succs (flatten (map #((mapping %) free-index) ss))]
      (if (subset? (apply hash-set succs)
                   (apply hash-set ss))
        ss
        (recur (distinct (concat succs ss)))))))

; 非決定性有限オートマトン
(defn nfa [charcters mapping acceptions]
  (let [index (zipmap charcters (range))]
    (fn [state [head & tail]]
      (loop [states [state]
             h head
             t tail]
        (let [ss (free-move states mapping (index :free))]
          (cond
            (and (every? #(mapping %) ss) (index h))
            (recur (-> (map #((mapping %) (index h)) ss)
                       flatten
                       distinct)
                   (first t)
                   (rest t))

            (and (nil? h) (some #(acceptions %) ss))
            {:accepted (filter #(acceptions %) ss)}

            :else
            {:rejected ss
             :rest (string/join (concat [h] t))}))))))

(def automaton
  (nfa
   [\a \b :free]
   {:q1 [[:q1] [:q1 :q2] []]
    :q2 [[:q3] [:q3] []]
    :q3 [[:q4] [:q4] []]
    :q4 [[] [] []]}
   #{:q4}))


(def mapping
  {:q1 [[] [:q2 :q4]]
   :q2 [[:q3] []]
   :q3 [[:q2] []]
   :q4 [[:q5] []]
   :q5 [[:q6] []]
   :q6 [[:q4] []]})

(def free-move-automaton
  (nfa
   [\a :free]
   mapping
   #{:q2 :q4}))