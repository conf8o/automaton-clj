(ns automaton-clj.core)

(def characters #{\a \b})
(defn head [[c & _]] (characters c))
(defmulti one head)
(defmulti two head)
(defmulti three head)

(defmethod one \a [[_ & tail]] (two tail))
(defmethod one \b [[_ & tail]] (one tail))
(defmethod one :default [_] (println "Rejected on one"))

(defmethod two \a [[_ & tail]] (two tail))
(defmethod two \b [[_ & tail]] (three tail))
(defmethod two :default [_] (println "Rejected on two"))

(defmethod three :default [_] (do (println "Accepted") :accepted))