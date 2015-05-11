(ns rimizak.core)


(defn stl-solid [s]
	(str "solid Rimizak_Model
		" s "
endsolid Rimizak_Model\n"))

(defn stl-normal [nv]
	(str "normal " (nth nv 0) " " (nth nv 1) " " (nth nv 2)))

(defn stl-facet
  ([facet] (stl-facet (:normal facet) (:vertices facet)))
  ([nv vs]
	(str "facet " (stl-normal nv) "\n"
				"	outer loop\n"
					(apply str (map (fn [v] (str "  vertex " (nth v 0) " " (nth v 1) " " (nth v 2) "\n")) vs))
					"endloop
				endfacet\n")))

(def cube-facets
	[
		{:normal [-0 0 1]
		 :vertices [
		   [-1 1 1]
		   [1 -1 1]
		   [1 1 1]]
		}
		{:normal [0 0 1]
		 :vertices [
		   [1 -1 1]
		   [-1 1 1]
		   [-1 -1 1]]
		}
		{:normal [0 0 -1]
		 :vertices [
		   [-1 -1 -1]
		   [1 1 -1]
		   [1 -1 -1]]
		}
		{:normal [-0 0 -1]
		 :vertices [
		   [1 1 -1]
		   [-1 -1 -1]
		   [-1 1 -1]]
		}
		{:normal [0 -1 0]
		 :vertices [
		   [-1 -1 -1]
		   [1 -1 1]
		   [-1 -1 1]]
		}
		{:normal [0 -1 -0]
		 :vertices [
		   [1 -1 1]
		   [-1 -1 -1]
		   [1 -1 -1]]
		}
		{:normal [1 -0 0]
		 :vertices [
		   [1 -1 1]
		   [1 1 -1]
		   [1 1 1]]
		}
		{:normal [1 0 0]
		 :vertices [
		   [1 1 -1]
		   [1 -1 1]
		   [1 -1 -1]]
		}
		{:normal [0 1 -0]
		 :vertices [
		   [1 1 -1]
		   [-1 1 1]
		   [1 1 1]]
		}
		{:normal [0 1 0]
		 :vertices [
		   [-1 1 1]
		   [1 1 -1]
		   [-1 1 -1]]
		}
		{:normal [-1 0 0]
		 :vertices [
		   [-1 -1 -1]
		   [-1 1 1]
		   [-1 1 -1]]
		}
		{:normal [-1 -0 0]
		 :vertices [
		   [-1 1 1]
		   [-1 -1 -1]
		   [-1 -1 1]]
		}
	])

(defn cube2 [cx cy cz sz]
	(let [sz-half (* 0.5 sz)
				sz-neg-half (- (* 0.5 sz) sz)]
				(stl-solid (apply str (map (fn [v] (stl-facet (:normal v) (:vertices v))) cube-facets)))))

(defn scale-vertex [scale v]
  (map (fn [n] (* scale n)) v))

(defn scale-facets [scale facets]
  (map (fn [facet] {:normal (:normal facet)
                    :vertices (map (partial scale-vertex scale) (:vertices facet))})
		   facets))

(defn translate-vertex [trans-vec v]
  (map (fn [t vc] (+ t vc)) trans-vec v))

(defn translate-facets [trans-vec facets]
  (map (fn [facet] {:normal (:normal facet)
                    :vertices (map (partial translate-vertex trans-vec) (:vertices facet))})
       facets))

(defn translated-solid [trans-vec facets]
  (let [translated-facets (translate-facets trans-vec facets)
        stl-facets (map stl-facet translated-facets)]
    (stl-solid (apply str stl-facets))))

(defn scaled-solid [cx cy cz scale facets]
  (let [scaled-facets (scale-facets scale facets)
        stl-facets (map stl-facet scaled-facets)]
    (stl-solid (apply str stl-facets))))

(defn transformed-solid [trans-fn facets]
  (let [trans-facets (trans-fn facets)
        stl-facets (map stl-facet trans-facets)]
    (stl-solid (apply str stl-facets))))

(defn solid-to-file [solid out-file-name]
		(with-open [w (clojure.java.io/writer out-file-name :append false)]
			(.write w solid)))

(defn facets-to-file [out-file-name facets]
  (let [stl-facets (map stl-facet facets)
        solid (stl-solid (apply str stl-facets))]
    (with-open [w (clojure.java.io/writer out-file-name :append false)]
      (.write w solid))))


;(solid-to-file (scaled-solid 1 2 3 10 cube-facets) "stl-cube-out.stl")
;(solid-to-file (translated-solid [10 10 10] 10 cube-facets) "stl-cube-out3.stl")
;(solid-to-file (transformed-solid (comp (partial translate-facets [10 10 10]) (partial scale-facets 10)) cube-facets) "stl-cube-out4.stl")

(comment
(facets-to-file "stl-cube-out8.stl" (concat
  (->> cube-facets
       (scale-facets 3))
  (->> cube-facets
       (scale-facets 10)
       (translate-facets [5 10 15]))))

(->> cube-facets
     (scale-facets 10)
     (translate-facets [5 10 15])
     (facets-to-file "stl-cube-out7.stl"))
)
