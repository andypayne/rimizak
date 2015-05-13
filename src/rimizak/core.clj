(ns rimizak.core)

(defn cross-product [v1 v2]
	[
		(- (* (nth v1 1) (nth v2 2)) (* (nth v1 2) (nth v2 1)))
		(- (* (nth v1 2) (nth v2 0)) (* (nth v1 0) (nth v2 2)))
		(- (* (nth v1 0) (nth v2 1)) (* (nth v1 1) (nth v2 0)))
	])

(defn normalized-vector [v]
	(let [mag (+ (Math/abs (nth v 0))
							 (Math/abs (nth v 1))
							 (Math/abs (nth v 2)))]
		[
			(/ (nth v 0) mag)
			(/ (nth v 1) mag)
			(/ (nth v 2) mag)
		]
	))

(defn surface-normal [s]
	(normalized-vector (cross-product (nth s 0) (nth s 1))))

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

(def icosahedron-vertices
	(let [t (/ (+ 1.0 (Math/sqrt 5.0)) 2.0)]
		[
			[-1 t 0]
			[1 t 0]
			[-1 (- t) 0]
			[1 (- t) 0]

			[0 -1 t]
			[0 1 t]
			[0 -1 (- t)]
			[0 1 (- t)]

			[t 0 -1]
			[t 0 1]
			[(- t) 0 -1]
			[(- t) 0 1]
		]))

(def icosahedron-facets
	(let [ivs icosahedron-vertices
				indices [
					[ 0 11  5]
					[ 0  5  1]
					[ 0  1  7]
					[ 0  7 10]
					[ 0 10 11]

					[ 1  5  9]
					[ 5 11  4]
					[11 10  2]
					[10  7  6]
					[ 7  1  8]

					[ 3  9  4]
					[ 3  4  2]
					[ 3  2  6]
					[ 3  6  8]
					[ 3  8  9]

					[ 4  9  5]
					[ 2  4 11]
					[ 6  2 10]
					[ 8  6  7]
					[ 9  8  1]
				]
		]
		(map (fn [[x y z]] [(nth ivs x) (nth ivs y) (nth ivs z)]) indices)))

(defn icosahedron-solid []
	(map (fn [facet]
		(stl-facet {:normal (surface-normal facet)
								:vertices facet}))
		icosahedron-facets))


;(clojure.core/refer 'rimizak.core)
;(solid-to-file (stl-solid (apply str (icosahedron-solid))) "stl-ico-out.stl")
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
