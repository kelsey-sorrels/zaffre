(ns zaffre.text
  (:require clojure.string
            [taoensso.timbre :as log]))

;; like split-at, but takes a sequence of n and splits into lengths of n
(defn split-n [n-seq l]
  (reduce (fn [l n]
            (concat (butlast l) (split-at n (last l))))
          [l]
          n-seq))

;; like split-at, but takes a sequence of n and splits into lengths of n
(defn split-string-n [n-seq s]
  (reduce (fn [l n]
            (concat (butlast l)
                    (map (partial apply str) (split-at n (last l)))))
          [s]
          n-seq))

;; Adapted from https://www.rosettacode.org/wiki/Word_wrap#Clojure
;; About 3.8x faster than the naive version
(defn word-wrap [size text]
  (if text
	(let [size (max (int size) 1)]
      (if (<= (count text) size)
        [text]
		(re-seq (re-pattern (str ".{1," size "}\\s|.{1," size "}"))
				(clojure.string/replace text #"\n" " "))))
    []))

;; Cascade style into descendants
(defn cascade-style [parent-style element]
  (if (string? element)
    ;; return a text element here.
    ;; this has the effect of not having mixed (string + text) children
    ;; by puthing strings one level deeper
    [:text parent-style element]
    (let [[type {:keys [style]} children] element
          new-style (merge parent-style style)]
      [type
       new-style
       (map (partial cascade-style new-style) children)])))

;; Transform :text tree into a sequence by walking the tree
(defn text-tree-seq [text-element]
  (let [text-seq (tree-seq (fn [[_ _ children]] (every? vector? children))
                           last
                           text-element)]
    text-seq))

;; Return only the leaf nodes from text-tree-seq
(defn filter-leaves [text-seq]
	(filter (fn [v] (-> v last string?)) text-seq))

;; Given a :text tree, return [concatenated-text style-seq]
(defn text-spans [text-element]
  (let [spans (->> text-element
                (cascade-style {})
				text-tree-seq
				filter-leaves)
       strings (map last spans)]
  (log/trace "text-spans" (vec spans))
  [(clojure.string/join strings)
   (mapcat (fn [style length] (repeat length style))
       (map second spans)
       (map count strings))]))

(defn text [text-element]
  (let [leaves (-> text-element text-tree-seq filter-leaves)]
    (log/trace "text element" text-element)
    (log/trace "text leaves" (vec leaves))
    (->> leaves (map last) (apply str))))

;; return list of sequence of lines where each line is a sequence
;; of [s style length]
(defn word-wrap-text-tree [width height text-element]
  (let [[text style-seq] (text-spans text-element)
         lines (word-wrap width text)
         line-lengths (map count lines)
         style-for-line (split-n line-lengths style-seq)]
    (log/trace "lines" (vec lines))
    (log/trace "line-lengths" (vec line-lengths))
    (log/trace "style-for-line" (vec style-for-line))
    (take height
	  (map (fn [line style-for-line]
             (let [style-spans (map (fn [span] [(first span) (count span)])
                                    (partition-by identity style-for-line))]
			   (map (fn [s [style length]]
					  [s style length])
					(split-string-n (map second style-spans) line)
					style-spans)))
		   lines
           style-for-line))))

