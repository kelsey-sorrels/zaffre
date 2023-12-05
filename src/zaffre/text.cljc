(ns zaffre.text
  (:require clojure.string
            [taoensso.timbre :as log])
  (:import (java.text AttributedString)))

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
		(let [lines (vec (re-seq (re-pattern (str ".{1," size "}\\s|.{1," size "}"))
				           (clojure.string/replace (str text " ") #"\n" " ")))]
          (update lines (dec (count lines)) (fn [line] (clojure.string/trimr line))))))
    []))

;; Cascade style into descendants
(defn cascade-style [parent-style element]
  #_(log/info "cascade-style" element)
  (let [{:keys [element-type props children]} element
        {:keys [style]} props
        new-style (merge parent-style style)]
    #_(log/info "element-type" element-type (= element-type :raw-text))
    (if (= element-type :raw-text)
      ;; return a text element here.
      ;; this has the effect of not having mixed (string + text) children
      ;; by puthing strings one level deeper
      [element-type new-style (-> children deref first)]
      [element-type
       new-style
       (map (partial cascade-style new-style) @children)])))

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

(defn text-instance-tree-seq
  [text-element]
  (let [text-seq (tree-seq (fn [e] (= (:element-type e) :text))
                           (comp deref :children)
                           text-element)]
    text-seq))

(defn text [text-element]
  (let [leaves (->> text-element
                 text-instance-tree-seq
                 (filter (fn [e] (= (:element-type e) :raw-text)))
                 (map (fn [e] (-> e :children deref first))))]
    #_(log/info "text element" text-element)
    #_(log/info "text leaves" (vec leaves))
    (apply str leaves)))

(defn attributed-string
  [text-element]
  nil)

(defn break-points
  [text]
  nil)

(defn attributed-string->chars
  [attributed-string break-points]
  nil)

;; return list of sequence of lines where each line is a sequence
;; of [s style length]
(defn word-wrap-text-tree [width height text-element]
  ; if text contains spaces, wrap text
  (let [{:keys [props children]} text-element
        s (clojure.string/join " " (let [child (-> children deref first)]
                                     (if (string? child)
                                       child
                                       ; if we're dealing with nested :text just always word-wrap
                                       " ")))]
    (if (and (-> children deref count (= 1))
               (re-matches #"\s" s))
      (let [[text style-seq] (text-spans text-element)
             lines (word-wrap width text)
             line-lengths (map count lines)
             style-for-line (split-n line-lengths style-seq)]
        #_(log/info "word-wrap" text-element)
        #_(log/info "lines" (vec lines))
        #_(log/info "line-lengths" (vec line-lengths))
        #_(log/info "style-for-line" (vec style-for-line))
        (take height
          (map (fn [line style-for-line]
                 (let [style-spans (map (fn [span] [(first span) (count span)])
                                        (partition-by identity style-for-line))]
                   (map (fn [s [style length]]
                          [s style length])
                        (split-string-n (map second style-spans) line)
                        style-spans)))
               lines
               style-for-line)))
      ; else - shortcut to not do wrapping
      (let [{:keys [style]} props]
        [[[s style (count s)]]]))))

