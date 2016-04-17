;; Functions for rendering state to screen
(ns zaffre.aterminal)

(defprotocol ATerminal
  (get-size [this])
  (alter-group-pos! [this group-id f])
  (put-chars! [this layer-id characters])
  (set-fg! [this layer-id x y fg])
  (set-bg! [this layer-id x y bg])
  (assoc-fx-uniform! [this k v])
  (pub [this])
  (apply-font! [this windows-font else-font])
  (set-cursor! [this x y])
  (refresh! [this])
  (clear! [this]
          [this layer-id])
  (fullscreen! [this v])
  (fullscreen-sizes [this])
  (set-fx-fg! [this layer-id x y fg])
  (set-fx-bg! [this layer-id x y bg])
  (set-fx-char! [this layer-id x y c])
  (clear-fx! [this]
             [this layer-id])
  (destroy! [this]))

;; namespace with only a protocol gets optimized out, causing missing dependencies.
;; add a dummp def to prevent this ns from being optimized away.
#?(:cljs
(def x 1))
