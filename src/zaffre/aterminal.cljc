;; Functions for rendering state to screen
(ns zaffre.aterminal)

(defprotocol ATerminal
  (get-size [this])
  (put-chars! [this layer-id characters])
  (set-fg! [this x y fg])
  (set-bg! [this x y bg])
  (get-key-chan [this])
  (apply-font! [this windows-font else-font size antialias])
  (set-cursor! [this x y])
  (refresh! [this])
  (clear! [this]
          [this layer-id])
  (set-fx-fg! [this x y fg])
  (set-fx-bg! [this x y bg])
  (set-fx-char! [this x y c])
  (clear-fx! [this])
  (destroy! [this]))

;; namespace with only a protocol gets optimized out, causing missing dependencies.
;; add a dummp def to prevent this ns from being optimized away.
#?(:cljs
(def x 1))
