;; Functions for rendering animated state to screen
(ns zaffre.animation.animatedterminal)

(defprotocol AId
  (id [this]))

(defprotocol AEffect
  (apply-effect! [this terminal]))

(defprotocol AFilter
  (transform-fg [this opts x y fg])
  (transform-bg [this opts x y bg])
  (transform-fx-fg [this opts x y fx-fg])
  (transform-fx-bg [this opts x y fx-bg]))

(defprotocol AMask
  (swap-mask! [this f])
  (reset-mask! [this mask]))

(defprotocol ARequiresState
  (reset-state! [this state]))

(defprotocol APalette
  (update-palette! [this f]))

(defprotocol AAnimatedTerminal
  (swap-effect-seq! [this f])
  (swap-matching-effect-or-filter! [this p f])
  (start! [this fps]))

;; namespace with only a protocol gets optimized out, causing missing dependencies.
;; add a dummp def to prevent this ns from being optimized away.
#?(:cljs
(def x 1))
