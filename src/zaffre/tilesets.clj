(ns zaffre.tilesets
  (:require [zaffre.font :as zfont]
            [zaffre.util :as zutil]
            [clojure.java.io :as jio]
            [taoensso.timbre :as log])
  (:import (zaffre.font CP437Font TTFFont)))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defn map->tile->col-row [tilemap]
  (reduce (fn [m [t col row]]
            (assoc m t [col row]))
          {}
          (mapcat concat
            (map-indexed (fn [row line]
                           (map-indexed (fn [col t]
                                          [t col row])
                                        line))
                         tilemap))))

(defn map->tile->transparent [tilemap v]
  (zipmap (flatten tilemap) (repeat v)))

;; 1-bit tileset by Clint Bellanger
;; CC-BY 3.0
;; http://opengameart.org/content/tileset-1bit-color
(def one-bit-map [[:metal      :panels      :stone        :masonry         :dark-metal :dark-panels  :grass       :tree]
                  [:water-nw   :water-n     :water-ne     :column          :parapet    :parapet-flag :sign        :palm]
                  [:water-w    :water       :water-e      :fence           :gate       :dead-tree    :tombstone1  :tombstone2]
                  [:water-sw   :water-s     :water-se     :black           :pine-tree  :cave         :cave-nw     :cave-ne]
                  [:water-f-se :water-f-sw  :masonry-top  :wall-top        :gravel     :mountain     :cave-sw     :cave-se]
                  [:water-f-se :water-f-nw  :window       :wall            :robot      :lever-left   :lever-right :key]
                  [:dark-water :locked-door :open-door    :masonry-parapet :window     :ladder       :stairs-up   :stairs-down]
                  [:cactus     :skull       :boulder      :crate           :barrel     :chest-closed :chest-open  :blank]])

(def one-bit-tileset
  (delay (zfont/tileset "http://opengameart.org/sites/default/files/tileset_1bit.png"
                        :green 16 16 0
                        one-bit-map
                        (map->tile->transparent one-bit-map false))))

;; 2-bit tileset by Muziak
;; http://pixeljoint.com/pixelart/71895.htm
(def two-bit-map [[:tree-nw :tree-ne :small-tree :stump-w :stump-e :roof-nw :roof-n :roof-ne :factory-roof-nw :factory-roof-n :factory-foor-ne :palette]
                  [:tree-sw :tree-se :cliff-nw   :cliff-n :cliff-ne :roof-sw :roof-s :roof-se :door-w :door :door-e :empty]
                  [:fence-w :fence :fence-e      :empty   :turf     :window-w :window :window-e :wall-w :wall :wall-e :empty]
                  [:water-nw :water-n :water-ne :se :sw  :flowers :vine-nw :vine-ne :cliff-e :fence-nw :fence-ne :empty]
                  [:water-w  :land   :water-e :ne :nw :pattern-1 :vine-sw :vine-se :cliff-w :fence-v :empty :fence-v :empty]
                  [:water-sw :water-s :water-se :water :turf-2 :rocks :cliff-sw :cliff-se :dock-n :fence-sw :fence-se :empty]
                  [:fruit-tree-nw :fruit-tree-ne :diamond :pattern-2 :pattern-3 :pattern-4 :pattern-5 :empty :dock-v :fence-sw-2 :fence-se-2 :empty]
                  [:fruit-tree-sw :fruit-tree-se :waves :lilly-pad :is-h :dock-w :dock-h :dock-e :dock-s :stairs-left-w :stairs-left-e :empty]
                  [:pattern-6 :stairs-v :empty :pen-n :is-v :pan-sw :pwn-se :barrel :signpost-top :stairs-right-w :stairs-right-e :empty]
                  [:pattern-7 :empty :pen-w :empty :pen-e :pen-nw :pen-ne :empty :signpost-bottom :empty :empty]
                  [:pattern-8 :pattern-9 :empty :pen-s :cliff-v-se :cliff-v-sw :cliff-nw :cliff-n :cliff-ne :cliff-h-w :cliff-h-e :empty]
                  [:pine-nw :pine-ne :empty :empty :empty :empty :empty :empty :empty :empty :empty :empty]
                  [:pine-sw :pine-se :empty :empty :empty :empty :empty :empty :empty :empty :empty :empty]])

(def two-bit-tileset
  (delay (zfont/tileset "http://pixeljoint.com/files/icons/full/tileset__r1752392111.png"
                        :alpha 16 16 0
                        two-bit-map
                        (map->tile->transparent two-bit-map true))))

;; Roguelike tileset by Derek
;; Non-commercial
;; http://40.media.tumblr.com/7a9682566f3c68afb661f347116fad90/tumblr_mh2mhude3E1qcptgwo1_250.png
(def roguelike-map
  [[:map :empty :stairs-up :stairs-down :king :serf :ninja :ring-face :steam :worker :trap-open :trap-closed :bomb :bomb-2 :monster-1]
   [:gem :pickaxe :key-blue :bars :acolyte-1 :acoylte-2 :wall-1 :lava :spikes :shop :ent :acolyte-y-1 :acolyte-y-2 :bomb-4 :npc-1]
   [:potion-blue :scroll :key-gold :stealth :acolyte-3 :acolyte-4 :door-open :water :wyrm :snake :tree :acolyte-y-3 :acolyte-y-4 :bomb-2 :npc-2]
   [:sword-gold :armor-gold :sword-silver :armor-silver :acolyte-5 :acolyte-6 :wall-2 :dragon :sharkman :clover :dragon-red :cobra :dirt :bomb-1 :npc-3]
   [:red-man :red-man-horns :golem-gold :golem-silver :door-closed :door-locked :wall-3 :square :gold-man :keyhole :wizard :moneybag :coins-1 :coins-2 :npc-4]
   [:knight-red :knight-purple :select :crown :big-jelly :small-jelly :sheep :mushroom :goblin :kobol :kobol-gold :pigman-1 :pigman-2 :centaur-1 :centaur-2]
   [:bed :demon :shyguy :theif :eye :scorpion :dootdoot :ghost :rat :fox :xin :monkey :boar :minotaur-3 :empty]
   [:light-demon :web :spider :purple-jelly :mushroom :owl :skelly :skelly-lord :crazy-man :kitsune :bat-red :bat-purple-1 :bat-purple-2 :vampire-purple :vampire-read]
   [:chest-closed :chest-open :bullet :bread :apple :meat-1 :meat-2 :steak :zombie :leaves :bun :big-bun :lemon :plum :empty]
   [:potion-blue :potion-red :potion-yellow :potion-purple :potion-green :potin-brown :potion-grey :potion-black :potion-fizzy :potion-striped :potion-white :potion-beige :hand-left :head :hand-right]
   [:ring-blue :ring-red :ring-yellow :ring-purple :ring-green :ring-brown :ring-grey :ring-diamond :ring-gold :ring-silver :ring-iron :ring-glass :hand-down-left :torso :hand-down-right]
   [:katana :claymore :sabre :club :mace :fairy :gold-axe :empty :imp :purple-monster :empty :ring-dark :1 :feet :2]
   [:dagger :short-sword :sword :bastard-sword :foil :sabre :scimitar :? :headdress :bow :axe :double-axe :dog :wold :dog-mount]
   [:staff :nunchuck :three-section-staff :halberd :glave :fauchard :armor-leather :armor-pelt :armor-chain :armor-plate :silver-plate :bronze-plate :iron-plate :gold-plate :ice-wolf]
   [:wand-blue :wand-red :wand-yellow :wand-purple :wand-green :wand-brown :quiver :ice-quiver :robes :red-armor :grey-armor :grey-plate :full-armor :? :?]])

(def d-roguelike
  (delay (zfont/tileset "http://40.media.tumblr.com/7a9682566f3c68afb661f347116fad90/tumblr_mh2mhude3E1qcptgwo1_250.png"
                        :alpha 16 16 0
                        roguelike-map
                        (map->tile->transparent roguelike-map true))))

;; 16x16 Fantasy tileset by Jerom
;; CC-BY-SA 3.0
;; http://opengameart.org/content/16x16-fantasy-tileset
(def fantasy-map
  [[:door-x :door-1 :door-2 :door-open-1 :door-3 :door-open-2 :double-door :double-door-open :heavy-door :heavy-door-open]
   [:steps :step-wall :wall :stairs-down :stairs-up :tile :stone-1 :stone-2 :spikes :parquet]
   [:wall-nw :wall-n :wall-ne :wall-2-nw :wall-2-ne :floor-1 :floor-2 :floor-3 :floor-4 :floor-5]
   [:wall-w :wall-s-open :wall-e :wall-2-sw :wall-2-se :floor-1-nw :floor-1-ne :floor-1-w :floor-2-nw :floor-2-ne]
   [:wall-sw :wall-s-closed :wall-se :wall-stairs :wall-s-1 :floor-1-sw :floor-1-se :floor-2-w :floor-2-sw :fllor-2-se]
   [:floor :grass :weeds :shrubs :ice :snow :water :pine :tree :bridge]
   [:tombstone :grave :posts :sign :chasm :hole :moutain :cave :rocks :volcano]
   [:ocean :ocean-2 :ship :whirlpool :house :house-x :house-2 :store :castle-1 :castle-2]
   [:key-1 :key-2 :key-3 :key-4 :chest-closed :chest-open :pot :fire :bookcase :bed]
   [:coins :purse :ring-1 :ring-2 :amulet :bones :meat :flask :poption :jug]
   [:book-1 :book-2 :book-3 :book-4 :book-5 :book-6 :book-7 :book-8 :book-9 :book-10]
   [:scroll-0 :scroll-1 :scroll-2 :scroll-3 :scroll-4 :scroll-5 :scroll-6 :scroll-7 :scroll-8 :scroll-9]
   [:scroll-a :scroll-b :scroll-c :scroll-d :scroll-e :scroll-f :scroll-g :scroll-h :scroll-i :scroll-j]
   [:blade-0 :blade-1 :blade-2 :blade-3 :blade-4 :blade-5 :blade-6 :blade-8 :blade-9 :blade-10]
   [:hammer-0 :hammer-1 :hammer-2 :hammer-3 :hammer-4 :hammer-5 :hammer-6 :hammer-8 :hammer-9 :hammer-10]
   [:slingshot :boomarang-0 :boomarang-1 :boomarang-2 :bow-0 :bow-1 :bow-2 :arrow-0 :arrow-1 :arrow-2]
   [:armor-0 :armor-1 :armor-2 :armor-3 :armor-4 :armor-5 :armor-6 :armor-8 :armor-9 :armor-10]
   [:shield-0 :shield-1 :shield-2 :shield-3 :shield-4 :shield-5 :shield-6 :shield-8 :shield-9 :shield-10]
   [:helmet-0 :helmet-1 :helmet-2 :helmet-3 :helmet-4 :helmet-5 :helmet-6 :helmet-8 :helmet-9 :helmet-10]
   [:fighter :rogue :archer :ninja :dwarf :knight :theif :wizard :warlock :acyolyte]
   [:goblin :gobin-1 :goblin-2 :monkey :farmer :worker :prince :princess :queen :king]
   [:cat :bat :wolf :snake :spider :big-spider :eye :small-pudding :large-pudding :tentacle]
   [:slime :orc :mummy :goat :zombie :zombie-lord :devil :yeti :dryad :gryffin]
   [:health :strength :weapon :armod :shield :arrow :scroll :fireball :lightning :target]
   [:charge :move :wait :monster :sleep :talk :pause :alert :question :cancel]
   [:heart-100 :heart-75 :heart-50 :heart-25 :heart-0 :fist :shoot :pray :fire :boulder]
   [:? :? :? :? :? :? :? :? :? :?]
   [:select-0 :select-1 :? :? :? :? :? :? :? :?]])

;(assert (every? (fn [line] (= (count line) 10)) fantasy-map))

(def fantasy
  (delay (zfont/tileset "http://opengameart.org/sites/default/files/16x16_Jerom_CC-BY-SA-3.0_0.png"
                        :alpha 16 16 0
                        fantasy-map
                        (map->tile->transparent fantasy-map true))))


;; CP437 tilesets from
;; http://dwarffortresswiki.org/Tileset_repository
(def pastiche-8x8
  (delay (zfont/cp-437 "http://dwarffortresswiki.org/images/b/be/Pastiche_8x8.png" :green true)))
(def pastiche-16x16
  (delay (zfont/scale (zfont/cp-437 "http://dwarffortresswiki.org/images/b/be/Pastiche_8x8.png" :green true) 2)))
(def herrbdog-7x7
  (delay (zfont/cp-437 "http://dwarffortresswiki.org/images/9/94/Herrbdog_7x7_tileset.gif" :green true)))
(def herrbdog-14x14
  (delay (zfont/scale (zfont/cp-437 "http://dwarffortresswiki.org/images/9/94/Herrbdog_7x7_tileset.gif" :green true) 2)))
(def talryth-15x15
  (delay (zfont/cp-437 "http://dwarffortresswiki.org/images/6/6d/Talryth_square_15x15.png" :alpha  true)))

;; Opaque version of CP437 fonts
(def pastiche-8x8-op
  (delay (zfont/cp-437 "http://dwarffortresswiki.org/images/b/be/Pastiche_8x8.png" :green false)))
(def pastiche-16x16-op
  (delay (zfont/scale (zfont/cp-437 "http://dwarffortresswiki.org/images/b/be/Pastiche_8x8.png" :green  false) 2)))
(def herrbdog-7x7-op
  (delay (zfont/cp-437 "http://dwarffortresswiki.org/images/9/94/Herrbdog_7x7_tileset.gif" :green false)))
(def herrbdog-14x14-op
  (delay (zfont/scale (zfont/cp-437 "http://dwarffortresswiki.org/images/9/94/Herrbdog_7x7_tileset.gif" :green false) 2)))
(def talryth-15x15-op
  (delay (zfont/cp-437 "http://dwarffortresswiki.org/images/6/6d/Talryth_square_15x15.png" :alpha false)))
