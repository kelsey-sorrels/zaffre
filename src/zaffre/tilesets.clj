(ns zaffre.tilesets
  (:require [zaffre.font :as zfont]
            [zaffre.util :as zutil]
            [clojure.java.io :as jio]
            [taoensso.timbre :as log])
  (:import (zaffre.font CP437Font TileSet TTFFont)))

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

(def one-bit-tileset (TileSet. "http://opengameart.org/sites/default/files/tileset_1bit.png" :green 16 16 0
                               (map->tile->col-row one-bit-map)
                               (map->tile->transparent one-bit-map false)))

;; 2-bit tileset by Muziak
;; http://pixeljoint.com/pixelart/71895.htm
(def two-bit-map [[:tree-nw :tree-ne :small-tree :stump-w :stump-e :roof-nw :roof-n :roof-ne :factory-roof-nw :factory-roof-n :factory-foor-ne :palette]
                  [:tree-sw :tree-se :hedge-nw   :hedge-n :hedge-ne :roof-sw :roof-s :roof-se :door-w :door :door-e :empty]
                  [:fence-w :fence :fence-e      :empty   :turf     :window-w :window :window-e :wall-w :wall :wall-e :empty]
                  [:water-nw :water-n :water-ne :se :sw  :flowers :vine-nw :vine-ne :hedge-e :fence-nw :fence-ne :empty]
                  [:water-w  :land   :water-e :ne :nw :pattern-1 :vine-sw :vine-se :hedge-w :fence-v :empty :fence-v :empty]
                  [:water-sw :water-s :water-se :water :turf-2 :rocks :hedge-sw :hedge-se :dock-n :fence-sw :fence-se :empty]
                  [:fruit-tree-nw :fruit-tree-ne :diamond :pattern-2 :pattern-3 :pattern-4 :pattern-5 :empty :dock-v :fence-sw-2 :fence-se-2 :empty]
                  [:fruit-tree-sw :fruit-tree-se :waves :lilly-pad :? :dock-w :dock-h :dock-e :dock-s :stairs-left-w :stairs-left-e :empty]
                  [:pattern-6 :stairs-v :empty :? :? :? :? :barrel :signpost-top :stairs-right-w :stairs-right-e :empty]
                  [:pattern-7 :empty :? :empty :? :? :? :empty :signpost-bottom :empty :empty]
                  [:pattern-8 :pattern-9 :empty :? :? :? :? :? :? :? :? :empty]
                  [:pine-nw :pine-ne :empty :empty :empty :empty :empty :empty :empty :empty :empty :empty]
                  [:pine-sw :pine-se :empty :empty :empty :empty :empty :empty :empty :empty :empty :empty]])

(def two-bit-tileset (TileSet. "http://pixeljoint.com/files/icons/full/tileset__r1752392111.png" :alpha 16 16 1
                               (map->tile->col-row two-bit-map)
                               (map->tile->transparent two-bit-map true)))

;; Roguelike tileset by Derek
;; Non-commercial
;; http://40.media.tumblr.com/7a9682566f3c68afb661f347116fad90/tumblr_mh2mhude3E1qcptgwo1_250.png
(def roguelike-map
  [[:map :empty :stairs-up :stairs-down :king :serf :ninja :? :? :worker :trap-open :trap-closed :bomb :bomb-2 :monster-1]
   [:gem :pickaxe :key-blue :bars :acolyte-1 :acoylte-2 :wall-1 :lava :spikes :shop :ent :acolyte-y-1 :acolyte-y-2 :bomb-4 :npc-1]
   [:potion-blue :scroll :key-gold :stealth :acolyte-3 :acolyte-4 :door-open :water :wyrm :snake :tree :acolyte-y-3 :acolyte-y-4 :bomb-2 :npc-2]
   [:sword-gold :armor-gold :sword-silver :armor-silver :acolyte-5 :acolyte-6 :wall-2 :dragon :sharkman :? :dragon-red :cobra :dirt :bomb-1 :npc-3]
   [:? :? :golem-gold :golem-silver :door-closed :door-locked :wall-3 :sqare :? :? :wizard :moneybag :coins-1 :coins-2 :npc-4]
   [:knight-red :knight-purple :select :? :big-jelly :small-jelly :sheep :mushroom :goblin :kobol :kobol-gold :pigman-1 :pigman-2 :centaur-1 :centaur-2]
   [:bed :? :shyguy :theif :eye :scorpion :dootdoot :ghost :rat :fox :xin :monkey :boar :minotaur-3 :empty]
   [:? :web :spider :purple-jelly :? :owl :skelly :? :? :kitsune :bat-red :bat-purple-1 :bat-purple-2 :vampire-purple :vampire-read]
   [:chest-closed :chest-open :? :bread :apple :meat-1 :meat-2 :steak :zombie :leaves :? :? :? :? :empty]
   [:potion-blue :potion-red :potion-yellow :potion-purple :potion-green :potin-brown :potion-grey :potion-black :potion-fizzy :potion-striped :potion-white :potion-beige :hand-left :head :hand-right]
   [:ring-blue :ring-red :ring-yellow :ring-purple :ring-green :ring-brown :ring-grey :ring-diamond :ring-gold :ring-silver :ring-iron :ring-glass :hand-down-left :torso :hand-down-right]
   [:katana :claymore :sabre :club :mace :fairy :axe :empty :? :? :empty :ring-dark :1 :feet :2]
   [:dagger :? :? :? :? :? :? :? :headdress :bow :? :? :dog :wold :dog-mount]
   [:staff :nunchuck :? :? :? :? :armor-leather :? :armor-chain :armor-plate :? :? :? :? :ice-wolf]
   [:staff-blue :wand :? :? :? :? :? :? :? :? :? :? :? :? :?]])

(def d-roguelike (TileSet. "http://40.media.tumblr.com/7a9682566f3c68afb661f347116fad90/tumblr_mh2mhude3E1qcptgwo1_250.png" :alpha 16 16 0
                           (map->tile->col-row roguelike-map)
                           (map->tile->transparent roguelike-map true)))

;; 16x16 Fantasy tileset by Jerom
;; CC-BY-SA 3.0
;; http://opengameart.org/content/16x16-fantasy-tileset
(def fantasy-map
  [[:door-x :door-1 :door-2 :door-open-1 :door-3 :door-open-2 :double-door :double-door-open :heavy-door :heavy-door-open]
   [:? :? :wall :stairs-down :stairs-up :tile :stone-1 :stone-2 :spikes :parquet]
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
   [:? :? :? :? :? :? :? :? :? :?]
   [:? :? :? :? :? :? :? :? :? :?]
   [:? :? :? :? :? :? :? :? :? :?]
   [:? :? :? :? :? :? :? :? :? :?]
   [:? :? :? :? :? :? :? :? :? :?]
   [:? :? :? :? :? :? :? :? :? :?]
   [:fighter :rogue :archer :ninja :dwarf :knight :theif :wizard :warlock :acyolyte]
   [:goblin :gobin-1 :goblin-2 :monkey :farmer :worker :prince :princess :queen :king]
   [:cat :bat :wolf :snake :spider :big-spider :eye :? :? :tentacle]
   [:slime :orc :mummy :goat :zombie :? :? :yeti :? :?]
   [:? :? :? :? :? :? :? :? :? :?]
   [:? :? :? :? :? :? :? :? :? :?]
   [:? :? :? :? :? :? :? :? :? :?]
   [:? :? :? :? :? :? :? :? :? :?]
   [:? :? :? :? :? :? :? :? :? :?]])

;(assert (every? (fn [line] (= (count line) 10)) fantasy-map))

(def fantasy (TileSet. "http://opengameart.org/sites/default/files/16x16_Jerom_CC-BY-SA-3.0_0.png" :alpha 16 16 0
                       (map->tile->col-row fantasy-map)
                       (map->tile->transparent fantasy-map true)))


;; CP437 tilesets from
;; http://dwarffortresswiki.org/Tileset_repository
(def pastiche-8x8 (CP437Font. "http://dwarffortresswiki.org/images/b/be/Pastiche_8x8.png" :green 1 true))
(def pastiche-16x16 (CP437Font. "http://dwarffortresswiki.org/images/b/be/Pastiche_8x8.png" :green 2 true))
(def herrbdog-7x7 (CP437Font. "http://dwarffortresswiki.org/images/9/94/Herrbdog_7x7_tileset.gif" :green 1 true))
(def herrbdog-14x14 (CP437Font. "http://dwarffortresswiki.org/images/9/94/Herrbdog_7x7_tileset.gif" :green 2 true))
(def talryth-15x15 (CP437Font. "http://dwarffortresswiki.org/images/6/6d/Talryth_square_15x15.png" :alpha 1 true))

