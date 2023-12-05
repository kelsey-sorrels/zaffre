(ns zaffre.keyboard
  (:import (org.lwjgl.glfw GLFW)))

(defn is-keypress? [v]
  (or (and (char? v)
           (contains? (set (range (int \!) (int \~))) (int v)))
      (and (keyword? v)
           (contains? #{:enter :escape #_:space :backspace
                        :f1 :f2 :f3 :f4 :f5
                        :f6 :f7 :f8 :f9 :f10 :f11 :f12
                        :up :down :left :right
                        :numpad0 :numpad1 :numpad2 :numpad3
                        :numpad4 :numpad5 :numpad6 :numpad7
                        :numpad8 :numpad9 :numlock :numpadenter :numpadequal
                        :tab} v))))

(defn convert-key-code [event-key event-scancode event-action event-mod]
  ;; Cond instead of case. For an unknown reason, case does not match event-key to GLFW/* constants.
  ;; Instead it always drops to the default case
  (when-let [key (condp = event-key
                   GLFW/GLFW_KEY_ENTER         :enter
                   GLFW/GLFW_KEY_ESCAPE        :escape
                   GLFW/GLFW_KEY_SPACE         :space
                   GLFW/GLFW_KEY_BACKSPACE     :backspace
                   ;; Function keys
                   GLFW/GLFW_KEY_F1            :f1
                   GLFW/GLFW_KEY_F2            :f2
                   GLFW/GLFW_KEY_F3            :f3
                   GLFW/GLFW_KEY_F4            :f4
                   GLFW/GLFW_KEY_F5            :f5
                   GLFW/GLFW_KEY_F6            :f6
                   GLFW/GLFW_KEY_F7            :f7
                   GLFW/GLFW_KEY_F8            :f8
                   GLFW/GLFW_KEY_F9            :f9
                   GLFW/GLFW_KEY_F10           :f10
                   GLFW/GLFW_KEY_F11           :f11
                   GLFW/GLFW_KEY_F12           :f12
                   ;; Arrow keys
                   GLFW/GLFW_KEY_UP            :up
                   GLFW/GLFW_KEY_DOWN          :down
                   GLFW/GLFW_KEY_LEFT          :left
                   GLFW/GLFW_KEY_RIGHT         :right
                   ;; Numberpad
                   GLFW/GLFW_KEY_KP_0          :numpad0
                   GLFW/GLFW_KEY_KP_1          :numpad1
                   GLFW/GLFW_KEY_KP_2          :numpad2
                   GLFW/GLFW_KEY_KP_3          :numpad3
                   GLFW/GLFW_KEY_KP_4          :numpad4
                   GLFW/GLFW_KEY_KP_5          :numpad5
                   GLFW/GLFW_KEY_KP_6          :numpad6
                   GLFW/GLFW_KEY_KP_7          :numpad7
                   GLFW/GLFW_KEY_KP_8          :numpad8
                   GLFW/GLFW_KEY_KP_9          :numpad9
                   GLFW/GLFW_KEY_NUM_LOCK      :numlock
                   GLFW/GLFW_KEY_KP_ENTER      :numpadenter
                   GLFW/GLFW_KEY_KP_EQUAL      :numpadequal
                   ;; Left special keys
                   GLFW/GLFW_KEY_TAB           :tab
                   GLFW/GLFW_KEY_LEFT_CONTROL  :lcontrol
                   GLFW/GLFW_KEY_LEFT_SHIFT    :lshift
                   GLFW/GLFW_KEY_LEFT_ALT      :lalt
                   ;; Right special keys
                   GLFW/GLFW_KEY_RIGHT_CONTROL  :rcontrol
                   GLFW/GLFW_KEY_RIGHT_SHIFT    :rshift
                   GLFW/GLFW_KEY_RIGHT_ALT      :ralt
                   ;GLFW/GLFW_KEY_ADD           :add
                   ;GLFW/GLFW_KEY_APOSTROPHE    :apostrophe
                   ;GLFW/GLFW_KEY_BACKSLASH     :backslash
                   ;GLFW/GLFW_KEY_COLON         :colon
                   ;GLFW/GLFW_KEY_COMMA         :comma
                   ;GLFW/GLFW_KEY_DIVIDE        :divide
                   ;GLFW/GLFW_KEY_LEFT_BRACKET  :lbracket
                   ;GLFW/GLFW_KEY_MINUS         :minus
                   ;GLFW/GLFW_KEY_MULTIPLY      :multiply
                   ;GLFW/GLFW_KEY_PERIOD        :period
                   ;GLFW/GLFW_KEY_RIGHT_BRACKET :rbracket
                   ;GLFW/GLFW_KEY_RIGHT_CONTROL :rcontrol
                   ;GLFW/GLFW_KEY_RETURN        :return
                   ;GLFW/GLFW_KEY_RIGHT_SHIFT   :rshift
                   ;GLFW/GLFW_KEY_EQUALS        :equals
                   ;GLFW/GLFW_KEY_SEMICOLON     :semicolon
                   ;GLFW/GLFW_KEY_SLASH         :slash
                   ;GLFW/GLFW_KEY_SUBTRACT      :subtract

                   ;; Navigation/editing
                   GLFW/GLFW_KEY_DELETE        :delete
                   GLFW/GLFW_KEY_END           :end
                   GLFW/GLFW_KEY_HOME          :home
                   GLFW/GLFW_KEY_INSERT        :insert

                   ;GLFW/GLFW_KEY_PAUSE         :pause
                   ;GLFW/GLFW_KEY_PRIOR         :prior
                   ;GLFW/GLFW_KEY_SCROLL        :scroll
                   ;GLFW/GLFW_KEY_SECTION       :section
                   ;GLFW/GLFW_KEY_SYSRQ         :sysrq
                   ;; event-key didn't match, default to event-char if it is printable, else nil
                   event-key nil)]
    key))

(defn convert-action
  [action]
  ; case doesn't work for some weird reason
  (condp = (int action)
     GLFW/GLFW_PRESS :keydown
     GLFW/GLFW_RELEASE :keyup
     GLFW/GLFW_REPEAT :keyrepeat))
 
(def mod-map {
    GLFW/GLFW_MOD_SHIFT     :shift
    GLFW/GLFW_MOD_CONTROL   :control
    GLFW/GLFW_MOD_ALT       :alt
    GLFW/GLFW_MOD_SUPER     :super
    GLFW/GLFW_MOD_CAPS_LOCK :capslock
    GLFW/GLFW_MOD_NUM_LOCK  :numlock})

(defn key-mods
  [mods]
  (set (for [[k v] mod-map
             :when (pos? (bit-and k mods))]
         v)))

(def num->numpad
  {\0 :numpad0
   \1 :numpad1
   \2 :numpad2
   \3 :numpad3
   \4 :numpad4
   \5 :numpad5
   \6 :numpad6
   \7 :numpad7
   \8 :numpad8
   \9 :numpad9})
