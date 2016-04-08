(ns zaffre.keyboard
  (:import (org.lwjgl.glfw GLFW)))

(defn convert-key-code [event-char event-key]
  ;; Cond instead of case. For an unknown reason, case does not match event-key to GLFW/* constants.
  ;; Instead it always drops to the default case
  (when-let [key (condp = (int event-key)
                   ;GLFW/KEY_RETURN        :enter
                   GLFW/GLFW_KEY_ESCAPE        :escape
                   GLFW/GLFW_KEY_SPACE         :space
                   (int GLFW/GLFW_KEY_BACKSPACE)    :backspace
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

                   ;; Right special keys
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
                   (when (<= (int (first " ")) (int event-char) (int \~))
                     event-char))]
    key))


