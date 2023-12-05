(ns zaffre.keyboard
  (:import (org.lwjgl.input Keyboard)))

(defn convert-key-code [event-char event-key]
  ;; Cond instead of case. For an unknown reason, case does not match event-key to Keyboard/* constants.
  ;; Instead it always drops to the default case
  (when-let [key (condp = (int event-key)
                   Keyboard/KEY_RETURN        :enter
                   Keyboard/KEY_ESCAPE        :escape
                   Keyboard/KEY_SPACE         :space
                   (int Keyboard/KEY_BACK)    :backspace
                   ;; Function keys
                   Keyboard/KEY_F1            :f1
                   Keyboard/KEY_F2            :f2
                   Keyboard/KEY_F3            :f3
                   Keyboard/KEY_F4            :f4
                   Keyboard/KEY_F5            :f5
                   Keyboard/KEY_F6            :f6
                   Keyboard/KEY_F7            :f7
                   Keyboard/KEY_F8            :f8
                   Keyboard/KEY_F9            :f9
                   Keyboard/KEY_F10           :f10
                   Keyboard/KEY_F11           :f11
                   Keyboard/KEY_F12           :f12
                   ;; Arrow keys
                   Keyboard/KEY_UP            :up
                   Keyboard/KEY_DOWN          :down
                   Keyboard/KEY_LEFT          :left
                   Keyboard/KEY_RIGHT         :right
                   ;; Numberpad
                   Keyboard/KEY_NUMPAD1       :numpad1
                   Keyboard/KEY_NUMPAD2       :numpad2
                   Keyboard/KEY_NUMPAD3       :numpad3
                   Keyboard/KEY_NUMPAD4       :numpad4
                   Keyboard/KEY_NUMPAD5       :numpad5
                   Keyboard/KEY_NUMPAD6       :numpad6
                   Keyboard/KEY_NUMPAD7       :numpad7
                   Keyboard/KEY_NUMPAD8       :numpad8
                   Keyboard/KEY_NUMPAD9       :numpad9
                   Keyboard/KEY_NUMLOCK       :numlock
                   Keyboard/KEY_NUMPADCOMMA   :numpadcomma
                   Keyboard/KEY_NUMPADENTER   :numpadenter
                   Keyboard/KEY_NUMPADEQUALS  :numpadequals
                   ;; Left special keys
                   Keyboard/KEY_TAB           :tab
                   Keyboard/KEY_LCONTROL      :lcontrol
                   Keyboard/KEY_LMENU         :lmenu
                   Keyboard/KEY_LMETA         :lmeta
                   Keyboard/KEY_LSHIFT        :lshift
                   Keyboard/KEY_CAPITAL       :capital

                   ;; Right special keys
                   Keyboard/KEY_ADD           :add
                   Keyboard/KEY_APOSTROPHE    :apostrophe
                   Keyboard/KEY_BACKSLASH     :backslash
                   Keyboard/KEY_COLON         :colon
                   Keyboard/KEY_COMMA         :comma
                   Keyboard/KEY_DIVIDE        :divide
                   Keyboard/KEY_LBRACKET      :lbracket
                   Keyboard/KEY_MINUS         :minus
                   Keyboard/KEY_MULTIPLY      :multiply
                   Keyboard/KEY_PERIOD        :period
                   Keyboard/KEY_RBRACKET      :rbracket
                   Keyboard/KEY_RCONTROL      :rcontrol
                   Keyboard/KEY_RETURN        :return
                   Keyboard/KEY_RMENU         :rmenu
                   Keyboard/KEY_RMETA         :rmeta
                   Keyboard/KEY_RSHIFT        :rshift
                   Keyboard/KEY_EQUALS        :equals
                   Keyboard/KEY_SEMICOLON     :semicolon
                   Keyboard/KEY_SLASH         :slash
                   Keyboard/KEY_SUBTRACT      :subtract
                   Keyboard/KEY_UNDERLINE     :underline

                   ;; Symbols
                   Keyboard/KEY_AT            :at
                   Keyboard/KEY_AX            :ax
                   Keyboard/KEY_CIRCUMFLEX    :circumflex
                   Keyboard/KEY_DECIMAL       :decimal
                   Keyboard/KEY_YEN           :yen

                   Keyboard/KEY_CLEAR         :clear
                   Keyboard/KEY_CONVERT       :convert
                   Keyboard/KEY_FUNCTION      :function
                   ;; Navigation/editing
                   Keyboard/KEY_DELETE        :delete
                   Keyboard/KEY_END           :end
                   Keyboard/KEY_HOME          :home
                   Keyboard/KEY_INSERT        :insert

                   Keyboard/KEY_KANA          :kana
                   Keyboard/KEY_KANJI         :kanji
                   Keyboard/KEY_NEXT          :next
                   Keyboard/KEY_NOCONVERT     :noconvert
                   Keyboard/KEY_PAUSE         :pause
                   Keyboard/KEY_POWER         :power
                   Keyboard/KEY_PRIOR         :prior
                   Keyboard/KEY_SCROLL        :scroll
                   Keyboard/KEY_SECTION       :section
                   Keyboard/KEY_SLEEP         :sleep
                   Keyboard/KEY_STOP          :stop
                   Keyboard/KEY_SYSRQ         :sysrq
                   Keyboard/KEY_UNLABELED     :unlabeled
                   ;; event-key didn't match, default to event-char if it is printable, else nil
                   (when (<= (int (first " ")) (int event-char) (int \~))
                     event-char))]
    key))


