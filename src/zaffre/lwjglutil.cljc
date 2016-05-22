; Functions for making lwjgl easier to use
(ns zaffre.lwjglutil
  (:import
    (org.lwjgl.system Platform)))
 
(defn platform []
  (condp = (Platform/get)
    Platform/LINUX :linux
    Platform/MACOSX :macosx
    Platform/WINDOWS :windows))
