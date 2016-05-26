; Functions for making lwjgl easier to use
(ns zaffre.lwjglutil
  (:import
    (org.lwjgl.system APIUtil Platform)
    (org.lwjgl.opengl GL11)))
 
(defn platform []
  (condp = (Platform/get)
    Platform/LINUX :linux
    Platform/MACOSX :macosx
    Platform/WINDOWS :windows))

(defn gl-error-string [error-code]
  (case error-code
    GL11/GL_NO_ERROR
      "No error"
    GL11/GL_INVALID_ENUM
      "Enum argument out of range"
    GL11/GL_INVALID_VALUE
      "Numeric argument out of range"
    GL11/GL_INVALID_OPERATION
      "Operation illegal in current state"
    GL11/GL_STACK_OVERFLOW
      "Command would cause a stack overflow"
    GL11/GL_STACK_UNDERFLOW
      "Command would cause a stack underflow"
    GL11/GL_OUT_OF_MEMORY
      "Not enough memory left to execute command"
    GL30/GL_INVALID_FRAMEBUFFER_OPERATION
      "Framebuffer object is not complete"
    GL30/GL_TABLE_TOO_LARGE
      "The specified table is too large"
    (APIUtil/apiUnknownToken error-code)))

- 
