(phpinspect--root-index
 (classes
  ("\\App\\Entity\\AuthToken" phpinspect--class
   (methods #s(phpinspect--function "getToken"
                                    (:public)
                                    nil "\\bool") ;; A changed function
            #s(phpinspect--function "anAddedFunction"
                                    (:public)
                                    nil nil)      ;; A new function
            #s(phpinspect--function "getCreationTime"
                                    (:public)
                                    nil "\\DateTime")
            #s(phpinspect--function "isValid"
                                    (:public)
                                    nil "\\bool")
            #s(phpinspect--function "hasStudentRole"
                                    (:public)
                                    nil "\\bool")
            #s(phpinspect--function "getUser"
                                    (:public)
                                    nil "\\App\\Entity\\User")
            #s(phpinspect--function "getToken"
                                    (:public)
                                    nil "\\string")
            #s(phpinspect--function "__construct"
                                    (:public)
                                    (("token" "\\string")
                                     ("user" "\\App\\Entity\\User")
                                     ("valid" "\\bool")
                                     ("creation_time" "\\DateTime"))
                                    nil))
   (class-name . "\\App\\Entity\\AuthToken")
   (static-methods)
   (static-variables)
   (variables #s(phpinspect--variable "extra"      ;; an added variable attribute
                                      (:private)
                                      nil)
              #s(phpinspect--variable "creation_time"
                                      (:private)
                                      "\\DateTime")
              #s(phpinspect--variable "valid"
                                      (:private)
                                      "\\bool")
              #s(phpinspect--variable "user"
                                      (:private)
                                      "\\App\\Entity\\App\\\\Entity\\\\User")
              #s(phpinspect--variable "token"
                                      (:private)
                                      "\\string"))
   (constants)
   (extends)
   (implements)))
 (functions))
