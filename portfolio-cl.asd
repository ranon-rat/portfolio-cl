(defsystem "portfolio-cl"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on ("cl-markdown"
               "ningle"
               "clack"
               "alexandria"
               "ingle"
               "cl-dotenv"
               "woo"
               "djula"
               "cl-json")
  :components
  ((:module "src"
            :serial t
            :components
            ((:file "package")
             (:file "variables")
             (:file "types")
             (:file "server")
             (:file "setup")
             (:file "controllers")
             (:file "middlewares")
             (:file "utils")
             (:file "handlers")
             (:file "templates")
             (:file "routers")
             (:file "db"))))

  :description "")


(defsystem "portfolio-cl/cmd"
  :description ""
  :serial t
  :depends-on (:portfolio-cl)
  :components ((:module "cmd"
                        :components ((:file "main")))))