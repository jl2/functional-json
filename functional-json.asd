
(asdf:defsystem
    :functional-json

  :description "A package for working with JSON, based on ST-JSON."

  :author '("Jeremiah LaRocco <jeremiah_larocco@fastmail.com>"
            "Marijn Haverbeke <marijnh@gmail.com>")

  :license "BSD"

  :serial t
  :depends-on (:trivial-gray-streams)

  :components ((:file "package")
               (:file "functional-json")
               (:file "readers")
               (:file "writers")
               (:file "utils")))
