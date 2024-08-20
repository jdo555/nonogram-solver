(defsystem "nonogram-solver"
  :author "Jacob Olson"
  :version "0.8.0"
  :license ""
  :description "A collection of tools for solving nonograms, and representing them with text, written in Common Lisp."
  :components ((:file "nonogram-solver")))

(defsystem "nonogram-solver/tests/test-nonograms"
  :author "Jacob Olson"
  :version "0.1.0"
  :license ""
  :description "A small collection of nonograms with their associated answers."
  :components ((:module "tests"
                :components
                ((:file "test-nonograms")))))

(defsystem "nonogram-solver/cli-demo"
  :author "Jacob Olson"
  :version "0.1.0"
  :license ""
  :description "A command-line interface for demonstrating the nonogram-solver."
  :depends-on ("nonogram-solver"
               "nonogram-solver/tests/test-nonograms")
  :components ((:module "demo"
                :components
                ((:file "cli-demo")))))
