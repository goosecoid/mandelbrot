(defsystem "mandelbrot"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "mandelbrot/tests")))
  :build-operation "program-op"
  :build-pathname "mandelbrot"
  :entry-point "mandelbrot::main")

(defsystem "mandelbrot/tests"
  :author ""
  :license ""
  :depends-on ("mandelbrot"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for mandelbrot"
  :perform (test-op (op c) (symbol-call :rove :run c))
  )
