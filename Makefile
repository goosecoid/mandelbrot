build:
	sbcl	--eval '(ql:quickload :cl-raylib)' \
			--eval '(asdf:load-system "mandelbrot")' \
		 	--eval '(ql:quickload :mandelbrot)' \
			--eval "(sb-ext:save-lisp-and-die #p\"mandelbrot\" :toplevel #'mandelbrot::main :executable t)"
