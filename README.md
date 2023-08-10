# Mandelbrot

A tiny fractal viewer

## Dependencies 

- CFFI
- Raylib

More info: <https://github.com/longlene/cl-raylib> 

## Run

Go to `src/main.lisp` and eval:

``` common-lisp
(ql:quickload :cl-raylib)
```

Next eval the whole buffer and run `(main)` in the REPL

You could also copy this repo in your `~/quicklisp/local-projects` and do

``` common-lisp
(ql:quickload :cl-raylib)
(ql:quickload :mandelbrot)
(mandelbrot::main)
```

## Compile

See Makefile, but basically (sbcl only afaik):

``` common-lisp
$ cd mandelbrot
$ make
```

## Screenshot

![mandelbrot](/assets/mandelbrot.png)
