
# cl-metal, Common Lisp meets Metal on macOS

> cl-metal is still in the early development stage. Contributions are welcome!

An attempt to interoperate Common Lisp Array and Metal. This (should) brings the overwhelming performance of Common Lisp made possible by the use of Apple Silicon. As of this writing, this package is divided to three principle components: `:cl-metal` which provides a low-level binding for MetalKit and Common Lisp, `:cl-metal.mathkit` which provides basic mathematical operations, and `:cl-metal.linalg` providing optimized linear algebra ops. Its low-level bindings are strongly inspired from: [metalcompute](https://github.com/baldand/py-metal-compute).

The package would be dedicated to macOS with Apple Silicon. If you're looking for other devices, explore: [petalisp](https://github.com/marcoheisig/Petalisp/tree/master), [cl-cuda](https://github.com/takagi/cl-cuda).

# At a first glance

```
(define-mfunc (aux :stream t)
    (uint8-t ((x float :in) (y float :in) (a float :in) (b float :in) (m uint8-t :in)))
    (if (< m 100)
	(let ((x1 (- (* x x) (* y y) a))
	      (y1 (- (* 2.0 x y)     b)))
	  (if (> (+ (* x1 x1) (* y1 y1)) 4.0)
	      m
	      (aux x1 y1 a b (+ m 1))))
	0))

(define-kernel (mandelbrot
		:thread-position-in-grid id
		:using (aux)
		:stream t)
    (void ((x* uint8-t :out)))
    (let ((a (/ (- (mod id 2048) 512.0) 1024.0))
	  (b (/ (- (/   id 2048) 1024.0) 1024.0)))
      (setf (aref x id) (aux 0.0 0.0 a b 1))))
```

## Development

### Building libCLMetal.dylib

```sh
$ make build
```

### Running Tests

```sh
% make test
```

### Docs

(TODO)

## Features (TODO)

- Embedding inlined `.metallib` codes in Common Lisp

- Principle buffers, devices, and library managements.

- simple-array interop

- Directly embedding metal kernels on your Common Lisp Code, being compiled in ahead of execution

- Fundamental Metal APIs (BLAS, Mathematical Functions etc...)

- :cl-metal.linalg

- :cl-metal.math

- cl-waffe2, MetalTensor backend

- Creating a DSL (like mgl-mat) with a state of :direction = :in, :out. or keep compatiblity with cl-cuda?

## Environments

### Metal

cl-metal was tested on:

- Macbook Pro (2023, M3 Pro)

- (To be) Macbook Pro Early 2017 (Intel Core i5)

### Requirements/Tested on

- macOS (>= 11.0)

- SBCL(2.3.10)

- ECL(23.9.9)

