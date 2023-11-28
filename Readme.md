
# cl-metal, Common Lisp meets Metal on macOS

A low-level binding for Common Lisp Array and Metal Interoperation. Inspired from [metalcompute](https://github.com/baldand/py-metal-compute)

## Build

```sh
$ make build
```

## Test

```sh
% make test
```

## Docs

(TODO)

## Env

cl-metal was tested on:

- Macbook Pro (2023, M3 Pro)

- Macbook Pro Early 2017 (Intel i5)

## Features

- Principle buffers, devices, and library managements.

- simple-array interop

- Directly embedding metal kernels on your Common Lisp Code, being compiled in ahead of execution

- Fundamental Metal APIs (BLAS, Mathematical Functions etc...)

## Requirements

- ECL

- SBCL (still falls, w h y ???)

