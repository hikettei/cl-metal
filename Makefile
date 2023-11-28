
CL            := ros
QUICKLOAD     := --load cl-metal.asd --eval '(progn (load "cl-metal.asd") (ql:quickload :cl-metal))'

ARM_TARGET    := arm64-apple-macos11
X64_TARGET    := x86_64-apple-macos11

.DEFAULT_GOAL := help

.PHONY: help
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'

.PHONY: build
build: ## Builds Swift Object Files
	cd ./lib && swift build -c release

.PHONY: test
test: ## Tests the whole cl-metal package
	$(CL) $(QUICKLOAD) --eval "(asdf:test-system :cl-metal)"

