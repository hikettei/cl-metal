
CL            := ros
QUICKLOAD     := --load cl-metal.asd --eval '(asdf:load-system :cl-metal)'

ARM_TARGET    := arm64-apple-macos11
X64_TARGET    := x86_64-apple-macos11

.DEFAULT_GOAL := help

.PHONY: help
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'

.PHONY: build
build: ## Builds Swift Object Files
	mkdir -p build
	swiftc -c lib/cl-metal.swift -target $(ARM_TARGET) -o build/cl-metal_ARM64.a
	swiftc -c lib/cl-metal.swift -target $(X64_TARGET) -o build/cl-metal_X64.a
	lipo -create build/cl-metal_ARM64.a build/cl-metal_X64.a -o build/cl-metal.a

.PHONT: test
test: ## Tests the whole cl-metal package
	$(CL) $(QUICKLOAD)

