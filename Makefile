
CL            := ros
QUICKLOAD     := --load cl-metal.asd --eval '(asdf:load-system :cl-metal)'
.DEFAULT_GOAL := help

.PHONY: help
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'

.PHONY: compile
compile: ## Builds Swift Object Files
	mkdir -p build/swift

.PHONT: test
test: ## Tests the whole cl-metal package
	$(CL) $(QUICKLOAD)

