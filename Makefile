HS_FILES := $(shell find ./ -type f -name '*.hs' | grep -v '.stack-work')

.PHONY: run
run: build
	stack exec onix-exe

.stack-work: $(HS_FILES) package.yaml stack.yaml
	stack build

build: .stack-work

fixtures/20201200.json: run
	go run github.com/kogai/onix/go/helper
