TS_FILES := $(shell find ./ -type f -name '*.ts' | grep -v 'node_modules')
HS_FILES := $(shell find ./ -type f -name '*.hs' | grep -v '.stack-work')
BZL_BIN := $(shell npx bazel info bazel-bin)

.PHONY: run
run: build
	stack exec onix-exe

.PHONY: debug
debug: build
	stack exec --trace -- onix-exe +RTS -xc

.PHONY: test
test:
	stack test --trace --fast

.stack-work: $(HS_FILES) package.yaml stack.yaml
	stack build --fast

build: .stack-work

json: fixtures/20201200.json
fixtures/20201200.json: run
	go run github.com/kogai/onix-codegen/go/helper
