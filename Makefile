TS_FILES := $(shell find ./ -type f -name '*.ts' | grep -v 'node_modules')
HS_FILES := $(shell find ./ -type f -name '*.hs' | grep -v '.stack-work')
BZL_BIN := $(shell npx bazel info bazel-bin)

.PHONY: run
run: build
	stack exec onix-exe

.PHONY: debug
debug: build
	stack exec --trace -- onix-exe +RTS -xc

.stack-work: $(HS_FILES) package.yaml stack.yaml
	stack build

build: .stack-work

fixtures/20201200.json: run
	go run github.com/kogai/onix/go/helper

# src/code.yml: $(TS_FILES)
# 	npx bazel run //:bin
# 	cp $(BZL_BIN)/bin.sh.runfiles/__main__/code.yml src/code.yml
