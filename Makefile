TS_FILES := $(shell find ./ -type f -name '*.ts' | grep -v 'node_modules')
HS_FILES := $(shell find ./ -type f -name '*.hs' | grep -v '.stack-work')
BZL := npx bazelisk
BZL_BIN := $(shell npx bazel info bazel-bin)

.PHONY: run
run: build
	stack exec onix-exe

.PHONY: debug
debug: build
	stack exec --trace -- onix-exe +RTS -xc

.PHONY: test
test: schema
	stack test --trace --fast

.stack-work: $(HS_FILES) package.yaml stack.yaml
	stack build --fast

build: schema .stack-work

json: fixtures/20201200.json
fixtures/20201200.json: run
	go run github.com/kogai/onix-codegen/go/helper

WORKSPACE: go.mod
	$(BZL) run //:gazelle -- update-repos -from_file=go.mod

.PHONY: schema
schema: schema/v2 schema/v3

schema/v2:
	mkdir -p schema
	$(BZL) build copy_of_onix2p1
	cp -r $(BZL_BIN)/v2/ schema/v2/

schema/v3:
	mkdir -p schema
	$(BZL) build copy_of_onix3p0p7
	cp -r $(BZL_BIN)/v3/ schema/v3/
