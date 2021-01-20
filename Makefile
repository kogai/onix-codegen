TS_FILES := $(shell find ./ -type f -name '*.ts' | grep -v 'node_modules')
HS_FILES := $(shell find ./ -type f -name '*.hs' | grep -v '.stack-work')
BZL := npx bazelisk
BZL_BIN := $(shell npx bazel info bazel-bin)

generated/go/%: build
	stack exec onix-exe -- --schemaVersion $(@F) --language go

debug: build
	stack exec --trace -- onix-exe +RTS -xc --RTS --schemaVersion v3 --language go

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

schema: schema/v2 schema/v3

schema/%:
	mkdir -p schema
	$(BZL) build onix_$(@F)
	cp -r $(BZL_BIN)/$(@F)/ schema/$(@F)/
