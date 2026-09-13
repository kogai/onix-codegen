TS_FILES := $(shell find ./ -type f -name '*.ts' | grep -v 'node_modules')
HS_FILES := $(shell find ./ -type f -name '*.hs' | grep -v '.stack-work')
BZL := npx bazelisk
# Deferred on purpose: `:=` would run bazel on every make invocation, including
# `make test`, which runs in a job with no node_modules and no need for bazel.
BZL_BIN = $(shell $(BZL) info bazel-bin)

generated/go/%: build
	stack exec onix-exe -- --schemaVersion $(@F) --language go

generated/ts/%: build
	stack exec onix-exe -- --schemaVersion $(@F) --language typescript

debug: build
	stack exec --trace -- onix-exe +RTS -xc --RTS --schemaVersion v3 --language go

# The fixtures under fixtures/ are self-contained, so the tests deliberately do
# not depend on the `schema` target: `make schema` downloads the EDItEUR
# archives over the network, and requiring it here made the test suite
# unrunnable whenever editeur.org was unreachable. Keep fixtures from including
# anything outside fixtures/, or this dependency comes back.
# See docs/adr/0002-decouple-unit-tests-from-the-vendored-schema.md
.PHONY: test
test:
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
