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

# ONIX 2.1 is also recoverable without the network. Commit 9352123 deleted
# 2_1_rev03_schema/ when the build moved to downloading at build time, so its
# parent still carries the files. This restores them from there. It fetches
# nothing and adds no EDItEUR file to the tree — schema/ is gitignored — so it
# changes nothing about what this repository distributes.
# See docs/adr/0003-editeur-schema-acquisition.md
ONIX_V2_COMMIT := 9352123^:2_1_rev03_schema
ONIX_V2_FILES := \
	ONIX_BookProduct_CodeLists.xsd \
	ONIX_BookProduct_Release2.1_reference.xsd \
	ONIX_BookProduct_Release2.1_short.xsd \
	ONIX_XHTML_Subset.xsd \
	ONIX_XHTML_Subset_reference.xsd \
	ONIX_XHTML_Subset_short.xsd

.PHONY: schema-from-history
schema-from-history:
	mkdir -p schema/v2
	@for f in $(ONIX_V2_FILES); do \
		git show $(ONIX_V2_COMMIT)/$$f > schema/v2/$$f || exit 1; \
		echo "restored schema/v2/$$f"; \
	done

schema: schema/v2 schema/v3

schema/%:
	mkdir -p schema
	$(BZL) build onix_$(@F)
	cp -r $(BZL_BIN)/$(@F)/ schema/$(@F)/
