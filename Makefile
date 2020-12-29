HS_FILES := $(shell find ./ -type f -name '*.hs' | grep -v '.stack-work')

.PHONY: run
run: build
	stack exec onix-exe

.stack-work: $(HS_FILES)
	stack build

build: .stack-work
