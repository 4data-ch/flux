# Top level Makefile for the entire project
#
# This Makefile encodes the "go generate" prerequeisites ensuring that the proper tooling is installed and
# that the generate steps are executed when their prerequeisites files change.
#
# This Makefile follows a few conventions:
#
#    * All cmds must be added to this top level Makefile.
#    * All binaries are placed in ./bin, its recommended to add this directory to your PATH.
#    * Each package that has a need to run go generate, must have its own Makefile for that purpose.
#    * All recursive Makefiles must support the targets: generate and clean.
#

SHELL := /bin/bash

GO_TAGS=libflux
GO_ARGS=-tags '$(GO_TAGS)'

# Test vars can be used by all recursive Makefiles
export GOOS=$(shell go env GOOS)
export GO_BUILD=env GO111MODULE=on go build $(GO_ARGS)
export GO_TEST=env GO111MODULE=on go test $(GO_ARGS)
export GO_TEST_FLAGS=
# Do not add GO111MODULE=on to the call to go generate so it doesn't pollute the environment.
export GO_GENERATE=go generate $(GO_ARGS)
export GO_VET=env GO111MODULE=on go vet $(GO_ARGS)
export CARGO=cargo

define go_deps
	$(shell env GO111MODULE=on go list -f "{{range .GoFiles}} {{$$.Dir}}/{{.}}{{end}}" $(1))
endef

default: build

STDLIB_SOURCES = $(shell find . -name '*.flux')

GENERATED_TARGETS = \
	ast/asttest/cmpopts.go \
	internal/scanner/scanner.gen.go \
	stdlib/packages.go

generate: $(GENERATED_TARGETS)

.SECONDEXPANSION:
ast/asttest/cmpopts.go: ast/ast.go ast/asttest/gen.go $$(call go_deps,./internal/cmd/cmpgen)
	$(GO_GENERATE) ./ast/asttest

stdlib/packages.go: $(STDLIB_SOURCES)
	$(GO_GENERATE) ./stdlib

internal/scanner/unicode.rl: internal/scanner/unicode2ragel.rb
	ruby unicode2ragel.rb -e utf8 -o internal/scanner/unicode.rl
internal/scanner/scanner.gen.go: internal/scanner/gen.go internal/scanner/scanner.rl internal/scanner/unicode.rl
	$(GO_GENERATE) ./internal/scanner

libflux: libflux/target/debug/libflux.a

libflux/target/debug/libflux.a:
	@echo "cd libflux && $(CARGO) build"
	@cd libflux && $(CARGO) build && if [[ "$$GOOS" == "darwin" ]]; then \
		sed -i '' -e "s@${CURDIR}/@@g" -e "s@debug/debug@debug@g" target/debug/libflux.d; \
	else \
		sed -i -e "s@${CURDIR}/@@g" -e "s@debug/debug@debug@g" target/debug/libflux.d; \
	fi
-include libflux/target/debug/libflux.d

build: libflux
	$(GO_BUILD) ./...

clean:
	rm -rf bin
	cd libflux; $(CARGO) clean

cleangenerate:
	rm -f $(GENERATED_FILES)

fmt: $(SOURCES_NO_VENDOR)
	go fmt ./...
	cd libflux; $(CARGO) fmt

checkfmt:
	./etc/checkfmt.sh

tidy:
	GO111MODULE=on go mod tidy

checktidy:
	./etc/checktidy.sh

checkgenerate:
	./etc/checkgenerate.sh

staticcheck:
	GO111MODULE=on go mod vendor # staticcheck looks in vendor for dependencies.
	GO111MODULE=on go run honnef.co/go/tools/cmd/staticcheck ./...

test: test-go test-rust

test-go: libflux
	$(GO_TEST) $(GO_TEST_FLAGS) ./...

test-rust:
	cd libflux && $(CARGO) test

test-race: libflux
	$(GO_TEST) -race -count=1 ./...

test-bench: libflux
	$(GO_TEST) -run=NONE -bench=. -benchtime=1x ./...

vet: libflux
	$(GO_VET) ./...

bench:
	$(GO_TEST) -bench=. -run=^$$ ./...

release:
	./release.sh



.PHONY: generate \
	clean \
	cleangenerate \
	build \
	default \
	libflux \
	fmt \
	checkfmt \
	tidy \
	checktidy \
	checkgenerate \
	staticcheck \
	test \
	test-go \
	test-rust \
	test-race \
	test-bench \
	vet \
	bench \
	checkfmt \
	release

