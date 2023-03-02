.PHONY: all
all: build

.PHONY: build
build:
	stack build

.PHONY: install
install:
	stack install

.PHONY: test
test: build
	stack test

.PHONY: clean
clean:
	stack clean

VERSION := $$(sed -n '/^version:/s/^.*  *\([0-9][0-9.]*\).*$$/\1/p' miv.cabal)
GIT_DIFF := $$(git diff --name-only)

.PHONY: bump
bump:
	test -z "$$(git status --porcelain || echo .)"
	test "$$(git branch --show-current)" = "main"
	@printf "Bump up version in miv.cabal. Press Enter to proceed: "
	@read -n1
	@[ "$(GIT_DIFF)" == "miv.cabal" ] || { \
		echo "Version is not updated or unrelated file is updated:"; \
		[ -z "$(GIT_DIFF)" ] || printf "  %s\n" $(GIT_DIFF); \
		exit 1; \
	}
	git commit -am "bump up version to $(VERSION)"
	git tag "v$(VERSION)"
	git push --atomic origin main tag "v$(VERSION)"

.PHONY: sdist
sdist:
	stack sdist --tar-dir .
