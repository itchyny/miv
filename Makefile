.PHONY: all
all: build

.PHONY: build
build:
	stack build

.PHONY: install
install:
	stack install

.PHONY: test
test:
	stack test

.PHONY: clean
clean:
	stack clean

.PHONY: bump
bump:
	@[ "$$(git branch --show-current)" = "main" ] || { \
		echo "Current branch is not main: $$(git branch --show-current)"; \
		exit 1; \
	}
	@[ "$$(git status --porcelain)" = " M $$(ls *.cabal)" ] || { \
		echo "Version is not updated, or unrelated files are updated:"; \
		git status --porcelain -z | xargs -0 -r printf "  %s\n" ; \
		exit 1; \
	}
	$(eval VERSION:=$(shell sed -n "s/^version: *//p" *.cabal))
	git commit -am "bump up version to $(VERSION)"
	git tag "v$(VERSION)"
	git push --atomic origin main tag "v$(VERSION)"

.PHONY: sdist
sdist:
	stack sdist --test-tarball --tar-dir .
