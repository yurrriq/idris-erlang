IDRIS := stack exec idris --
PKG   := erlang

.PHONY: build clean clean-all install rebuild doc doc-clean

build:
	@$(IDRIS) --build $(PKG).ipkg

clean:
	@find . -name '*.ibc' -delete

clean-all: clean doc-clean

install:
	@$(IDRIS) --install $(PKG).ipkg

rebuild: clean build

doc:
	@$(IDRIS) --mkdoc $(PKG).ipkg

doc-clean:
	@rm -rf $(PKG)_doc
