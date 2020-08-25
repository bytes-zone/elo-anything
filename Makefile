all: dist/index.html

dist/index.html: $(shell find src -type f -name '*.elm') elm.json
	@mkdir -p dist
	elm make src/Main.elm --output $@
