all: dist

dist: dist/index.html $(shell find static -type f)
	cp -r static/* dist

dist/index.html: $(shell find src -type f -name '*.elm') elm.json
	@mkdir -p dist
	elm make src/Main.elm --optimize --output $@
