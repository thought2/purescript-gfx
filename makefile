clean:
	rm -rf bower_components output node_modules dist

install:
	bower install

build: install
	pulp build

build-example:
	if [ ! -d dist/$(EXAMPLE) ] ; then mkdir dist/$(EXAMPLE); fi
	cp -r examples/public/* -t dist/$(EXAMPLE)
	pulp browserify -I examples --main $(EXAMPLE) --to dist/$(EXAMPLE)/main.js

build-examples: install build
	rm -rf dist > /dev/null
	mkdir dist
	$(MAKE) build-example EXAMPLE=Minimal
	$(MAKE) build-example EXAMPLE=BackgroundColor
	$(MAKE) build-example EXAMPLE=Points
	$(MAKE) build-example EXAMPLE=Shaders
