NODE_PACKAGEMANAGER="yarn";

.PHONY: default
default: generate 

.PHONY: help
help:
	@echo "make build - build the site generator"
	@echo "make generate - build the site generator if necessary and generate the site content"
	@echo "make clean - delete all generated site content"
	@echo "make clean_generator - delete the site generator" 
	@echo "make deep_clean - delete all generated content and the site generator" 
	@echo "make run - build all necessary resources and run the site in debug mode"
	@echo "make publish - publish site on enter-haken.github.io"

.PHONY: build
build:
	ghc --make site.hs

.PHONY: generate
generate:
	./updateLicenseIfNecessary.sh
	if [ ! -d ./node_modules/ ]; then yarn install; fi;
	if [ ! -f ./css/milligram.min.css ]; then cp ./node_modules/milligram/dist/milligram.min.css ./css; fi;
	if [ ! -f ./css/vis.min.css ]; then cp ./node_modules/vis/dist/vis.min.css ./css; fi;
	if [ ! -f ./js/vis.min.js ]; then mkdir -p ./js && cp ./node_modules/vis/dist/vis.min.js ./js; fi;
	if [ ! -f ./site ]; then make build; fi;
	./site build

clean:
	if [ ! -f ./site ]; then make build; fi;
	./site clean

.PHONY: clean_generator
clean_generator:
	rm site.hi
	rm site.o
	rm site

.PHONY: clean_node_modules
clean_node_modules:
	rm ./node_modules -rf || true
	rm ./css/milligram.min.css || true

.PHONY: deep_clean
deep_clean: clean clean_generator clean_node_modules

.PHONY: run
run:
	if [ ! -f ./site ]; then make build; fi;
	if [ ! -d ./_site ]; then make generate; fi;
	# npm install -g serve
	serve -n _site/ 

.PHONY: publish
publish:
	cd ../enter-haken.github.io/ && \
	git reset --hard && \
	cp -r ../blog/_site/* . && \
	git add * && \
	git commit -v && \
	git push



