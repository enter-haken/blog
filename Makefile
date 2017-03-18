help:
	@echo "make site - build the generator"
	@echo "make buld - generate the site itself"
	@echo "make rebuild - rebuild the site itself"
	@echo "make clean - delete all generated content and the build resource"
	@echo "make serve - run the site in debug mode"
	@echo "make publish - publish site on enter-haken.github.io"
	@echo "make run - site build serve"

site:
	ghc --make site.hs

build:
	./site build

rebuild:
	./site rebuild

clean:
	./site clean
	rm site.hi
	rm site.o
	rm site

serve:
	./site watch

publish:
	cd ../enter-haken.github.io/ && \
	git reset --hard && \
	cp -r ../blog/_site/* . && \
	git add * && \
	git commit -v && \
	git push

run: site build serve 


