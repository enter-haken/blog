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

run: rebuild serve


