site:
	ghc --make site.hs

build:
	./site build

rebuild:
	./site rebuild

clean:
	./site clean

serve:
	./site watch

run: rebuild serve


