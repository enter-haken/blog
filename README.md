blog
----

blog sources for enter-haken.github.io

additional stuff
----------------

nodejs is mandatory for the build process

    yarn global add serve
    yarn global add sass 

or

    npm install -g serve
    npm install -g sass

will install the necessary components for the preview and the sass compilation.


```
./updateLicenseIfNecessary.sh
./milligram_build.sh
Submodule path 'milligram': checked out '7a1c78f313363797cdc70a4f002ee5a3a3a31da7'
cp milligram/dist/milligram.min.css css
if [ ! -f ./site ]; then make build; fi;
make[1]: Entering directory '/home/gooose/src/published/blog'
ghc --make site.hs
[1 of 1] Compiling Main             ( site.hs, site.o )
Linking site ...
make[1]: Leaving directory '/home/gooose/src/published/blog'
./site build
Initialising...
  Creating store...
  Creating provider...
  Running rules...
Checking for out-of-date items
Compiling
  updated templates/default.html
  updated about.markdown
  updated atom.xml
  updated cookbook.markdown
  ...
  updated index.html
  updated license.markdown
  updated templates/note.html
  updated notes/haskell.markdown
  updated notes/linux.markdown
  updated notes/sandcastle.markdown
  updated notes/vim.markdown
  updated templates/note-list.html
  updated notes.html
  updated projects.markdown
  updated read.markdown
Success
```

A local webserver will be started when
    
    $ make run

You can see the generated result with

    http://localhost:5000


Contact
-------

Jan Frederik Hake, <jan_hake@gmx.de>. [@enter_haken](https://twitter.com/enter_haken) on Twitter.
