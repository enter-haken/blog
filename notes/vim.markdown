---
title: vim
---

When [Bram Moolenaar][vimCreator] created vim in 1991, he made a foundation for one of the best editing experiences in the world.
[Vim][vim] is a highly customizable editor, especially used and loved by developers.

<!--more-->

# .vimrc

The `.vimrc` is the config file for vim. 
You can add a own `.vimrc` in your home folder.
There are some settings, which are more or less helpful.

```
set number
```

This command shows line numbers for every buffer.

```
syntax on
```

Depending on the file extension or file type, the syntax is highlighted if possible.
This option is very use full for coding, and works for several languages.


```
set autoindent
```

When this option is enabled, every time you hit `<enter>` in *edit mode*, you get the same indention as the line above.
This is quite useful, when you're writing code blocks, especially when you're coding in a language with indention as a semantic meaning like python.

# plugins

One of the common plugin managers for Vim is called [Vundle][vundle], which is short for *Vim bundle*.
It helps tracking plugin changes and offers convenience functions for adding, updating and deleting bundles.

A minimal `.vimrc` integration looks like

```
set nocompatible
filetype off   

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'

" add plugins here 

call vundle#end()
filetype plugin indent on
```

After adding a plugin to the `.vimrc` and a reload, you can call `BundleInstall` to install the new plugins.

## syntastic

When you write code with vim, [syntastic][syntastic] is one of the most helping plugins.
It offers syntax checking for several languages.

```
Plugin 'scrooloose/syntastic'
```

## CtrlP

When you have deeply nested project folder, [CtrlP][ctrlp] helps you finding the right file using fuzzy logic.
It is quite fast, and completely written in vim script.

```
https://github.com/ctrlpvim/ctrlp.vim
```

## delimitMate

When you write code, you often open and close quotes, parenthesis and brackets. 
[delimitMate][delimitMate] helps you closing these, if necessary, but you can write the closing part if you like.

```
Plugin 'Raimondi/delimitMate'
```

## vim-airline

[vim-airline][airline] gives you a quit fast status- and tab line.

```
Plugin 'bling/vim-airline'
```

# misc

This are some vim hints, which are helpful from time to time.

## pretty print xml on a windows machine

If you want to pretty print a xml file on a windows machine, you need a running version of `libxml`.
Therefore you need the following files from the [libxml project downloads][libXmlFtp].

* [libXml][libXmlFtpLibXml]
* [iConv][libXmlFtpIconv]
* [zLib][libXmlZlib]

The binary folders should be added to the system path, so `xmllint.exe` is accessible from command line.

If you want to pretty print a xml in a open buffer you can execute

    :% !xmllint.exe "%" --format

The current open buffer is replaced with the result of `xmllint.exe`.

## ctags

You can tag your sources using [ctags][ctags].
This is a very helpful tool, when you are working on huge codebases.

First of all you have to build up the tag file in the root of your application with

    ctags -R .

After starting vim you can use

    :tag whatever

to search your application and browse to the occurrences with

    :ts

`ctags` is very helpful in combination with `CtrlP`.

    :CtrlPTag

will use the generated `tags` file for navigation through the sources.
For further reading Andrew Stewards [blog post][vimandctags] will give you more information.

[vimCreator]: https://en.wikipedia.org/wiki/Bram_Moolenaar
[vim]: https://en.wikipedia.org/wiki/Vim_(text_editor)
[vundle]: https://github.com/VundleVim/Vundle.vim
[syntastic]: https://github.com/vim-syntastic/syntastic 
[ctrlp]: https://github.com/ctrlpvim/ctrlp.vim
[delimitMate]: https://github.com/Raimondi/delimitMate
[airline]: https://github.com/vim-airline/vim-airline
[libXmlFtp]: http://xmlsoft.org/sources/win32/
[libXmlFtpIconv]: http://xmlsoft.org/sources/win32/iconv-1.9.2.win32.zip
[libXmlFtpLibXml]: http://xmlsoft.org/sources/win32/libxml2-2.7.8.win32.zip
[libXmlZlib]: http://xmlsoft.org/sources/win32/zlib-1.2.5.win32.zip
[ctags]: http://ctags.sourceforge.net/
[vimandctags]: https://andrew.stwrt.ca/posts/vim-ctags/
