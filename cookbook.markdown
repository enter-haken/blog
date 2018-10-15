---
title: Cookbook
---

This is my loose collection of snippets, applications and commands, I use from day to day.

# dot

[dot](https://en.wikipedia.org/wiki/DOT_(graph_description_language)) is a graph description language.
Assuming a image viewer like [feh](https://feh.finalrewind.org) is installed

```
echo 'digraph { a -> b; b -> c; a -> c; }' | dot -Tpng | feh -
```

will show 

![](/images/dot_test.png)

# snippet

remove empty lines from output

```
sed '/^\s*$/d'
```

count lines of code (simple)

```
find . -name '*.extension' | xargs wc -l
```

same as above without emtpy lines and comments for erlang files

```
find . -name '*.erl' | xargs cat | sed -e '/^%/d' -e '/^\s*$/d' | wc -l
```

# bash

    HISTSIZE=""

or

    HISTSIZE="INFINITE"

will let the `~/.bash_history` grow infinitely. [source](https://superuser.com/questions/479726/how-to-get-infinite-command-history-in-bash)

## gentoo specific aliases

    alias eqf='equery f'
    alias equ='equery u'
    alias eqh='equery h'
    alias eqa='equery a'
    alias eqb='equery b'
    alias eql='equery l'
    alias eqd='equery d'
    alias eqg='equery g'
    alias eqc='equery c'
    alias eqk='equery k'
    alias eqm='equery m'
    alias eqy='equery y'
    alias eqs='equery s'
    alias eqw='equery w'
    
    alias current="sudo genlop -c"

# git
pull all repositories located in sub directories, assuming there are only subdirectories in the current directory

```bash
#!/bin/bash
ls | xargs -I{} git -C {} pull
```
## reset a single file

```
git checkout HEAD -- fileName.xxx
```

## aliases

common aliases

```
git config --global alias.co checkout
git config --global alias.br branch
git config --global alias.ci commit
git config --global alias.st status
```

### reset a single file to head

```
git config --global alias.unstage "reset HEAD --"
git unstage file.txt
```

### show graph

```
git config --global alias.l "log --graph --oneline --all"
```

### cat diff to default browser

With [diff2html](https://diff2html.xyz/) a `git diff` output can be transformed into html.

    npm install -g diff2html

[bcat](https://rtomayko.github.io/bcat/) is a tool for piping data to the standard browser.

    git diff branch_1..branch_2 | diff2html -i stdin -o stdout | bcat

With an alias

    alias bdiff="diff2html -i stdin -o stdout | bcat"

the call can be simplified to.

    git diff branch_1..branch_2 | bdiff

If you want to see the history of a file, you can use
    
    git log -p filename | bdiff

### cat the content of a libreoffice or docx document to console

    alias lcat="libreoffice --cat"

with 
    
    lcat document.docx | vim -

you can browse the text content in your favourite editor ;)

# firefox addons

* [VimFx](https://addons.mozilla.org/de/firefox/addon/vimfx/) - using vim commands for browsing
* [MarkdownViewer](https://addons.mozilla.org/de/firefox/addon/markdown-viewer/) - render markdown files

# tmux

[tmux shortcuts & cheatsheet](https://gist.github.com/MohamedAlaa/2961058)

basic

* `tmux a` - atach
* `<ctrl> b d` - detatch
* `<shift>` - select text with left mouse bottom [link](https://awhan.wordpress.com/2012/04/18/tmux-copy-paste-with-mouse/)
* `<shift>` - paste text with right mouse bottom

Window

* `<ctrl> b c` - create new window
* `<ctrl> b n` - next window

Pane

* `<ctrl> b x` - kill pane
* `<ctrl> b "` - create horizontal pane
* `<ctrl> b %` - create vertical pane

# batch

* `for /F %L in (commandParameters.txt) do echo %L` - Execute command for every line in given textfile (in this case the `echo` command) / [for command](https://technet.microsoft.com/en-us/library/bb490909.aspx)

# vim

* [vimsheed](http://vimsheet.com/) - a good summary of vim commands.

## movement

basic movement

* `k` - up
* `j` - down
* `h` - left 
* `l` - right

jumps

* `0` - beginning of a line
* `$` - end of a line
* `w` - beginning of a world
* `)` - forward one sentence
* `(` - backward one sentence

## visual

* `vi'` - select inner text between two `'` 
* `<C-V>` - select column
    * `I` - insert text -> `<ESC>` -> insert text in front of the selection

## repeats
* `g` - global
   * `:g/Test/d` - delete all lines containing `Test`
   * `:g!/Test/d` - delete all lines not containing `Test`
   
## substitute
* `:%s/foo/bar/g` - replace all `foo` with `bar`

## settings
* `:set ignorecase` - case insensitive search
* `:set wrap` / `:set nowrap` - enables / disables automatic line breaks

## misc

* `u` - undo
* `<ctrl> r` - redo
* `:w !sudo tee %` - save file, if you forgot `sudo`
* `:so $MYVIMRC` - reload the current `.vimrc`
* `e $MYVIMRC` - edit the current `.vimrc`

# haskell

[How I start by Chris Allen](http://howistart.org/posts/haskell/1)

Preconditions

* ghci / cabal / stack installed
* `stack new projectName simple` - start a new simple haskell project

after changed into project folder

* `stack setup` - init project
* `stack build` - build the project
* `stack exec projectName` - start programm
* `stack ghci` - load project aware ghci

## ghci

* `:set -XOverloadedStrings` - activate overloaded string extension

# erlang / elixir

## observer

`Erlang` has to be emerged with `smp` and `wxwidgets`use flag.
For `elixir` you can start the `observer` with

    $ iex
    Erlang/OTP 20 [erts-9.1] [source] [64-bit] [smp:8:8] [async-threads:10]
    
    Interactive Elixir (1.5.2) - press Ctrl+C to exit (type h() ENTER for help)
    iex(1)> :observer.start
    :ok
    iex(2)>

![observer](/images/observer.png)

# Windows batch programming

[Windows batch scripting](https://en.wikibooks.org/wiki/Windows_Batch_Scripting)

* `START "" http://localhost` - starts a default browser with `http://localhost` or opens a new tab.

# .NET / C\#

## Force to copy an unused assembly

When the compiler sees no usage of a referenced assembly, it won't be copied to the output folder.
If you need for some reason the assembly in your output folder, you can create a dummy usage, which forces the compiler to copy the assembly.

```
private static void Dummy()
{
    Action<Type> doesNothingWith = _ => { };
    var anyType = typeof(ReferencedAssembly.SomeClass);
    doesNothingWith(anyType);
}
```

## Development stack

Sometimes it is usefull, to start a build from the command line. 
A `make_debugBuild.bat` can look like

```
call "C:\Program Files (x86)\Microsoft Visual Studio 14.0\Common7\Tools\VsDevCmd.bat"
msbuild SolutionName.sln /p:Configuration=Debug
```

For a `make_releaseBuild.bat` you can just change the msbuild `Configuration` parameter.

```
call "C:\Program Files (x86)\Microsoft Visual Studio 14.0\Common7\Tools\VsDevCmd.bat"
msbuild SolutionName.sln /p:Configuration=Release
```

To delete common generated / build folders from command line, you can use a `cleanup.bat` like

```
FOR /F "tokens=*" %%G IN ('DIR /B /AD /S obj') DO RMDIR /S /Q "%%G"
FOR /F "tokens=*" %%G IN ('DIR /B /AD /S bin') DO RMDIR /S /Q "%%G"

REM RMDIR /S /Q AdditionalFolderToDelete
```

# Windows

Here comes the Windows related stuff

## Chocolatey

A package manager for Windows.

### Installation

Before the installation you must allow remote scripts to run.
Start a powershell with administrator rights

```
Set-ExecutionPolicy RemoteSigned
iwr https://chocolatey.org/install.ps1 -UseBasicParsing | iex
```
### Ack

```
choco install ack 
```

# misc

## whatching amazon prime video from chromium on a gentoo system via chromecast

At first the `widevine` use flag for `chromium` has to be enabled.
It enables the closed source capability of chromium, used by Amazon Prime Video and Netflix.
The `chrome://flags/#load-media-router-component-extension` has to be `enabled`.
With this configuration you can go to prime videos and cast the tab while playing.

In the long run a fire tv stick is a better fit, because it comes from the same house.

## setup system date

    # date 041513122017
    Sat Apr 15 13:12:00 CEST 2017

## xclip

    cat id_rsa.pub | xclip -selection clipboard

## brightness

    $ sudo echo 800 > /sys/class/backlight/intel_backlight/brightness

sets the current brightness to `800`. (1500 is default)

## gcalcli

Working with the Google calendar on console

put a `.gcalclirc` in users home directory

    --client_id=<client_id>
    --client_secret=<client-secret>
    --locale=de_DE.utf8
    --defaultCalendar=<calendar name>

Set up some convenient aliases

    alias week="gcalcli calw"
    alias month="gcalcli calm"
    alias todo="gcalcli agenda"

Get the next three weeks

    week 3
