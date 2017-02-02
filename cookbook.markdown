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

## show graph

```
git log --graph --oneline --all
```
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

some vim stuff

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
