---
title: Cookbook
---

This is my loose collection of snippets, I use from day to day.

# dot

[dot](https://en.wikipedia.org/wiki/DOT_(graph_description_language)) is a graph description language.
Assuming a image viewer like [feh](https://feh.finalrewind.org) is installed

```
echo 'digraph test { a -> b; b -> c; a -> c; }' | dot -Tpng | feh -

```

will show 

![](/images/dot_test.png)

# git
pull all repositories located in sub directories, assuming there are only subdirectories in the current directory

```bash
#!/bin/bash
ls | xargs -I{} git -C {} pull
```

# vim

some vim stuff

## movement

* `k` - up
* `j` - down
* `h` - left 
* `l` - right

* `0` - beginning of a line
* `$` - end of a line
* 'w' - beginning of a world
* ')' - forward one sentence
* '(' - backward one sentence


## visual

* `iv'` - select inner text between two `'` 
* `<C-V>` - select column
    * `I` - insert text -> `<ESC>` -> insert text in front of the selection
