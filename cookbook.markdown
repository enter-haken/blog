---
title: Cookbook
---

This is my loose collection of snippets, I use from day to day.

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
* 'w' - beginning of a world
* ')' - forward one sentence
* '(' - backward one sentence

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
