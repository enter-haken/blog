---
title: Cookbook
---

This is my loose collection of snippets, I use from day to day.

### git 

pull all repositories located in sub directories, assuming there are only subdirectories in the current directory

```bash
#!/bin/bash
ls | xargs -I{} git -C {} pull
```

### vim

some vim stuff

#### movement

* `k` - up
* `j` - down
* `h` - left 
* `l` - right

#### visual

* `iv'` - select inner text bettween two `'` 
* `<C-V>` - select column
    * `I` - insert text -> `<ESC>` -> insert text in front of the selection
