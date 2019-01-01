---
title: dev stack 
---

I am running my Gentoo system for a few years now.
This is my loose collection of hints and tricks.
These notes are work in progress.

<!--more-->

# Gentoo

## portage 

The primary package manager of Gentoo is called [portage][portage].
`portage` will keep track on the dependencies of each package installed.
Almost every package will be compiled from source.

## use flags

Within a Gentoo system, [use flags][use] can enable or disable package specific features.
The `app-portage/gentoolkit` contains some useful portage tools.
One of them is `equery` which can be used to inspect packages.

For example:

    $ equery u vim
    [ Legend : U - final flag setting for installation]
    [        : I - package is installed with flag     ]
    [ Colors : set, unset                             ]
     * Found these USE flags for app-editors/vim-8.1.0648:
     U I
     - - X                              : Link console vim against X11 libraries to enable title and clipboard
                                          features in xterm
     + + acl                            : Add support for Access Control Lists
     - - cscope                         : Enable cscope interface
     - - debug                          : Enable extra debug codepaths, like asserts and extra output. If you
                                          want to get meaningful backtraces see
                                          https://wiki.gentoo.org/wiki/Project:Quality_Assurance/Backtraces
     + + gpm                            : Add support for sys-libs/gpm (Console-based mouse driver)
     + + lua                            : Enable Lua scripting support
     ...
     + + python                         : Add optional support/bindings for the Python language
     ...
     - - racket                         : Enable support for Scheme using dev-scheme/racket
     + + ruby                           : Add support/bindings for the Ruby language
     + + tcl                            : Add support the Tcl language
     - - terminal                       : Enable terminal emulation support
     - - vim-pager                      : Install vimpager and vimmanpager links

If you like to enable `x` support for `vim` you could simply do

    USE="X" emerge -avq app-editors/vim

or you can simply add a file to `/etc/portage/package.use/` with the content

    app-editors/vim X

to enable the `X` feature for the next time `vim` is emerged.

If you like to have a feature globally enabled you can add use flags to the `/etc/portage/make.conf` file.

## profile

There are different [profiles][profile] present for a Gentoo system. 

With `app-admin/eselect` you can 

    $ eselect profile list
    Available profile symlink targets:
      ...
      [12]  default/linux/amd64/17.0 (stable)
      [13]  default/linux/amd64/17.0/selinux (stable)
      [14]  default/linux/amd64/17.0/hardened (stable)
      [15]  default/linux/amd64/17.0/hardened/selinux (stable)
      [16]  default/linux/amd64/17.0/desktop (stable) *
      [17]  default/linux/amd64/17.0/desktop/gnome (stable)
      [18]  default/linux/amd64/17.0/desktop/gnome/systemd (stable)
      [19]  default/linux/amd64/17.0/desktop/plasma (stable)
      [20]  default/linux/amd64/17.0/desktop/plasma/systemd (stable)
      [21]  default/linux/amd64/17.0/developer (stable)
      [22]  default/linux/amd64/17.0/no-multilib (stable)
      [23]  default/linux/amd64/17.0/no-multilib/hardened (stable)
      [24]  default/linux/amd64/17.0/no-multilib/hardened/selinux (stable)
      [25]  default/linux/amd64/17.0/systemd (stable)
      [26]  default/linux/amd64/17.0/x32 (dev)
      ...

choose a suitable profile.

For a minimal desktop system the profile `13` is suitable.

A system update after a profile change

    emerge --sync && emerge -uNDqv world

can take several hours of compile time.

# beyond base system 

After having a reboot consistent system, more packages can be installed


## portage tools

    app-portage/eix
    app-portage/genlop
    app-portage/gentoolkit

First of all, some portage tools should be installed.

`app-portage/eix` has some [features][eix], including searching the portage tree.

If you want to estimate a package upgrade time, `app-portage/genlop` is your friend.
A call to

    $ genlop -c

shows the current emerge time and the estimated time. 
Since the logs are parsed to retrieve the estimated time, the estimation only works for packages that have already been emerged.

The `app-portage/gentoolkit` package has some portage tools, you need to work with a Gentoo system.

A often used tool is `equery`. 
It helps you to get information about a Gentoo package.

    $ equery
    Gentoo package query tool
    Usage: equery [global-options] module-name [module-options]
    
    global options
     -h, --help              display this help message
     -q, --quiet             minimal output
     -C, --no-color          turn off colors
     -N, --no-pipe           turn off pipe detection
     -V, --version           display version info
    
    modules (short name)
     (b)elongs               list what package FILES belong to
     (c)hanges               list changelog entries for ATOM
     chec(k)                 verify checksums and timestamps for PKG
     (d)epends               list all packages directly depending on ATOM
     dep(g)raph              display a tree of all dependencies for PKG
     (f)iles                 list all files installed by PKG
     h(a)s                   list all packages for matching ENVIRONMENT data stored in
                             /var/db/pkg
     (h)asuse                list all packages that have USE flag
     ke(y)words              display keywords for specified PKG
     (l)ist                  list package matching PKG
     (m)eta                  display metadata about PKG
     (s)ize                  display total size of all files owned by PKG
     (u)ses                  display USE flags for PKG
     (w)hich                 print full path to ebuild for PKG

Because you use this tool so often, it is practical to create some aliases in your `.bashrc` 

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

## version control 

    dev-vcs/git
    dev-vcs/tig

Working with version control is a mandatory tasks for a software developer.
`git` is the most widely used version control software.
Although you can easily use `git` from command line, `tig` has a ncurses-based text-mode interface.
It's handy, to get a quick overview of a repository.

## vim

    app-editors/vim
  
It does not matter which editor you use, it should only suit you best.
In my case it is `vim`. 
You can see more about `vim` in my [notes][vim].
    
## system

    app-admin/sudo
    net-misc/dhcpcd
    net-wireless/wpa_supplicant
    net-misc/ntp
    sys-process/htop

There are some system specific packages, which slightly depends on your setup.
`sudo` let you execute commands as an other user. 
This is often used in conjunction with root actions.

`net-misc/dhcpcd` is a dhcp daemon, which should be started during boot, so that the network is up and running.

When you use a wireless network, `net-wireless/wpa_supplicant` can help you configure a wireless network.
The console version of the config tool is slightly more complicated than the X11 version.
See the [documentation][wpa_supplicant] for further information.

The `net-misc/ntp` daemon is not quiet necessary, but convenient.
It sets the system time, when it is necessary.

`sys-process/htop` is a process monitor, where you can also kill processes if necessary.


## terminal / bash

    app-misc/ranger
    app-text/tree
    sys-apps/the_silver_searcher
  
## language specific package managers 

    dev-python/pip
    net-libs/nodejs
    dev-haskell/cabal

There are some applications, which can be installed via other package managers.

They can be installed via

    $ pip install pgcli --user

or 

    $ npm install sass -g

or 

    $ cabal install hakyll

## x11

    app-admin/conky
    app-text/evince
    app-text/texlive
    mail-client/thunderbird-bin
    media-gfx/graphviz
    media-gfx/feh
    x11-apps/xsetroot
    x11-base/xorg-server
    x11-misc/dmenu
    x11-misc/xscreensaver
    x11-terms/xterm
    x11-wm/dwm

    www-client/firefox-bin
    www-client/google-chrome

    media-sound/alsa-utils

## kernel

    sys-apps/pciutils
    sys-apps/usbutils
    sys-kernel/genkernel
    sys-kernel/gentoo-sources
 
## mobile

    sys-fs/fuse
    sys-fs/simple-mtpfs 
    dev-db/postgresql
    net-libs/nodejs
    sys-fs/dosfstools

# package installation

## pgcli

An alternative way to query a database from command line

### config

The local config file can be found at`~/.config/pgcli/config`

Set 

    pager = cat

to get inline results.

[portage]: https://wiki.gentoo.org/wiki/Portage
[stage3]: https://wiki.gentoo.org/wiki/Stage_tarball
[use]: https://wiki.gentoo.org/wiki/Handbook:AMD64/Working/USE
[profile]: https://wiki.gentoo.org/wiki/Profile_(Portage)
[eix]: https://wiki.gentoo.org/wiki/Eix
[vim]: /notes/vim
[wpa_supplicant]: https://wiki.gentoo.org/wiki/Wpa_supplicant
