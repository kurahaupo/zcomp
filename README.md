# zcomp
menu-style completion for tab-completion

## Installation
### With the `bash-completion` package
Assuming you have the `bash-completion` package installed, it should simply be a
matter of installing `src/_zcomp.bash` in either your personal `~/.bash_completion.d`
or in the system `/etc/bash_completion.d`.

Adjust its name so that it's
1. legal for the framework you're using; and
2. loaded as early as possible.

The recommended installation path is `/etc/bash_completion.d/000_zcomp.bash`.

The reason it should be loaded early is because it defines a _function_ called `complete`
which will then be unwittingly called by all the other completion scripts. It will
still work if it's loaded later, but it will be slower, because it will also need to
replace all the existing completions.

### Stand-alone
Install as above, but then edit your personal `~/.bashrc` or the system `/etc/bash.bashrc`
and add a line to source it:
* ```bash
  . /etc/bash_completion.d/000_zcomp.bash
  ```
* ```bash
  . "$HOME"/.bash_completion.d/000_zcomp.bash
  ```
Again, you can put this anywhere in the file, but it will be faster if you put it before
any other completions are defined.

## Usage

![image](https://user-images.githubusercontent.com/197253/182519165-bb56466e-5432-4925-86a3-2fc26bd173f8.png)
