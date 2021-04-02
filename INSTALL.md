Put src/_zcomp somewhere where it will be read by the shell, preferably *before*
all your other completions, as it redefines `complete`.  Typically that might be in
`/etc/bash_completion.d/` or `$HOME/.bash_completion.d/`, depending on your distro.
