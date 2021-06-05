# tcsh configuration file.
set symlinks = ignore       # Don't be confused when following links

# History:
set history = 2000          # Should be enough for the time being
set savehist = (2000 merge) # Save and merge with existing saved 
set histfile = ~/.tcsh_history
set histlit                 # Enable the literal use of "!" in strings
set histdup = prev          # Keep the history free of duplicates

# Command input:
set autorehash              # Always find new commands immediately
set backslash_quote         # Enable quoting with a backslash
set globdot                 # Wildcards shall match .<something>
set globstar                # Double wildcards will match in subdirs

# UTF-8:
setenv LANG de_DE.UTF-8
setenv LC_ALL de_DE.UTF-8
setenv LC_CTYPE de_DE.UTF-8

# Completion:
set autoexpand              # Tab completion expansion
set autolist                # Tab completion list
set complete = enhance      # Case-insensitivity for the latter
complete cd 'C/*/d/'        # "cd" should only expand directories
complete chown 'p/1/u/'     # "chown" should only expand users
complete which 'p/1/c/'     # "which" should only expand commands
complete where 'p/1/c/'     # ... so should "where"

# Key bindings:
bindkey "^R" i-search-back

# Import paths etc.:
source $HOME/.cshenv

# Directory-local environment variables:
# (Requires direnv.)
if (`where direnv` != "") then
  eval `direnv hook tcsh`
endif

# Starship:
# (Requires starship.)
if (`where starship` != "") then
  eval "`starship init tcsh`"
endif

# zoxide:
# (Requires zoxide and zoxide.tcsh.)
if (`where zoxide` != "") then
  source $HOME/zoxide.tcsh
  alias precmd '__zoxide_hook;set prompt = "[%n@%m:%~]%# "'
else
  set prompt = '[%n@%m:%~]%# '
endif
