# tcsh settings:
set history = 2000          # Should be enough for the time being
set savehist = (2000 merge) # Save and merge with existing saved 
set histfile = ~/.tcsh_history
set histlit = 1             # Enable the literal use of "!" in strings
set prompt = '[%n@%m:%~]%# '
set backslash_quote         # Enable quoting with a backslash
set globdot                 # Wildcards shall match .<something>

# Completion:
set autorehash              # Always find new commands immediately
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
