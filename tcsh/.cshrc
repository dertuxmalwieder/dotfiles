# tcsh settings:
set history = 2000          # Should be enough for the time being
set savehist = (2000 merge) # Save and merge with existing saved 
set histfile = ~/.tcsh_history
set prompt = '[%n@%m:%~]%# '
set autolist                # Tab completion list
set complete = enhance      # Case-insensitivity for the latter

bindkey "^R" i-search-back

# Import paths etc.:
source $HOME/.cshenv
