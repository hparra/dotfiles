echo "Sourcing dotfiles .bash_profile"

export EDITOR=emacs
export PAGER=less

export TERM=xterm-256color
ITERM_24BIT=1

# Prepend date to `history` command.
export HISTTIMEFORMAT="%Y-%m-%dT%H:%M:%SZ  "

#
# GIT
#

source $HOME/.dotfiles/scripts/.git-completion.bash

alias gcob='git checkout -b'
alias gs='git status'

alias gca='git commit -a'
alias gcam='git commit -a --amend'
alias gcamn='git commit -a --amend --no-edit'

alias gpoh='git push origin HEAD --tags'

alias gitk='gitk --all &'


# Interactive Rebase
# e.g. gri 10
gri () {
    re='^[0-9]+$'
    if [ -z $1 ]; then
				git rebase -i HEAD~1
    elif [[ $1 =~ $re ]]; then
				git rebase -i HEAD~$1
    else
				git rebase -i $1~1
    fi
}

