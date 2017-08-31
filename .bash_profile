echo "Sourcing dotfiles .bash_profile"

export EDITOR=emacs
export PAGER=less

export TERM=xterm-256color
ITERM_24BIT=1

# Prepend date to `history` command.
export HISTTIMEFORMAT="%Y-%m-%dT%H:%M:%SZ  "


# GIT
alias gpoh='git push origin HEAD --tags'
alias gca='git commit -a'
alias gitk='gitk --all &'

gri () {
    re='^[0-9]+$'
    if [ -z $1 ]
    then
	git rebase -i HEAD~1
    elif [[ $1 =~ $re ]]
    then
	git rebase -i HEAD~$1
    else
	git rebase -i $1~1
    fi
}

