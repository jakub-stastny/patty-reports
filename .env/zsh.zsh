save-function-list

load ~/.zsh/environments/basic.zsh

alias emacs="emacs --load $PWD/.env/emacs.el"

function report () {
  curl http://localhost:8080/api/v1/reports/$1 --json @$2 #| jq
}

report-custom-functions
