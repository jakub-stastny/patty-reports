save-function-list

load ~/.zsh/environments/basic.zsh

alias emacs="emacs --load $PWD/.env/emacs.el"

function report() {
  local curl_command
  curl_command=$(clj -M:report $1 $2)

  echo "$curl_command" | while IFS= read -r line; do
    echo -e "\033[0;36m$ $line\033[0m"
    # Can still be piped into jq for pretty-printed, colour-highlighted JSON.
    eval "$line"
    echo "\n"
  done
}

report-custom-functions
