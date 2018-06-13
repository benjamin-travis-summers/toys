alias publish=/Users/benjamin/proj/chains/publish.sh

pomo () {
  cd $HOME/proj/toys/script
  if [ $# -ne 2 ]
  then echo "usage: pomo minutes name"
  else pomodoro $1 "$2"
  fi
}

chains () {
  cd $HOME/proj/chains
  nix-shell --run ./browse-chains.sh
}
