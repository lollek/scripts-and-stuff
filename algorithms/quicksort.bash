#! /bin/bash

quicksort()
{
  if (( ! ${#@} )); then
    return
  fi

  local pivot=$1
  local lower=()
  local mid=($1)
  local higher=()
  
  shift
  for arg in $@; do
    if [[ $arg < $pivot ]]; then
      lower[${#lower[@]}]=$arg
    elif [[ $arg > $pivot ]]; then
      higher[${#higher[@]}]=$arg
    else
      mid[${#mid[@]}]=$arg
    fi
  done

  quicksort ${lower[@]}
  echo -n "${mid[@]} "
  quicksort ${higher[@]}
}

quicksort $@
echo
