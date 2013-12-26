#! /bin/bash
# ./knock hostname port1 port2 ...

knockfun() 
{
  local hostname="$1"
  shift

  echo -n "Knocking on $hostname ... "

  for ip in $@; do
    echo | nc "$hostname" "$ip" &>/dev/null
  done

  echo "DONE"
}

if [[ -z $1 ]]; then
  echo "Usage: ./knock hostname <ports>"
else
  knockfun "$@"
fi

