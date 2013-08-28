#! /bin/bash
#
# This file will take the names and test results from resultat.txt
#  and add their points together, sort them, and print them out
# They'll all be shell one-liners, but will use other programming languages

if [ -z $1 ]; then
    lang="unknown"
else
    lang=$1
fi

case $lang in 
    "perl")
        perl -ane 'printf "%d %s %s \n", $F[2]+$F[3]+$F[4], $F[0], $F[1]' resultat.txt | sort -r
        ;;
    "awk")
        #awk '{print$3+$4+$5,$1,$2}' resultat.txt|sort -r
        # This does not work in 'real' awk, but in gawk (but should work)
        awk '$0=$3+$4+$5FS$1FS$2' resultat.txt|sort -r
        ;;
    'bash')
        sed 's/\ -/0/g' resultat.txt|while read -a r;do echo $[r[2]+r[3]+r[4]] ${r[0]} ${r[1]};done|sort -r
        ;;
    *)
        echo "Usage: $0 <lang>"
        echo "Available languages:"
        echo "\tperl"
        echo "\tawk"
esac