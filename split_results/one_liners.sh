#! /bin/sh
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
        perl -ane 'printf "%d %s %s \n", $F[2]+$F[3]+$F[4],$F[0],$F[1]' resultat.txt|sort -r
        ;;
    "awk")
        #awk '{print$3+$4+$5,$1,$2}' resultat.txt|sort -r
        #awk '{print$3+$4+$5,$1,$2|"sort -r"}' resultat.txt

        # This needs gawk instead of awk, but that's kinda standard anyways
        #awk '$0=$3+$4+$5" "$1" "$2' resultat.txt|sort -r
        awk '$0=$3+$4+$5FS$1FS$2' resultat.txt|sort -r
        ;;
    *)
        echo "Usage: $0 <lang>"
        echo "Available languages:"
        echo "\tperl"
        echo "\tawk"
esac