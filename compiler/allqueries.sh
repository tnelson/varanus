#!/bin/bash
set -e
for file in `ls -1 queries/*.sq`; do  
  basefile=`basename $file .sq`
  echo "Compiling $basefile with dynamic flag"
  scala edu.brown.simon.SimonRunner -dynamic -o rules/$basefile.txt queries/$basefile.sq 
done