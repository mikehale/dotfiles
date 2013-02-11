#!/bin/sh

set -e

git checkout master
git pull origin master
for branch in `git branch --merged | egrep -v '\*|v[0-9]+$' | tr -d ' '`; do
  git branch -d $branch
  git push origin :$branch
done
