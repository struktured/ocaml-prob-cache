#!/bin/sh
COMMON_API_DIR=prob_cache_common.docdir
CONTAINERS_API_DIR=prob_cache_containers.docdir
CACHE_API_DIR=prob_cache_riak.docdir

git clone `git config --get remote.origin.url` .gh-pages --reference .
git -C .gh-pages checkout --orphan gh-pages
git -C .gh-pages reset
git -C .gh-pages clean -dxf
cp -L -r -t .gh-pages/ $COMMON_API_DIR
cp -L -r -t .gh-pages/ $CONTAINERS_API_DIR
cp -L -r -t .gh-pages/ $CACHE_API_DIR
git -C .gh-pages add .
git -C .gh-pages commit -m "Update Pages"
git -C .gh-pages push origin gh-pages -f
rm -rf .gh-pages

