#!/bin/sh

COMMON_API_DIR=_build/src/common/prob_cache_common.docdir
CONTAINERS_API_DIR=_build/src/containers/prob_cache_containers.docdir
RIAK_API_DIR=_build/src/riak/prob_cache_riak.docdir

git clone `git config --get remote.origin.url` .gh-pages --reference .
git -C .gh-pages checkout --orphan gh-pages
git -C .gh-pages reset
git -C .gh-pages clean -dxf

mkdir -p .gh-pages/prob_cache_common
cp -r -t .gh-pages/prob_cache_common $COMMON_API_DIR/*

mkdir -p .gh-pages/prob_cache_containers
cp -r -t .gh-pages/prob_cache_containers $CONTAINERS_API_DIR/*

mkdir -p .gh-pages/prob_cache_riak
cp -r -t .gh-pages/prob_cache_riak $RIAK_API_DIR/*

git -C .gh-pages add .
git -C .gh-pages commit -m "Update Pages"
git -C .gh-pages push origin gh-pages -f
rm -rf .gh-pages

