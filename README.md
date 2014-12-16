# README #

OCaml Probability Cache Library
  
## Introduction ##

This library provides a polymorphic cache for maintaining random events to their observed probabilities and expectations. There are two types of supported probability models- a set model and sequence model, either of which can be used in process (with [containers](https://github.com/c-cube/containers)) or over a distributed riak cache (with [riakc_ppx](https://github.com/struktured/riakc_ppx)). 

## Installation ##

### Using opam

To install simply run
```
opam install prob-cache
```
or you can pin this repository:
```
opam pin add prob-cache --dev-repo
```

## Building locally

The following will clone the repository, build locally, and then pin to opam:

```
git clone git@github.com:/struktured/ocaml-prob-cache.git
cd ocaml-prob-cache
oasis setup
make install
opam pin add prob-cache .
```

# Examples

### Containers Cache
```
module Coin = struct type t = HEADS | TAILS [@@deriving show, ord] end
module Model = Prob_cache_containers.Sequence_model.Make(Coin)


let events = Model.Events.of_list [HEADS;TAILS;HEADS;TAILS] 
let heads = Model.Events.of_list [HEADS] 
let tails = Model.Events.of_list [TAILS] 
let heads_tails = Model.Events.of_list [HEAD;TAILS] 

let m = Model.create "coin-flips" |>
  fun m -> Model.observe events m
  
(* Returns 1, 1. *)
let cnt,exp = Model.count events m, Model.exp events m

let a = Model.count heads m (* a = 1 *)
let b = Model.count tails m (* b = 0 *)
let c = Model.count heads_tails m (* c = 1 *)
```

### Riak Cache
```




```

## Complexity ##

Both models are brute force oriented in that they make no attempt at efficient encodings or sparse representations. 
The set model is exponential with respect to the number of observed events. It stores 2^N instances per observation containing N events. 

The sequence model is linear with respect to the number of observed events. It stores N events for an observed sequence of length N.


The sequence model stores
## Contributing ##

 * Open an issue on github 
 * Contact struktured on \#ocaml freenode irc.

## Future work ##

 * Unit tests
 * Lwt support
 * Modules for other caching middlewares
 * Sparser representations
 * Distance metrics to estimate distributions on new observations
 * Possible integration with kumbaya
 * GPU optimizations

