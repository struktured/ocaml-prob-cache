# README #

OCaml Probability Cache Library
  
## Introduction ##

This library provides a polymorphic cache for maintaining random events to their observed probabilities and expectations. There are two types of supported probability models- a set model and sequence model, either of which can be used in process (with [containers](https://github.com/c-cube/containers)) or over a distributed riak cache (with [riakc_ppx](https://github.com/struktured/riakc_ppx)). 

## Dependencies ##

All of these are installable via opam.

 - [containers](https://github.com/c-cube/ocaml-containers) (for in process cache)
 - [sequence](https://github.com/c-cube/sequence) (supplement to containers)
 - [ppx_deriving](https://github.com/whitequark/ppx_deriving) (for implementing show and comparable)
 - [ppx_deriving_protobuf](https://github.com/whitequark/ppx_deriving_protobuf) (for distributed cache)
 - [riakc_ppx](https://github.com/struktured/riakc_ppx) (for distributed cache)
 - [async](https://github.com/janestreet/async) (to interact with riak cache)
 - [oasis](https://github.com/ocaml/oasis) (for compiling)

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
# Documentation

The API documentation is published on github pages.

 * [common library](https://struktured.github.io/ocaml-prob-cache/prob_cache_common) 
 * [containers library](https://struktured.github.io/ocaml-prob-cache/prob_cache_containers)
 * [riak library](https://struktured.github.io/ocaml-prob-cache/prob_cache_riak)

# Examples

### Containers Cache

#### Set Model
```OCaml
(* A toy event model where it can be raining, a water sprinkler may be on, and the ground may be wet 
 * due to one, both, or none of these events *)
module Event = struct type t = IS_RAINING | SPRINKLER_ON | GROUND_IS_WET [@@deriving show, ord] end
module Model = Prob_cache_containers.Set_model.Make(Event)

open Event
let raining = Model.Events.of_list [IS_RAINING]
let sprinkler_on = Model.Events.of_list [SPRINKLER_ON]
let ground_wet_raining = Model.Events.of_list [IS_RAINING; GROUND_IS_WET]
let ground_wet_sprinkler_on = Model.Events.of_list [SPRINKLER_ON; GROUND_IS_WET]
let ground_wet = Model.Events.of_list [GROUND_IS_WET] 
let ground_not_wet_sprinkler_on = Model.Events.of_list [SPRINKLER_ON] 

let m = Model.create "toy-model" |>
  Model.observe ground_wet_raining |>
  Model.observe ground_wet_sprinkler_on |>
  Model.observe ground_wet |>
  Model.observe ground_not_wet_sprinkler_on
  
let ground_wet_given_raining = Model.prob ~cond:raining ground_wet m (* returns 1 *)
let ground_wet_given_sprinkler_on = Model.prob ~cond:sprinkler_on ground_wet m (* returns .5 *)
let ground_wet = Model.prob ground_wet m (* returns .75 *)
```

#### Sequence Model
```Ocaml
(* Models a sequence of coin flips, not necesssarily where each flip is independent *) 
module Coin = struct type t = HEADS | TAILS [@@deriving show, ord] end
module Model = Prob_cache_containers.Sequence_model.Make(Coin)

open Coin
let events = Model.Events.of_list [HEADS;TAILS;HEADS;TAILS] 
let heads = Model.Events.of_list [HEADS] 
let tails = Model.Events.of_list [TAILS] 
let heads_tails = Model.Events.of_list [HEADS;TAILS] 

let m = Model.create "coin-flips" |>
  Model.observe events
  
let cnt,exp = Model.count events m, Model.exp events m (* cnt=1, exp = 1. *)

let a = Model.prob heads m (* returns 1. *)
let b = Model.prob tails m (* returns 0. *)
let c = Model.prob heads_tails m (* return 1. *)
```


For a complete list of containers examples, see https://github.com/struktured/ocaml-prob-cache/tree/master/src/examples.

### Riak Cache

See https://github.com/struktured/ocaml-prob-cache/tree/master/src/riak_examples.

## Complexity ##

Both models are data driven, caching only what is observed, but are brute force in that they make no explicit attempt at efficient encodings or sparse representations. 

Per observation, the set model is exponential with respect to the number of observed events. It stores 2^N instances per observation containing N events. 

The sequence model is linear with respect to the sequence length. It stores N events for an observed sequence of length N.

## Contributing ##

 * Open an issue on github 
 * Contact struktured on \#ocaml freenode irc.

## Future work ##

 * Unit tests
 * Lwt support
 * Modules for other caching middlewares
 * Sparser representations
 * Distance metrics to estimate distributions on new observations
 * Possible integration with https://github.com/agarwal/future
 * GPU optimizations

