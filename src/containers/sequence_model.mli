(** 
 * A cache for sequence oriented probability models 
 * The event type is polymorphic but depends on order semantics.
 * Use [@@deriving ord] to easily support this with your own types.
 *
 * Suppose we observe a sequence of events e_0, e_1, e_2, e_3
 * We now want to now the likelihoods of future observations e_4, e_5 or 
 * P(e_4,e_5|e_0, e_1, e_2, e_3) 
 * 
 * More concretely:
   * If we observe RED | GREEN | GREEN | BLUE | RED | GREEN
   * then our universe is the following
   RED - > 2
   RED | GREEN -> 2
   GREEN -> 3
   GREEN | GREEN -> 1
   RED | GREEN | GREEN -> 1
   BLUE -> 1
   GREEN | BLUE -> 1
   GREEN | GREEN | BLUE -> 1
   RED | GREEN | GREEN | BLUE -> 1
   BLUE | RED -> 1
   BLUE | RED | GREEN - > 1
   RED | GREEN | GREEN | BLUE | RED | GREEN -> 1
   GREEN | GREEN | BLUE | RED | GREEN -> 1
   GREEN | GREEN | BLUE | RED -> 1
   GREEN | BLUE | RED -> 1

   Complexity Overview:

   1) Total number of sequences we have to update given a new sequence of length of L is O(L),
      A frequency count is maintained for each subsequence of which there are L of them.
   2) If we cache each sequence independently we would get E^L + E^(L-1) + E^(L-2) .. entries,
      eg. O(E^L) in the worst case (E is cardinality of the event type). Ideally we would 
      observe much less than that, but this is also not the most efficient 
      representation, either.

      We can at least a little more efficiently encode the data by using a graphical representation.
      For instance, the observation RED | GREEN | RED results in this graphical encoding:
        [(RED, 2, [(GREEN, 1, [(RED, 1, [])])]);
        (GREEN, 1, [(RED, 1, [])])]
      The benefit is the key for GREEN | RED and is composed of the KEY for GREEN, which saves space referencing GREEN directly.
      While the complexity doesn't theoretically change much, in practice this can be a big win. Currently NOT implemented.
**)

(** The event type for each element of a sequence. *)
module type EVENT = Model_intf.EVENT

(** The type holding a sequence of events *)
module type EVENTS = Model_intf.EVENTS

(** Represents a polymoprhic cache of sequences mapped to probabilities and expectations *)
module type S = Model_intf.S

(** Creates a concrete instance of a sequence model cache for a given Event type *)
module Make : module type of Model_intf.Make_sequence

