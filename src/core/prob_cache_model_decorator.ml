open Or_errors.Std
open Prob_cache_events
module Data_fun = Prob_cache_data_fun
module Observe_data_fun = Prob_cache_observe_data_fun
module type DATA_FUN = Prob_cache_data_fun.S
module type OBSERVE_DATA_FUN = Observe_data_fun.S

module Model_kernel = Prob_cache_model_kernel
module type S =
sig
  include Prob_cache_model_kernel.S
  module Event = Events.Event
  type prob = ?cond:Events.t -> Events.t -> t -> float Or_error.t

  type exp = ?cond:Events.t -> Events.t -> t -> float Or_error.t

  type var = ?cond:Events.t -> Events.t -> t -> float Or_error.t

  type sum = ?cond:Events.t -> Events.t -> t -> float Or_error.t

  type max = ?cond:Events.t -> Events.t -> t -> float Or_error.t

  type min = ?cond:Events.t -> Events.t -> t -> float Or_error.t

  type last = ?cond:Events.t -> Events.t -> t -> float Or_error.t

  type observe = ?cnt: int -> ?exp:float -> Events.t -> t -> t Or_error.t

  val prob : prob
  (** Probability of events given [cond], possibly the empty events *)

  val exp : exp
  (** Expectation of events given [cond], possibly the empty events *)

  val var : var
  (** Statistical variance of events given [cond], possibly the empty events *)

  val sum : sum
  (** Aggregated sum of events given [cond], possibly the empty events *)

  val max : max
  (** Observed maximum of events given [cond], possibly the empty events *)

  val min : min
  (** Observed minimum of events given [cond], possibly the empty events *)

  val last : last
  (** Observed last value of events given [cond], possibly the empty events *)

  val observe : observe
  (* Observe a new data instance given [cond], possibly the empty events. *)


  module Sequences :
  sig
     module Sequence : module type of Sequence
     val scan : t ->
      state:'f ->
      init:'acc ->
      f:('f, 'acc) Fold.t ->
      'acc Sequence.t Or_error.t
  end

  (** Floating point style computations over the model *)
  module Floats :
  sig
    type 'f fold = ('f, float) Fold.t

    module Arrays :
    sig
      module Array : module type of Array
      val scan : t ->
        state:'f ->
        init:float ->
        f:('f fold) ->
        float array Or_error.t
    end

    module Vecs :
    sig
      module Vec : module type of Lacaml_D.Vec
      val scan : t ->
        state:'f ->
        init:float ->
        f:('f fold) ->
        Vec.t Or_error.t
    end

    (* TODO
    val to_matrix : t ->
      ?x_pred:'x_p predicate option ->
      ?y_pred:(x:Entry.t -> 'y_p predicate) option ->
      f:(x:Entry.t -> 'state fold) -> (* <- todo not right does not account for x *)
      Mat.t Or_error.t *)
    (* TODO
    val dot : t ->
      ?x_pred:'x_p predicate option ->
      x_f:'x_s fold ->
      ?y_pred:'y_p predicate option ->
      y_f:'y_s fold ->
      float Or_error.t *)
   end

end

module Make
  (Model_kernel : Model_kernel.S)
   : S with
    module Events = Model_kernel.Events and
    module Data = Model_kernel.Data and
    module Or_error = Model_kernel.Or_error
  =
struct
  include Model_kernel
  module Event = Events.Event
  type prob = ?cond:Events.t -> Events.t -> t -> float Or_error.t

  type exp = ?cond:Events.t -> Events.t -> t -> float Or_error.t

  type var = ?cond:Events.t -> Events.t -> t -> float Or_error.t

  type sum = ?cond:Events.t -> Events.t -> t -> float Or_error.t

  type max = ?cond:Events.t -> Events.t -> t -> float Or_error.t

  type min = ?cond:Events.t -> Events.t -> t -> float Or_error.t

  type last = ?cond:Events.t -> Events.t -> t -> float Or_error.t

  type observe = ?cnt: int -> ?exp:float -> Events.t -> t -> t Or_error.t

  let prob ?(cond:Events.t option) (events:Events.t) (t:t) = failwith("NYI")

  let _data_map ?(cond=Events.empty) events t f =
    Or_error.map ~f (data (Events.join cond events) t)

  let exp ?cond events t =
    _data_map ?cond events t Data.expect

   let var ?cond events t =
     _data_map ?cond events t Data.var

   let sum ?cond events t =
     _data_map ?cond events t Data.sum

   let max ?cond events t =
     _data_map ?cond events t Data.max

   let min ?cond events t =
     _data_map ?cond events t Data.min

   let last ?cond events t =
     _data_map ?cond events t Data.last

   let observe ?(cnt=1) ?(exp=1.0) events t =
     observe_data
      (Data.create ~cnt ~exp) events t

   module Sequences =
   struct
    module Sequence = Sequence
    let scan t
      ~(state:'f)
      ~(init:'acc) ~f =
      let f' ~state ((seq:'acc Sequence.t), (acc:'acc)) e =
        f ~state acc e |>
          Fold.Return.map ~f:(fun acc' ->
            let seq' = Sequence.append seq (Sequence.return acc') in
            seq', acc') in
      fold t ~state ~init:((Sequence.empty: 'acc Sequence.t), (init:'acc)) ~f:f'
      |> Or_error.map ~f:(fun (seq,_) -> seq)
    end

   module Floats =
   struct
    type 'f fold = ('f, float) Fold.t
    module Arrays =
    struct
      module Array = Array
      let scan t ~(state:'f) ~(init:'acc) ~f =
        Sequences.scan t ~state ~init ~f |>
        Or_error.map ~f:Sequence.to_array
    end

    module Vecs =
    struct
      module Vec = Lacaml_D.Vec
      let scan t ~(state:'f) ~(init:'acc) ~f =
        Arrays.scan t ~state ~init ~f |>
        Or_error.map ~f:Vec.of_array
    end
   end

end

