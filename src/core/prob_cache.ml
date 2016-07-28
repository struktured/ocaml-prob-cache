module Std =
struct
  include Or_errors.Std
  module Model_primitives = Prob_cache_model_primitives
  module Events = Prob_cache_events
  module Data = Prob_cache_data
  module Model_kernel = Prob_cache_model_kernel
  module Model_decorator = Prob_cache_model_decorator
  module Data_fun = Prob_cache_data_fun
  module Observe_data_fun = Prob_cache_observe_data_fun
  module Fold_fun = Prob_cache_fold_fun
  module Create_fun = Prob_cache_create_fun
  module Entry = Prob_cache_entry
  module Fold = Prob_cache_fold

  module type EVENTS = Events.EVENTS
  module type EVENT = Events.EVENT
  module type DATA = Data.S
  module type CREATE_FUN = Create_fun.S
  module type FOLD_FUN = Fold_fun.S
  module type DATA_FUN = Data_fun.S
  module type OBSERVE_DATA_FUN = Observe_data_fun.S
  module type MODEL_KERNEL = Model_kernel.S
  module type MODEL_DECORATOR = Model_decorator.S
  module Model = Prob_cache_model
  module Running = Data.Running
  module Update_rules = Prob_cache_update_rules
end
