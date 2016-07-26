module Std =
  struct
    include Or_errors.Std
    include Model_primitives
    module type EVENTS = Events_common.EVENTS
    module type EVENT = Events_common.EVENT
    module type DATA = Data.S
    module type CREATE_FUN = Create_fun.S
    module type FOLD_FUN = Fold_fun.S
    module type DATA_FUN = Data_fun.S
    module type OBSERVE_DATA_FUN = Observe_data_fun.S
    module type S_KERNEL = Model_kernel.S
    module Decorator = Model_decorator
    module type S = Decorator.S
  end
