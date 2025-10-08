open Main

module Functor = struct
  type 'f functor0 = __ -> __ -> (__ -> __) -> 'f -> 'f

  let fmap functor1 x x0 = Obj.magic functor1 __ __ x x0
end

include Functor
