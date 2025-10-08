open Main

module Monad = struct
  type 'm monad = Build_Monad of (__ -> __ -> 'm) * (__ -> __ -> 'm -> (__ -> 'm) -> 'm)

  let ret monad0 x =
    let (Build_Monad (ret, _)) = monad0 in
    Obj.magic ret __ x

  let bind monad0 x x0 =
    let (Build_Monad (_, bind)) = monad0 in
    Obj.magic bind __ __ x x0

  let liftM m f0 x = bind m x (fun x0 -> ret m (f0 x0))
  let functor_Monad m _ _ = liftM m
end

include Monad
