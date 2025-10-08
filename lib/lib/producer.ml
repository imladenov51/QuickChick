open Main
open Monad

module Producer = struct
  type 'g producer =
    | Build_Producer of
        'g monad
        * (__ -> 'g -> __ list)
        * (__ -> (int -> 'g) -> 'g)
        * (__ -> int -> 'g -> 'g)
        * (__ -> __ -> __ RandomQC.choosableFromInterval -> __ * __ -> 'g)
        * (__ -> __ -> 'g -> (__ -> __ -> 'g) -> 'g)

  let super producer0 =
    let (Build_Producer (super, _, _, _, _, _)) = producer0 in
    super

  let sample producer0 x =
    let (Build_Producer (_, sample, _, _, _, _)) = producer0 in
    Obj.magic sample __ x

  let sized producer0 x =
    let (Build_Producer (_, _, sized, _, _, _)) = producer0 in
    sized __ x

  let resize producer0 x x0 =
    let (Build_Producer (_, _, _, resize, _, _)) = producer0 in
    resize __ x x0

  let choose producer0 h x =
    let (Build_Producer (_, _, _, _, choose, _)) = producer0 in
    Obj.magic choose __ __ h x

  let bindPf producer0 g x =
    let (Build_Producer (_, _, _, _, _, bindPf)) = producer0 in
    Obj.magic bindPf __ __ g x
end

include Producer
