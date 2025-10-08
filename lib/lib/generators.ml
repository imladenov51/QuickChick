open Main
open Producer
open Monad

module Generators = struct
  type 'a genType = int -> RandomQC.randomSeed -> 'a
  type 'a g = 'a genType

  let mk_genType (x : 'a genType) = x
  let run g0 = g0
  let returnGen x _ _ = x

  let bindGen g0 k n0 r =
    let r1, r2 = RandomQC.randomSplit r in
    run (k (run g0 n0 r1)) n0 r2

  let monadGen = Build_Monad ((fun _ -> returnGen), fun _ _ -> bindGen)

  let rec createRange n0 acc =
    (fun fO fS n -> if n = 0 then fO () else fS (n - 1))
      (fun _ -> Lists.rev (0 :: acc))
      (fun n' -> createRange n' (n0 :: acc))
      n0

  let rec rnds s n' =
    (fun fO fS n -> if n = 0 then fO () else fS (n - 1))
      (fun _ -> [])
      (fun n'' ->
        let s1, s2 = RandomQC.randomSplit s in
        s1 :: rnds s2 n'')
      n'

  let sampleGen g0 =
    let l = Lists.combine (rnds RandomQC.newRandomSeed 20) (createRange 10 []) in
    Lists.map
      (fun p ->
        let r, n0 = p in
        g0 n0 r)
      l

  let sizedGen f0 n0 r = run (f0 n0) n0 r
  let resizeGen n0 g0 _ = g0 n0

  let chooseGen (h : 'a1 RandomQC.choosableFromInterval) range _ r =
    fst ((RandomQC.randomR h) range r)

  let producerGen =
    Build_Producer
      ( monadGen,
        (fun _ -> sampleGen),
        (fun _ -> sizedGen),
        (fun _ -> resizeGen),
        (fun _ _ -> chooseGen),
        fun _ _ g0 k n0 r ->
          let r1, r2 = RandomQC.randomSplit r in
          run (k (run g0 n0 r1) __) n0 r2 )

  let promote m n0 r = RoseTrees.fmapRose (fun g0 -> run g0 n0 r) m
end

include Generators
