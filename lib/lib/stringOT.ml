open Main

module StringOT = struct
  type t = char list
  type sOrdering = SLT | SEQ | SGT

  let sOrdering_rect f0 f1 f2 = function SLT -> f0 | SEQ -> f1 | SGT -> f2
  let sOrdering_rec f0 f1 f2 = function SLT -> f0 | SEQ -> f1 | SGT -> f2

  let rec strcmp s1 s2 =
    match s1 with
    | [] -> ( match s2 with [] -> SEQ | _ :: _ -> SLT)
    | ch1 :: s1' -> (
        match s2 with
        | [] -> SGT
        | ch2 :: s2' -> (
            match AsciiOT.compare ch1 ch2 with
            | OrderedType.LT -> SLT
            | OrderedType.EQ -> strcmp s1' s2'
            | OrderedType.GT -> SGT))

  let rec compare s s2 =
    match s with
    | [] -> ( match s2 with [] -> OrderedType.EQ | _ :: _ -> OrderedType.LT)
    | a :: s0 -> (
        match s2 with
        | [] -> OrderedType.GT
        | a0 :: s1 -> (
            let c = AsciiOT.compare a a0 in
            match c with
            | OrderedType.LT -> OrderedType.LT
            | OrderedType.EQ -> internal_eq_rew_r_dep a a0 (fun _ -> compare s0 s1) __
            | OrderedType.GT -> OrderedType.GT))
end

include StringOT
