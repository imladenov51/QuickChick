open State
open Coq_String
open Show
open Checker
open Lists
open RoseTrees
open Main
open RandomQC
open Generators

module Test = struct
  type args =
    | MkArgs of (RandomQC.randomSeed * int) option * int * int * int * int * bool * bool

  let replay = function MkArgs (replay, _, _, _, _, _, _) -> replay
  let maxSuccess = function MkArgs (_, maxSuccess, _, _, _, _, _) -> maxSuccess
  let maxDiscard = function MkArgs (_, _, maxDiscard, _, _, _, _) -> maxDiscard
  let maxShrinks = function MkArgs (_, _, _, maxShrinks, _, _, _) -> maxShrinks
  let maxSize = function MkArgs (_, _, _, _, maxSize, _, _) -> maxSize
  let chatty = function MkArgs (_, _, _, _, _, chatty, _) -> chatty
  let analysis = function MkArgs (_, _, _, _, _, _, analysis) -> analysis

  type result0 =
    | Success of int * int * (char list * int) list * char list
    | GaveUp of int * (char list * int) list * char list
    | Failure of
        int
        * int
        * int
        * RandomQC.randomSeed
        * int
        * char list
        * (char list * int) list
        * char list
    | NoExpectedFailure of int * (char list * int) list * char list

  let roundTo n0 m = Nat.div n0 m * m

  let computeSize'' maxSize_ maxSuccess_ n0 d =
    if
      (roundTo n0 maxSize_ + maxSize_ <= maxSuccess_ || maxSuccess_ <= n0)
      || Nat.modulo maxSuccess_ maxSize_ = 0
    then Stdlib.min (Nat.modulo n0 maxSize_ + Nat.div d 10) maxSize_
    else
      Stdlib.min
        (Nat.div
           (Nat.modulo n0 maxSize_ * maxSize_)
           (Nat.modulo maxSuccess_ maxSize_ + Nat.div d 10))
        maxSize_

  let computeSize' a n0 d = computeSize'' (maxSize a) (maxSuccess a) n0 d
  let at0 f0 s n0 d = if n0 = 0 && d = 0 then s else f0 n0 d

  let rec insertBy compare1 x l =
    match l with
    | [] -> x :: []
    | h :: t0 -> if compare1 x h then x :: l else h :: insertBy compare1 x t0

  let rec insSortBy compare1 = function
    | [] -> []
    | h :: t0 -> insertBy compare1 h (insSortBy compare1 t0)

  let summary (st : State.state) =
    let res = Coq_Map.fold (fun key elem acc -> (key, elem) :: acc) (labels st) [] in
    insSortBy (fun x y -> snd y <= snd x) res

  let doneTesting (st : State.state) =
    if expectedFailure st then
      Success
        ( numSuccessTests st + 1,
          numDiscardedTests st,
          summary st,
          if stDoAnalysis st then
            append
              [
                '"'; 'r'; 'e'; 's'; 'u'; 'l'; 't'; '"'; ':'; ' '; '"'; 's'; 'u'; 'c'; 'c';
                'e'; 's'; 's'; '"'; ','; ' '; '"'; 't'; 'e'; 's'; 't'; 's'; '"'; ':'; ' ';
              ]
              (append
                 (Show.show_nat (numSuccessTests st))
                 (append
                    [
                      ','; ' '; '"'; 'd'; 'i'; 's'; 'c'; 'a'; 'r'; 'd'; 's'; '"'; ':'; ' ';
                    ]
                    (Show.show_nat (numDiscardedTests st))))
          else
            append
              [ '+'; '+'; '+'; ' '; 'P'; 'a'; 's'; 's'; 'e'; 'd'; ' ' ]
              (append
                 (Show.show_nat (numSuccessTests st))
                 (append
                    [ ' '; 't'; 'e'; 's'; 't'; 's'; ' '; '(' ]
                    (append
                       (Show.show_nat (numDiscardedTests st))
                       (append
                          [ ' '; 'd'; 'i'; 's'; 'c'; 'a'; 'r'; 'd'; 's'; ')' ]
                          Show.newline)))) )
    else
      NoExpectedFailure
        ( numSuccessTests st,
          summary st,
          if stDoAnalysis st then
            append
              [
                '"'; 'r'; 'e'; 's'; 'u'; 'l'; 't'; '"'; ':'; ' '; '"'; 'e'; 'x'; 'p'; 'e';
                'c'; 't'; 'e'; 'd'; '_'; 'f'; 'a'; 'i'; 'l'; 'u'; 'r'; 'e'; '"'; ','; ' ';
                '"'; 't'; 'e'; 's'; 't'; 's'; '"'; ':'; ' ';
              ]
              (Show.show_nat (numSuccessTests st))
          else
            append
              [
                '*'; '*'; '*'; ' '; 'F'; 'a'; 'i'; 'l'; 'e'; 'd'; '!'; ' '; 'P'; 'a'; 's';
                's'; 'e'; 'd'; ' ';
              ]
              (append
                 (Show.show_nat (numSuccessTests st))
                 (append
                    [
                      ' '; 't'; 'e'; 's'; 't'; 's'; ' '; '('; 'e'; 'x'; 'p'; 'e'; 'c';
                      't'; 'e'; 'd'; ' '; 'F'; 'a'; 'i'; 'l'; 'u'; 'r'; 'e'; ')';
                    ]
                    Show.newline)) )

  let giveUp st =
    GaveUp
      ( numSuccessTests st,
        summary st,
        if stDoAnalysis st then
          append
            [
              '"'; 'r'; 'e'; 's'; 'u'; 'l'; 't'; '"'; ':'; ' '; '"'; 'g'; 'a'; 'v'; 'e';
              '_'; 'u'; 'p'; '"'; ','; ' '; '"'; 't'; 'e'; 's'; 't'; 's'; '"'; ':';
            ]
            (append
               (show_nat (numSuccessTests st))
               (append
                  [ ','; ' '; '"'; 'd'; 'i'; 's'; 'c'; 'a'; 'r'; 'd'; 's'; '"'; ':'; ' ' ]
                  (show_nat (numDiscardedTests st))))
        else
          append
            [
              '*'; '*'; '*'; ' '; 'G'; 'a'; 'v'; 'e'; ' '; 'u'; 'p'; '!'; ' '; 'P'; 'a';
              's'; 's'; 'e'; 'd'; ' '; 'o'; 'n'; 'l'; 'y'; ' ';
            ]
            (append
               (show_nat (numSuccessTests st))
               (append
                  [ ' '; 't'; 'e'; 's'; 't'; 's' ]
                  (append newline
                     (append
                        [ 'D'; 'i'; 's'; 'c'; 'a'; 'r'; 'd'; 'e'; 'd'; ':'; ' ' ]
                        (append (show_nat (numDiscardedTests st)) newline))))) )

  let callbackPostTest st res =
    let (MkResult (o, e, r, i, s, c, t0)) = res in
    fold_left
      (fun acc callback0 ->
        match callback0 with
        | PostTest (_, call) -> call st (MkSmallResult (o, e, r, i, s, t0)) + acc
        | PostFinalFailure (_, _) -> acc)
      c 0

  let callbackPostFinalFailure st res =
    let (MkResult (o, e, r, i, s, c, t0)) = res in
    fold_left
      (fun acc callback0 ->
        match callback0 with
        | PostTest (_, _) -> acc
        | PostFinalFailure (_, call) -> call st (MkSmallResult (o, e, r, i, s, t0)) + acc)
      c 0

  let rec localMin st = function
    | MkRose (res, ts) ->
        let rec localMin' st0 = function
          | [] ->
              let zero = callbackPostFinalFailure st0 res in
              (numSuccessShrinks st0 + zero, res)
          | r' :: ts' -> (
              let (MkRose (res', _)) = r' in
              let zero = callbackPostTest st0 res in
              match ok res' with
              | Some x ->
                  let consistent_tags =
                    match result_tag res with
                    | Some t1 -> (
                        match result_tag res' with
                        | Some t2 -> if string_dec t1 t2 then true else false
                        | None -> false)
                    | None -> (
                        match result_tag res' with Some _ -> false | None -> true)
                  in
                  if negb x && consistent_tags then
                    localMin
                      (updSuccessShrinks st0 (fun x0 -> x0 + Stdlib.Int.succ 0 + zero))
                      r'
                  else
                    localMin' (updTryShrinks st0 (fun x0 -> x0 + Stdlib.Int.succ 0)) ts'
              | None -> localMin' (updTryShrinks st0 (fun x -> x + Stdlib.Int.succ 0)) ts'
              )
        in
        localMin' st (Lazy.force ts)

  let rec runATest st f0 maxSteps =
    (fun fO fS n -> if n = 0 then fO () else fS (n - 1))
      (fun _ -> giveUp st)
      (fun maxSteps' ->
        let size = (computeSize st) (numSuccessTests st) (numDiscardedTests st) in
        let rnd1, rnd2 = randomSplit (randomSeed0 st) in
        let test0 =
         fun st0 ->
          if numSuccessTests st0 >= maxSuccessTests st0 then doneTesting st0
          else if numDiscardedTests st0 >= maxDiscardedTests st0 then giveUp st0
          else runATest st0 f0 maxSteps'
        in
        let (MkState (mst, mdt, ms, cs, nst, ndt, ls, _, r, nss, nts, ana)) = st in
        let (MkRose (res, ts)) = f0 size rnd1 in
        let res_cb = callbackPostTest st res in
        let (MkResult (ok0, e, reas, _, s, _, t0)) = res in
        match ok0 with
        | Some x ->
            if x then
              let ls' =
                match s with
                | [] -> ls
                | _ :: _ -> (
                    let s_to_add =
                      ShowFunctions.string_concat
                        (ShowFunctions.intersperse [ ' '; ','; ' ' ] s)
                    in
                    match Coq_Map.find s_to_add ls with
                    | Some k -> Coq_Map.add s_to_add (k + Stdlib.Int.succ 0) ls
                    | None -> Coq_Map.add s_to_add (res_cb + Stdlib.Int.succ 0) ls)
              in
              test0
                (MkState (mst, mdt, ms, cs, nst + 1, ndt, ls', e, rnd2, nss, nts, ana))
            else
              let tag_text =
                match t0 with
                | Some s0 -> append [ 'T'; 'a'; 'g'; ':'; ' ' ] (append s0 nl)
                | None -> []
              in
              let pre =
                if ana then
                  if expect res then
                    [
                      '"'; 'r'; 'e'; 's'; 'u'; 'l'; 't'; '"'; ':'; ' '; '"'; 'f'; 'a';
                      'i'; 'l'; 'e'; 'd'; '"'; ','; ' ';
                    ]
                  else
                    [
                      '"'; 'r'; 'e'; 's'; 'u'; 'l'; 't'; '"'; ':'; ' '; '"'; 'e'; 'x';
                      'p'; 'e'; 'c'; 't'; 'e'; 'd'; '_'; 'f'; 'a'; 'i'; 'l'; 'u'; 'r';
                      'e'; '"'; ' ';
                    ]
                else if expect res then
                  [ '*'; '*'; '*'; ' '; 'F'; 'a'; 'i'; 'l'; 'e'; 'd'; ' ' ]
                else
                  [
                    '+'; '+'; '+'; ' '; 'F'; 'a'; 'i'; 'l'; 'e'; 'd'; ' '; '('; 'a'; 's';
                    ' '; 'e'; 'x'; 'p'; 'e'; 'c'; 't'; 'e'; 'd'; ')'; ' ';
                  ]
              in
              let numShrinks, _ = localMin st (MkRose (res, ts)) in
              let suf =
                if ana then
                  append
                    [ '"'; 't'; 'e'; 's'; 't'; 's'; '"'; ':'; ' ' ]
                    (append
                       (show_nat (Stdlib.Int.succ nst))
                       (append
                          [
                            ','; ' '; '"'; 's'; 'h'; 'r'; 'i'; 'n'; 'k'; 's'; '"'; ':';
                            ' ';
                          ]
                          (append (show_nat numShrinks)
                             (append
                                [
                                  ','; ' '; '"'; 'd'; 'i'; 's'; 'c'; 'a'; 'r'; 'd'; 's';
                                  '"'; ':'; ' ';
                                ]
                                (show_nat ndt)))))
                else
                  append
                    [ 'a'; 'f'; 't'; 'e'; 'r'; ' ' ]
                    (append
                       (show_nat (Stdlib.Int.succ nst))
                       (append
                          [ ' '; 't'; 'e'; 's'; 't'; 's'; ' '; 'a'; 'n'; 'd'; ' ' ]
                          (append (show_nat numShrinks)
                             (append
                                [ ' '; 's'; 'h'; 'r'; 'i'; 'n'; 'k'; 's'; '.'; ' '; '(' ]
                                (append (show_nat ndt)
                                   [ ' '; 'd'; 'i'; 's'; 'c'; 'a'; 'r'; 'd'; 's'; ')' ])))))
              in
              if negb (expect res) then
                Success
                  ( nst + Stdlib.Int.succ 0,
                    ndt,
                    summary st,
                    append tag_text (append pre suf) )
              else
                Failure
                  ( nst + Stdlib.Int.succ 0,
                    numShrinks,
                    ndt,
                    r,
                    size,
                    append tag_text (append pre suf),
                    summary st,
                    reas )
        | None ->
            let ls' =
              match s with
              | [] -> ls
              | _ :: _ -> (
                  let s_to_add =
                    append
                      [ '('; 'D'; 'i'; 's'; 'c'; 'a'; 'r'; 'd'; 'e'; 'd'; ')'; ' ' ]
                      (ShowFunctions.string_concat
                         (ShowFunctions.intersperse [ ' '; ','; ' ' ] s))
                  in
                  match Coq_Map.find s_to_add ls with
                  | Some k -> Coq_Map.add s_to_add (k + Stdlib.Int.succ 0) ls
                  | None -> Coq_Map.add s_to_add (res_cb + Stdlib.Int.succ 0) ls)
            in
            test0
              (MkState
                 (mst, mdt, ms, cs, nst, Stdlib.Int.succ ndt, ls', e, rnd2, nss, nts, ana)))
      maxSteps

  let test st f0 =
    if numSuccessTests st >= maxSuccessTests st then doneTesting st
    else if numDiscardedTests st >= maxDiscardedTests st then giveUp st
    else
      let maxSteps = maxSuccessTests st + maxDiscardedTests st in
      runATest st f0 maxSteps

  let quickCheckWith x a p =
    match replay a with
    | Some p0 ->
        let rnd, s = p0 in
        let computeFun = at0 (computeSize' a) s in
        test
          (MkState
             ( maxSuccess a,
               maxDiscard a,
               maxShrinks a,
               computeFun,
               0,
               0,
               Coq_Map.empty,
               false,
               rnd,
               0,
               0,
               analysis a ))
          (run (x p))
    | None ->
        let computeFun = computeSize' a in
        test
          (MkState
             ( maxSuccess a,
               maxDiscard a,
               maxShrinks a,
               computeFun,
               0,
               0,
               Coq_Map.empty,
               false,
               newRandomSeed,
               0,
               0,
               analysis a ))
          (run (x p))

  let rec showCollectStatistics = function
    | [] -> []
    | p :: l' ->
        let s, n0 = p in
        append (show_nat n0)
          (append [ ' '; ':'; ' ' ]
             (append s (append newline (showCollectStatistics l'))))

  let showResult = function
    | Success (_, _, l, s) -> append (showCollectStatistics l) s
    | GaveUp (_, l, s) -> append (showCollectStatistics l) s
    | Failure (_, _, _, _, _, s, l, _) -> append (showCollectStatistics l) s
    | NoExpectedFailure (_, l, s) -> append (showCollectStatistics l) s
end

include Test
