module State = struct
  type state =
    | MkState of
        int
        * int
        * int
        * (int -> int -> int)
        * int
        * int
        * int Coq_Map.t
        * bool
        * RandomQC.randomSeed
        * int
        * int
        * bool

  let maxSuccessTests = function
    | MkState (maxSuccessTests, _, _, _, _, _, _, _, _, _, _, _) -> maxSuccessTests

  let maxDiscardedTests = function
    | MkState (_, maxDiscardedTests, _, _, _, _, _, _, _, _, _, _) -> maxDiscardedTests

  let maxShrinkNo = function
    | MkState (_, _, maxShrinkNo, _, _, _, _, _, _, _, _, _) -> maxShrinkNo

  let computeSize = function
    | MkState (_, _, _, computeSize, _, _, _, _, _, _, _, _) -> computeSize

  let numSuccessTests = function
    | MkState (_, _, _, _, numSuccessTests, _, _, _, _, _, _, _) -> numSuccessTests

  let numDiscardedTests = function
    | MkState (_, _, _, _, _, numDiscardedTests, _, _, _, _, _, _) -> numDiscardedTests

  let labels = function MkState (_, _, _, _, _, _, labels, _, _, _, _, _) -> labels

  let expectedFailure = function
    | MkState (_, _, _, _, _, _, _, expectedFailure, _, _, _, _) -> expectedFailure

  let randomSeed0 = function
    | MkState (_, _, _, _, _, _, _, _, randomSeed0, _, _, _) -> randomSeed0

  let numSuccessShrinks = function
    | MkState (_, _, _, _, _, _, _, _, _, numSuccessShrinks, _, _) -> numSuccessShrinks

  let numTryShrinks = function
    | MkState (_, _, _, _, _, _, _, _, _, _, numTryShrinks, _) -> numTryShrinks

  let stDoAnalysis = function
    | MkState (_, _, _, _, _, _, _, _, _, _, _, stDoAnalysis) -> stDoAnalysis

  let updTryShrinks st f0 =
    let (MkState (mst, mdt, ms, cs, nst, ndt, ls, e, r, nss, nts, ana)) = st in
    MkState (mst, mdt, ms, cs, nst, ndt, ls, e, r, nss, f0 nts, ana)

  let updSuccessShrinks st f0 =
    let (MkState (mst, mdt, ms, cs, nst, ndt, ls, e, r, nss, nts, ana)) = st in
    MkState (mst, mdt, ms, cs, nst, ndt, ls, e, r, f0 nss, nts, ana)
end

include State
