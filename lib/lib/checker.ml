module Checker = struct
  type callbackKind = Counterexample | NotCounterexample

  type smallResult =
    | MkSmallResult of
        bool option * bool * char list * bool * char list list * char list option

  type callback =
    | PostTest of callbackKind * (State.state -> smallResult -> int)
    | PostFinalFailure of callbackKind * (State.state -> smallResult -> int)

  type result =
    | MkResult of
        bool option
        * bool
        * char list
        * bool
        * char list list
        * callback list
        * char list option

  let ok = function MkResult (ok, _, _, _, _, _, _) -> ok
  let expect = function MkResult (_, expect, _, _, _, _, _) -> expect
  let reason = function MkResult (_, _, reason, _, _, _, _) -> reason
  let interrupted = function MkResult (_, _, _, interrupted, _, _, _) -> interrupted
  let stamp = function MkResult (_, _, _, _, stamp, _, _) -> stamp
  let callbacks = function MkResult (_, _, _, _, _, callbacks, _) -> callbacks
  let result_tag = function MkResult (_, _, _, _, _, _, result_tag) -> result_tag
  let succeeded = MkResult (Some true, true, [], false, [], [], None)
  let failed = MkResult (Some false, true, [], false, [], [], None)

  let updReason r s' =
    let (MkResult (o, e, _, i, s, c, t0)) = r in
    MkResult (o, e, s', i, s, c, t0)

  let addCallback res c =
    let (MkResult (o, e, r, i, s, cs, t0)) = res in
    MkResult (o, e, r, i, s, c :: cs, t0)

  type qProp = result RoseTrees.rose
  type checker = qProp Generators.g
  type 'a checkable = 'a -> checker

  let mk_qProp (x : qProp) = x
  let mk_checkable (x : 'a checkable) = x

  let liftBool = function
    | true -> succeeded
    | false -> updReason failed [ 'F'; 'a'; 'l'; 's'; 'i'; 'f'; 'i'; 'a'; 'b'; 'l'; 'e' ]

  let mapProp x f0 prop =
    Functor.fmap (Monad.functor_Monad (Obj.magic Generators.monadGen)) f0 (x prop)

  let mapRoseResult = mapProp
  let mapTotalResult x f0 = mapRoseResult x (RoseTrees.fmapRose f0)
  let testResult r = Monad.ret (Obj.magic Generators.monadGen) (RoseTrees.returnRose r)
  let testBool b = testResult (liftBool b)
  let testChecker x = x

  let rec props' t0 n0 pf shrinker x =
    (fun fO fS n -> if n = 0 then fO () else fS (n - 1))
      (fun _ -> RoseTrees.MkRose (t0 (pf x), lazy []))
      (fun n' ->
        RoseTrees.MkRose
          (t0 (pf x), lazy (Lists.map (props' t0 n' pf shrinker) (shrinker x))))
      n0

  let props h pf shrinker x = props' h 1000 pf shrinker x

  let shrinking h shrinker x0 pf =
    Functor.fmap
      (Monad.functor_Monad (Obj.magic Generators.monadGen))
      (fun x -> RoseTrees.joinRose (RoseTrees.fmapRose (fun q -> q) x))
      (Obj.magic Generators.promote (props h pf shrinker x0))

  let callback0 h cb = mapTotalResult h (fun r -> addCallback r cb)

  let printTestCase h s p =
    callback0 h (PostFinalFailure (Counterexample, fun _ _ -> Show.trace s 0)) p

  let forAllShrink x h gen0 shrinker pf =
    Generators.bindGen gen0 (fun x0 ->
        shrinking testChecker shrinker x0 (fun x' ->
            printTestCase x (Coq_String.append (h x') Show.newline) (pf x')))

  let testProd h h0 h1 h3 f0 = forAllShrink testChecker h h0 h1 (fun x -> h3 x (f0 x))
end

include Checker
