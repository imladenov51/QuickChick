Set Implicit Arguments.

Require Import RandomQC.
Require Import Coq.Strings.String.

From Stdlib Require Import MSetAVL OrderedType ZArith Int.
From QuickChick Require Import StringOT.

Set Implicit Arguments.
Unset Strict Implicit.

Module Map. 
#[local] Open Scope lazy_bool_scope.
#[local] Notation int := Z_as_Int.t.

Section Elt.

Variable elt : Type.

Inductive bst {elt : Type} :=
  | Leaf : bst 
  | Node : bst -> StringOT.t -> elt -> bst -> int -> bst.
Arguments bst : clear implicits.

Definition t := bst.

Notation tt := (bst elt).

Definition empty : tt := Leaf.

Fixpoint find x m : option elt :=
   match m with
     |  Leaf => None
     |  Node l y d r _ => match StringOT.compare x y with
             | LT _ => find x l
             | EQ _ => Some d
             | GT _ => find x r
         end
   end.

Definition height (m : tt) : int :=
  match m with
  | Leaf => Z_as_Int._0
  | Node _ _ _ _ h => h
  end.

Definition create l x e r :=
   Node l x e r (Z.max (height l) (Z.add (height r) 1)).

Definition assert_false := create.
Definition bal l x d r :=
  let hl := height l in
  let hr := height r in
  if Z_as_Int.gt_le_dec hl (hr+2) then
    match l with
     | Leaf => assert_false l x d r
     | Node ll lx ld lr _ =>
       if Z_as_Int.ge_lt_dec (height ll) (height lr) then
         create ll lx ld (create lr x d r)
       else
         match lr with
          | Leaf => assert_false l x d r
          | Node lrl lrx lrd lrr _ =>
              create (create ll lx ld lrl) lrx lrd (create lrr x d r)
         end
    end
  else
    if Z_as_Int.gt_le_dec hr (hl+2) then
      match r with
       | Leaf => assert_false l x d r
       | Node rl rx rd rr _ =>
         if Z_as_Int.ge_lt_dec (height rr) (height rl) then
            create (create l x d rl) rx rd rr
         else
           match rl with
            | Leaf => assert_false l x d r
            | Node rll rlx rld rlr _ =>
                create (create l x d rll) rlx rld (create rlr rx rd rr)
           end
      end
    else
      create l x d r.

Fixpoint fold (A : Type) (f : StringOT.t -> elt -> A -> A) (m : tt) : A -> A :=
 fun a => match m with
  | Leaf => a
  | Node l x d r _ => fold f r (f x d (fold f l a))
 end.

Fixpoint add x d m :=
  match m with
   | Leaf => Node Leaf x d Leaf 1%Z
   | Node l y d' r h =>
      match StringOT.compare x y with
         | LT _ => bal (add x d l) y d' r
         | EQ _ => Node l y d r h
         | GT _ => bal l y d' (add x d r)
      end
  end.

End Elt.

End Map.

Record State := MkState
  { maxSuccessTests   : nat
  ; maxDiscardedTests : nat
  ; maxShrinkNo       : nat
  ; computeSize       : nat -> nat -> nat

  ; numSuccessTests   : nat
  ; numDiscardedTests : nat

  ; labels            : Map.t nat

  ; expectedFailure   : bool
  ; randomSeed        : RandomSeed

  ; numSuccessShrinks : nat
  ; numTryShrinks     : nat
  ; stDoAnalysis      : bool
  }.

Definition updTryShrinks (st : State) (f : nat -> nat) : State :=
  match st with
    | MkState mst mdt ms cs nst ndt ls e r nss nts ana =>
      MkState mst mdt ms cs nst ndt ls e r nss (f nts) ana
  end.

Definition updSuccessShrinks (st : State) (f : nat -> nat) : State :=
  match st with
    | MkState mst mdt ms cs nst ndt ls e r nss nts ana =>
      MkState mst mdt ms cs nst ndt ls e r (f nss) nts ana
  end.

Definition updSuccTests st f :=
  match st with
    | MkState mst mdt ms cs nst     ndt ls e r nss nts ana =>
      MkState mst mdt ms cs (f nst) ndt ls e r nss nts ana
  end.

Definition updDiscTests st f :=
  match st with
    | MkState mst mdt ms cs nst ndt     ls e r nss nts ana =>
      MkState mst mdt ms cs nst (f ndt) ls e r nss nts ana
  end.
