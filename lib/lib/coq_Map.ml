module Coq_Map = struct
  type 'elt bst = Leaf | Node of 'elt bst * StringOT.t * 'elt * 'elt bst * Z_as_Int.t
  type 'elt t = 'elt bst

  let empty = Leaf

  let rec find x = function
    | Leaf -> None
    | Node (l, y, d, r, _) -> (
        match StringOT.compare x y with
        | OrderedType.LT -> find x l
        | OrderedType.EQ -> Some d
        | OrderedType.GT -> find x r)

  let height = function Leaf -> Z_as_Int._0 | Node (_, _, _, _, h) -> h

  let create l x e r =
    Node (l, x, e, r, Z.max (height l) (Z.add (height r) Big_int_Z.unit_big_int))

  let assert_false = create

  let bal l x d r =
    let hl = height l in
    let hr = height r in
    if
      Z_as_Int.gt_le_dec hl
        (Z.add hr (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int))
    then
      match l with
      | Leaf -> assert_false l x d r
      | Node (ll, lx, ld, lr, _) -> (
          if Z_as_Int.ge_lt_dec (height ll) (height lr) then
            create ll lx ld (create lr x d r)
          else
            match lr with
            | Leaf -> assert_false l x d r
            | Node (lrl, lrx, lrd, lrr, _) ->
                create (create ll lx ld lrl) lrx lrd (create lrr x d r))
    else if
      Z_as_Int.gt_le_dec hr
        (Z.add hl (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int))
    then
      match r with
      | Leaf -> assert_false l x d r
      | Node (rl, rx, rd, rr, _) -> (
          if Z_as_Int.ge_lt_dec (height rr) (height rl) then
            create (create l x d rl) rx rd rr
          else
            match rl with
            | Leaf -> assert_false l x d r
            | Node (rll, rlx, rld, rlr, _) ->
                create (create l x d rll) rlx rld (create rlr rx rd rr))
    else create l x d r

  let rec fold f0 m a =
    match m with Leaf -> a | Node (l, x, d, r, _) -> fold f0 r (f0 x d (fold f0 l a))

  let rec add x d = function
    | Leaf -> Node (Leaf, x, d, Leaf, Big_int_Z.unit_big_int)
    | Node (l, y, d', r, h) -> (
        match StringOT.compare x y with
        | OrderedType.LT -> bal (add x d l) y d' r
        | OrderedType.EQ -> Node (l, y, d, r, h)
        | OrderedType.GT -> bal l y d' (add x d r))
end

include Coq_Map
