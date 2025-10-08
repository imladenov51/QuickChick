module RandomQC = struct
  let mkRandomSeed x =
    Random.init x;
    Random.get_state ()

  type randomSeed = Random.State.t

  let randomNext r = (Random.State.bits r, r)
  let randomSplit x = (x, x)

  let randomRNat =
   fun (x, y) r ->
    if y < x then failwith "choose called with unordered arguments"
    else (x + Random.State.full_int r (y - x + 1), r)

  let randomRBool _ r = (Random.State.bool r, r)

  let randomRInt (x, y) r =
    if Big_int_Z.lt_big_int y x then failwith "choose called with unordered arguments"
    else
      let range_Z = Big_int_Z.succ_big_int (Big_int_Z.sub_big_int y x) in
      let range_int = Big_int_Z.int_of_big_int range_Z in
      ( Big_int_Z.add_big_int x (Big_int_Z.big_int_of_int (Random.State.int r range_int)),
        r )

  let randomRN (x, y) r =
    if Big_int_Z.lt_big_int y x then failwith "choose called with unordered arguments"
    else
      let range_Z = Big_int_Z.succ_big_int (Big_int_Z.sub_big_int y x) in
      let range_int = Big_int_Z.int_of_big_int range_Z in
      ( Big_int_Z.add_big_int x (Big_int_Z.big_int_of_int (Random.State.int r range_int)),
        r )

  let enumRNat p =
    LazyList.lazy_seq
      (fun x -> Stdlib.Int.succ x)
      (fst p)
      (Stdlib.Int.succ (snd p - fst p))

  let newRandomSeed = Random.State.make_self_init ()

  type 'a choosableFromInterval =
    | Build_ChoosableFromInterval of
        ('a * 'a -> randomSeed -> 'a * randomSeed) * ('a * 'a -> 'a LazyList.lazyList)

  let chooseNat = Build_ChoosableFromInterval (randomRNat, enumRNat)
  let randomR = function Build_ChoosableFromInterval (randomR, _) -> randomR
  let enumR = function Build_ChoosableFromInterval (_, enumR) -> enumR
end

include RandomQC
