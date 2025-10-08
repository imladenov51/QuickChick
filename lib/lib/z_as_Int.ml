module Z_as_Int = struct
  type t = Big_int_Z.big_int

  let _0 = Big_int_Z.zero_big_int
  let _1 = Big_int_Z.unit_big_int
  let _2 = Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int

  let _3 =
    (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
      Big_int_Z.unit_big_int

  let add = Z.add
  let opp = Z.opp
  let sub = Z.sub
  let mul = Z.mul
  let max = Z.max
  let eqb = Z.eqb
  let ltb = Z.ltb
  let leb = Z.leb
  let eq_dec = Z.eq_dec

  let gt_le_dec i j =
    let b = Z.ltb j i in
    if b then true else false

  let ge_lt_dec i j =
    let b = Z.ltb i j in
    if b then false else true

  let i2z n0 = n0
end

include Z_as_Int
