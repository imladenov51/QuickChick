module Z = struct
  let double x =
    (fun fO fp fn z ->
      let s = Big_int_Z.sign_big_int z in
      if s = 0 then fO () else if s > 0 then fp z else fn (Big_int_Z.minus_big_int z))
      (fun _ -> Big_int_Z.zero_big_int)
      (fun p -> Big_int_Z.mult_int_big_int 2 p)
      (fun p -> Big_int_Z.minus_big_int (Big_int_Z.mult_int_big_int 2 p))
      x

  let succ_double x =
    (fun fO fp fn z ->
      let s = Big_int_Z.sign_big_int z in
      if s = 0 then fO () else if s > 0 then fp z else fn (Big_int_Z.minus_big_int z))
      (fun _ -> Big_int_Z.unit_big_int)
      (fun p -> (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x)) p)
      (fun p -> Big_int_Z.minus_big_int (Pos.pred_double p))
      x

  let pred_double x =
    (fun fO fp fn z ->
      let s = Big_int_Z.sign_big_int z in
      if s = 0 then fO () else if s > 0 then fp z else fn (Big_int_Z.minus_big_int z))
      (fun _ -> Big_int_Z.minus_big_int Big_int_Z.unit_big_int)
      (fun p -> Pos.pred_double p)
      (fun p ->
        Big_int_Z.minus_big_int
          ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x)) p))
      x

  let rec pos_sub x y =
    (fun f2p1 f2p f1 p ->
      if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 ()
      else
        let q, r = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
        if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
      (fun p ->
        (fun f2p1 f2p f1 p ->
          if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 ()
          else
            let q, r = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
            if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
          (fun q -> double (pos_sub p q))
          (fun q -> succ_double (pos_sub p q))
          (fun _ -> Big_int_Z.mult_int_big_int 2 p)
          y)
      (fun p ->
        (fun f2p1 f2p f1 p ->
          if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 ()
          else
            let q, r = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
            if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
          (fun q -> pred_double (pos_sub p q))
          (fun q -> double (pos_sub p q))
          (fun _ -> Pos.pred_double p)
          y)
      (fun _ ->
        (fun f2p1 f2p f1 p ->
          if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 ()
          else
            let q, r = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
            if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
          (fun q -> Big_int_Z.minus_big_int (Big_int_Z.mult_int_big_int 2 q))
          (fun q -> Big_int_Z.minus_big_int (Pos.pred_double q))
          (fun _ -> Big_int_Z.zero_big_int)
          y)
      x

  let add = Big_int_Z.add_big_int
  let opp = Big_int_Z.minus_big_int
  let sub = Big_int_Z.sub_big_int
  let mul = Big_int_Z.mult_big_int

  let compare =
   fun x y ->
    let s = Big_int_Z.compare_big_int x y in
    if s = 0 then Main.Eq else if s < 0 then Main.Lt else Main.Gt

  let leb x y = match compare x y with Main.Gt -> false | _ -> true
  let ltb x y = match compare x y with Main.Lt -> true | _ -> false
  let eqb = Big_int_Z.eq_big_int
  let max = Big_int_Z.max_big_int
  let eq_dec = Big_int_Z.eq_big_int
end

include Z
