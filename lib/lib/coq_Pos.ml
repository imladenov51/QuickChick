module Coq_Pos = struct
  let succ = Big_int_Z.succ_big_int

  let rec add = Big_int_Z.add_big_int

  and add_carry x y =
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
          (fun q ->
            (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
              (add_carry p q))
          (fun q -> Big_int_Z.mult_int_big_int 2 (add_carry p q))
          (fun _ ->
            (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x)) (succ p))
          y)
      (fun p ->
        (fun f2p1 f2p f1 p ->
          if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 ()
          else
            let q, r = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
            if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
          (fun q -> Big_int_Z.mult_int_big_int 2 (add_carry p q))
          (fun q ->
            (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x)) (add p q))
          (fun _ -> Big_int_Z.mult_int_big_int 2 (succ p))
          y)
      (fun _ ->
        (fun f2p1 f2p f1 p ->
          if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 ()
          else
            let q, r = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
            if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
          (fun q ->
            (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x)) (succ q))
          (fun q -> Big_int_Z.mult_int_big_int 2 (succ q))
          (fun _ ->
            (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
              Big_int_Z.unit_big_int)
          y)
      x

  let mul = Big_int_Z.mult_big_int

  let rec eq_dec p x0 =
    (fun f2p1 f2p f1 p ->
      if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 ()
      else
        let q, r = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
        if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
      (fun p0 ->
        (fun f2p1 f2p f1 p ->
          if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 ()
          else
            let q, r = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
            if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
          (fun p1 -> eq_dec p0 p1)
          (fun _ -> false)
          (fun _ -> false)
          x0)
      (fun p0 ->
        (fun f2p1 f2p f1 p ->
          if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 ()
          else
            let q, r = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
            if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
          (fun _ -> false)
          (fun p1 -> eq_dec p0 p1)
          (fun _ -> false)
          x0)
      (fun _ ->
        (fun f2p1 f2p f1 p ->
          if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 ()
          else
            let q, r = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
            if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
          (fun _ -> false)
          (fun _ -> false)
          (fun _ -> true)
          x0)
      p
end

include Coq_Pos
