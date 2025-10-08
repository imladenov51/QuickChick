module Pos = struct
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

  let rec pred_double x =
    (fun f2p1 f2p f1 p ->
      if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 ()
      else
        let q, r = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
        if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
      (fun p ->
        (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
          (Big_int_Z.mult_int_big_int 2 p))
      (fun p ->
        (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x)) (pred_double p))
      (fun _ -> Big_int_Z.unit_big_int)
      x

  let mul = Big_int_Z.mult_big_int

  let rec compare_cont r x y =
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
          (fun q -> compare_cont r p q)
          (fun q -> compare_cont Main.Gt p q)
          (fun _ -> Main.Gt)
          y)
      (fun p ->
        (fun f2p1 f2p f1 p ->
          if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 ()
          else
            let q, r = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
            if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
          (fun q -> compare_cont Main.Lt p q)
          (fun q -> compare_cont r p q)
          (fun _ -> Main.Gt)
          y)
      (fun _ ->
        (fun f2p1 f2p f1 p ->
          if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 ()
          else
            let q, r = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
            if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
          (fun _ -> Main.Lt)
          (fun _ -> Main.Lt)
          (fun _ -> r)
          y)
      x

  let compare = compare_cont Main.Eq

  let rec eqb p q =
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
          (fun q0 -> eqb p0 q0)
          (fun _ -> false)
          (fun _ -> false)
          q)
      (fun p0 ->
        (fun f2p1 f2p f1 p ->
          if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 ()
          else
            let q, r = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
            if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
          (fun _ -> false)
          (fun q0 -> eqb p0 q0)
          (fun _ -> false)
          q)
      (fun _ ->
        (fun f2p1 f2p f1 p ->
          if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 ()
          else
            let q, r = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
            if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
          (fun _ -> false)
          (fun _ -> false)
          (fun _ -> true)
          q)
      p

  let rec iter_op op p a =
    (fun f2p1 f2p f1 p ->
      if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 ()
      else
        let q, r = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
        if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
      (fun p0 -> op a (iter_op op p0 (op a a)))
      (fun p0 -> iter_op op p0 (op a a))
      (fun _ -> a)
      p

  let to_nat x = iter_op ( + ) x (Stdlib.Int.succ 0)
end

include Pos
