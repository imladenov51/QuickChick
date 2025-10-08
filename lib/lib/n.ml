module N = struct
  let compare x y =
    let s = Big_int_Z.compare_big_int x y in
    if s = 0 then Main.Eq else if s < 0 then Main.Lt else Main.Gt

  let add = Big_int_Z.add_big_int
  let mul = Big_int_Z.mult_big_int

  let to_nat a =
    (fun fO fp n -> if Big_int_Z.sign_big_int n <= 0 then fO () else fp n)
      (fun _ -> 0)
      (fun p -> Pos.to_nat p)
      a
end

include N
