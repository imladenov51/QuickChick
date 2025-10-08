module Ascii = struct
  let rec n_of_digits = function
    | [] -> Big_int_Z.zero_big_int
    | b :: l' ->
        N.add
          (if b then Big_int_Z.unit_big_int else Big_int_Z.zero_big_int)
          (N.mul (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int) (n_of_digits l'))

  let n_of_ascii a =
    (* If this appears, you're using Ascii internals. Please don't *)
    (fun f c ->
      let n = Char.code c in
      let h i = n land (1 lsl i) <> 0 in
      f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
      (fun a0 a1 a2 a3 a4 a5 a6 a7 -> n_of_digits [ a0; a1; a2; a3; a4; a5; a6; a7 ])
      a
end

include Ascii
