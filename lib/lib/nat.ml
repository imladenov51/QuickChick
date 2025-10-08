module Nat = struct
  let rec to_little_uint n0 acc =
    (fun fO fS n -> if n = 0 then fO () else fS (n - 1))
      (fun _ -> acc)
      (fun n1 -> to_little_uint n1 (Little.succ acc))
      n0

  let to_uint n0 = Decimal.rev (to_little_uint n0 (Qc__Decimal.D0 Qc__Decimal.Nil))

  let rec divmod x y q u =
    (fun fO fS n -> if n = 0 then fO () else fS (n - 1))
      (fun _ -> (q, u))
      (fun x' ->
        (fun fO fS n -> if n = 0 then fO () else fS (n - 1))
          (fun _ -> divmod x' y (Stdlib.Int.succ q) y)
          (fun u' -> divmod x' y q u')
          u)
      x

  let div x = function 0 -> 0 | y -> x / y
  let modulo x = function 0 -> x | y -> x mod y
  let coq_Decidable_le_nat = ( <= )
end

include Nat
