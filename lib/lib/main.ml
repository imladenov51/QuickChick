module Main = struct
  type __ = Obj.t

  let __ =
    let rec f _ = Obj.repr f in
    Obj.repr f

  let negb = function true -> false | false -> true
  let fst = function x, _ -> x
  let snd = function _, y -> y
  let rec app l m = match l with [] -> m | a :: l1 -> a :: app l1 m
  let sub = fun n m -> Stdlib.max 0 (n - m)

  type comparison = Eq | Lt | Gt

  (* let withTime = (fun f -> let start = Unix.gettimeofday () in let res = f () in let ending = Unix.gettimeofday () in { aug_res = res; aug_time = ((Float.to_int ((ending -. start) *. 1000000.0))) } ) *)

  let internal_eq_rew_r_dep _ _ hC = hC
end

include Main
