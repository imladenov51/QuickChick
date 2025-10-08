module Coq_String = struct
  let rec string_dec s x =
    match s with
    | [] -> ( match x with [] -> true | _ :: _ -> false)
    | a :: s0 -> (
        match x with
        | [] -> false
        | a0 :: s1 -> if a = a0 then string_dec s0 s1 else false)

  let append (l : char list) (l' : char list) = List.append l l'
end

include Coq_String
