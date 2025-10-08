module ShowFunctions = struct
  let rec prepend a = function [] -> [] | h :: t0 -> a :: h :: prepend a t0
  let intersperse a = function [] -> [] | h :: t0 -> h :: prepend a t0
  let string_concat l = Lists.fold_left Coq_String.append l []
end

include ShowFunctions
