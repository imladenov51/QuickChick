module AsciiOT = struct
  type t = char

  let compare c d =
    let c0 = N.compare (Ascii.n_of_ascii c) (Ascii.n_of_ascii d) in
    match c0 with
    | Main.Eq -> OrderedType.EQ
    | Main.Lt -> OrderedType.LT
    | Main.Gt -> OrderedType.GT
end

include AsciiOT
