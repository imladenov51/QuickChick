module type OrderedType = sig
  type t
  type 'x compare = LT | EQ | GT

  val compare : t -> t -> t compare
  val eq_dec : t -> t -> bool
end

include OrderedType
