module Little = struct
  let rec succ = function
    | Decimal.Nil -> Decimal.D1 Decimal.Nil
    | Decimal.D0 d0 -> Decimal.D1 d0
    | Decimal.D1 d0 -> Decimal.D2 d0
    | Decimal.D2 d0 -> Decimal.D3 d0
    | Decimal.D3 d0 -> Decimal.D4 d0
    | Decimal.D4 d0 -> Decimal.D5 d0
    | Decimal.D5 d0 -> Decimal.D6 d0
    | Decimal.D6 d0 -> Decimal.D7 d0
    | Decimal.D7 d0 -> Decimal.D8 d0
    | Decimal.D8 d0 -> Decimal.D9 d0
    | Decimal.D9 d0 -> Decimal.D0 (succ d0)
end

include Little
