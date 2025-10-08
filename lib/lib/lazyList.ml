module LazyList = struct
  type 'a lazyList = Lnil | Lcons of 'a * (unit -> 'a lazyList)

  let rec lazy_seq s lo len =
    (fun fO fS n -> if n = 0 then fO () else fS (n - 1))
      (fun _ -> Lnil)
      (fun len' -> Lcons (lo, fun _ -> lazy_seq s (s lo) len'))
      len
end

include LazyList
