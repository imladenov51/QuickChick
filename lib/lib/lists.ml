module Lists = struct
  let map f l = List.map f l
  let fold_left f l a = List.fold_left f a l
  let rev l = List.rev l
  let fold_right f a l = List.fold_right f l a
  let combine l l' = List.combine l l'
end

include Lists
