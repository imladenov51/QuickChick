module Show = struct
  type 'a show = 'a -> char list

  let mk_show (x : 'a show) = x
  let newline = '\n' :: []
  let nl = '\n' :: []

  let print_extracted_coq_string l =
    print_string
      (let s = Bytes.create (List.length l) in
       let rec copy i = function
         | [] -> s
         | c :: l ->
             Bytes.set s i c;
             copy (i + 1) l
       in
       Bytes.to_string (copy 0 l))

  let trace l =
    print_extracted_coq_string l;
    flush stdout;
    fun y -> y

  let show_a str_func i =
    let s = str_func i in
    let rec copy acc i = if i < 0 then acc else copy (s.[i] :: acc) (i - 1) in
    copy [] (String.length s - 1)

  let show_nat i = show_a string_of_int i
  let show_bool i = show_a string_of_bool i
  let show_Z i = show_a Big_int_Z.string_of_big_int i
  let show_N i = show_a Big_int_Z.string_of_big_int i (* TODO: is this ok? *)
end

include Show
