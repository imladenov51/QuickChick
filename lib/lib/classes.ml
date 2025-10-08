module Classes = struct
  type 'a genSized = int -> 'a Generators.g
  type 'a gen = 'a Generators.g
  type 'a shrink = 'a -> 'a list

  let genOfGenSized h = Producer.sized (Obj.magic Generators.producerGen) h
end

include Classes
