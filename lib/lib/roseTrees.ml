module RoseTrees = struct
  type 'a rose = MkRose of 'a * 'a rose list Lazy.t

  let returnRose x = MkRose (x, lazy [])

  let rec joinRose = function
    | MkRose (r0, tts) ->
        let (MkRose (a, ts)) = r0 in
        MkRose (a, lazy (Main.app (Lists.map joinRose (Lazy.force tts)) (Lazy.force ts)))

  let rec fmapRose f0 = function
    | MkRose (x, rs) -> MkRose (f0 x, lazy (Lists.map (fmapRose f0) (Lazy.force rs)))
end

include RoseTrees
