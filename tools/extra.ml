let flip f x y = f y x
let ( <| ) x y = x y
let ( >> ) f g x = g ( f x )
let ( << ) g f x = g ( f x )
let ( ||> ) l f = List.rev_map f l |> List.rev
let ( <|| ) f l = List.rev_map f l |> List.rev
let ( |+ ) l f x = List.fold_left f x l
let ( ||>> ) f g x = List.rev_map g (f x) |> List.rev
