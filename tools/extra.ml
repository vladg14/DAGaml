let flip f x y = f y x
let ( <| ) x y = x y
let ( >> ) f g x = g ( f x )
let ( << ) g f x = g ( f x )
let ( ||> ) l f = List.map f l
let ( <|| ) f l = List.map f l
let ( |+ ) l f x = List.fold_left f x l
let ( ||>> ) f g x = List.map g (f x)
