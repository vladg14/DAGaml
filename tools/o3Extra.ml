open O3

let ( %>> ) : ('a, 'b) o3  -> ('b, 'c ) o3  -> ('a, 'c ) o3  = ( %>> )
let ( +>> ) : ('a, 'b) o3  -> ('b, 's ) o3s -> ('a, 's ) o3s = ( +>> )
let ( >>+) : ('a, 's) o3s -> ('s, 'ss) o3  -> ('a, 'ss) o3s = ( >>+ )
let ( %*  ) : ('a, 'aa) o3 -> ('b, 'bb) o3  -> ('a * 'b, 'aa * 'bb) o3 = ( %* )
let ( +*  ) : ('a, 's) o3s -> ('b, 's ) o3s -> ('a * 'b, 's) o3s = ( +* )

