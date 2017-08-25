type stream  = bool list
type 't dump = 't -> stream -> stream
type 't load = stream -> 't  * stream
type 't o3s  = ('t, stream) O3.o3s
type 't o3b  = ('t, Bitv.t) O3.o3
