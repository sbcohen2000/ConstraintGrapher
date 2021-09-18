exception Out_of_bounds of string
type 'a t

val empty : 'a t
val create : int -> 'a t

val size : 'a t -> int
val edge : 'a t -> int -> int -> 'a option
val edges : 'a t -> int -> 'a list
val neighbors : 'a t -> int -> int list

val expand : 'a t -> 'a t
val connect : 'a t -> int -> int -> 'a -> unit
