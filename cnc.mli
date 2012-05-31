type 'a request

type t
val connect : string -> int -> t
val name_of_axis : [< `X | `Y | `Z ] -> string
val home : [< `X | `Y | `Z ] list -> unit request
val move : [< `X of float | `Y of float | `Z of float ] list -> unit request
val where : (float * float * float) request
val motors_off : unit request

val wait : t -> 'a request -> 'a
val ignore : t -> 'a request -> unit
val async : t -> 'a request -> ('a -> unit) -> unit