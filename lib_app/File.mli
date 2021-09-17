type node
type t

val create : (unit -> unit) -> t
val add_constraint : t -> int -> Core.Constraint.t -> unit
val draw : t -> Cairo.context -> unit
val handle : t -> Graphics.EventParser.event -> unit


