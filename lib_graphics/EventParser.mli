type event = Click of float * float
           | Drag of { start : float * float;
                       finish : float * float }

type t

val create : (event -> unit) -> t
val register_move : t -> float -> float -> unit
val register_mouse_down : t -> float -> float -> unit
val register_mouse_up : t -> float -> float -> unit
