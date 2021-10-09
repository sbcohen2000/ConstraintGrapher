type t

(* create a boxcar filter with length *)
val create : int -> t
(* add a new value to the filter *)
val update : t -> float -> t
(* returns the mean of the filter *)
val mean : t -> float
