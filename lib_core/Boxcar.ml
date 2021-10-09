type elem = float option
type t = int * elem array

let create (n : int) = (0, Array.make n None);;

let update (idx, arr : t) (v : float) =
  Array.set arr idx (Some v);
  (Int.rem (idx + 1) (Array.length arr), arr);;

let mean (_, arr : t) =
  let sum = Array.fold_left (fun sum elem ->
                match elem with
                | Some f -> f +. sum
                | None -> sum) 0. arr in
  sum /. (Float.of_int (Array.length arr));;
