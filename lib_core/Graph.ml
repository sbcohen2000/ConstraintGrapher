exception Out_of_bounds of string
type 'a t = 'a option array array option

let empty = None

let create (n : int) =
  Some (Array.make n (Array.make n None))

let size (g : 'a t) =
  match g with
  | Some m -> Array.length m
  | None -> 0

let edge (g : 'a t) (orig : int) (dest : int) =
  match g with
  | None -> None
  | Some m -> Array.get (Array.get m orig) dest

let edges (g : 'a t) (orig : int) =
  match g with
  | None -> []
  | Some m ->
     let row = Array.get m orig in
     Array.fold_left (fun lst edge ->
         match edge with
         | Some e -> e::lst
         | None -> lst) [] row

let neighbors (g : 'a t) (orig : int) =
  match g with
  | None -> []
  | Some m ->
     let row = Array.get m orig in
     let _, lst = 
       Array.fold_left (fun (idx, lst) edge ->
           match edge with
           | Some _ -> (idx + 1), idx::lst
           | None -> (idx + 1), lst) (0, []) row in
     lst

(* copies 'from' into the upper
 * right matrix of 'into' *)
let copy (from : 'a t) (into : 'a t) =
  match (from, into) with
  | (Some f_mat, Some to_mat) ->
     Array.iteri (fun rn col ->
         Array.iteri (fun cn edge ->
             Array.set (Array.get to_mat rn) cn edge)
           col) f_mat
  | _ -> ()

(* increase the capacity of the
 * graph by 1 *)
let expand (g : 'a t) =
  let new_g = create (size g + 1) in
  copy g new_g;
  new_g

let connect (g : 'a t) (orig : int) (dest : int) (edge : 'a) =
  match g with
  | None -> raise (Out_of_bounds "connect: graph is empty")
  | Some m -> Array.set (Array.get m orig) dest (Some edge)
