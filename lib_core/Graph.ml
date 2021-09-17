type 'a t = 'a array * bool array array

let create (node : 'a) =
  [| node |], [| [| false |] |]
  
let print (graph : 'a t) =
  let _, connections = graph in
  Array.iteri (fun i row ->
      let strs = Array.map (fun b -> if b then "t" else "_") row in
      print_endline (Int.to_string i ^ ": " ^ String.concat " " (Array.to_list strs))
    ) connections

let add_node (graph : 'a t) (node : 'a) =
  let nodes, connections = graph in
  let nodes' = Array.append nodes [| node |] in
  let new_node_idx = Array.length nodes - 1 in
  let connections' = Array.map (fun row ->
                         Array.append row [| false |]) connections in
  let connections' = Array.append connections'
                       [| Array.make (new_node_idx + 2) false |] in
  nodes', connections'

let update_node (graph : 'a t) (idx : int) (node : 'a) =
  let nodes, _ = graph in
  Array.set nodes idx node

let nodes (graph : 'a t) =
  let nodes, _ = graph in nodes

let size (graph : 'a t) =
  let nodes, _ = graph in Array.length nodes

let connect (graph : 'a t) (a_idx : int) (b_idx : int) =
  let _, connections = graph in
  Array.set (Array.get connections a_idx) b_idx true

let disconnect (graph : 'a t) (a_idx : int) (b_idx : int) = 
  let _, connections = graph in
  Array.set (Array.get connections a_idx) b_idx false

let neighbor_indices (graph : 'a t) (idx : int) =
  let _, connections = graph in
  let row = Array.get connections idx in
  let _, lst = Array.fold_left (fun (idx, lst) b ->
                   if b then (idx + 1, idx::lst)
                   else (idx + 1, lst)) (0, []) row in
  lst

let at (graph : 'a t) (idx : int) =
  let nodes, _ = graph in
  Array.get nodes idx

let neighbors (graph : 'a t) (idx : int) =
  let indices = neighbor_indices graph idx in
  let f = at graph in
  List.map f indices


