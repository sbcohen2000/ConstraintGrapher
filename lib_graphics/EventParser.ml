type event = Click of float * float
           | Drag of { start : float * float;
                       finish : float * float }

type t_actual = { cb : event -> unit;
                  last : event; }

type t = t_actual ref

let create (cb : (event -> unit)) =
  let state = { cb; last = Click (0.0, 0.0) } in
  ref state

let register_move (state : t) (x : float) (y : float) =
  let new_event = match (!state).last with
    | Drag details -> Drag { start = details.finish;
                             finish = (x, y) }
    | Click (sx, sy) -> Drag { start = (sx, sy); finish = (x, y) } in
  state := { !state with last = new_event };
  !state.cb new_event

let register_mouse_down (state : t) (x : float) (y : float) =
  state := { !state with last = Click (x, y) }

let register_mouse_up (state : t) (x : float) (y : float) =
  let new_event = match (!state).last with
    | Drag details -> Drag { details with finish = (x, y) }
    | Click _ -> Click (x, y) in
  state := { !state with last = new_event };
  !state.cb new_event
