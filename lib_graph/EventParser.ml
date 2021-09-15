type event = CLICK of float * float
           | DRAG of { start : float * float;
                       finish : float * float }

type t_actual = { cb : event -> unit;
                  last : event; }

type t = t_actual ref

let create (cb : (event -> unit)) =
  let state = { cb; last = CLICK (0.0, 0.0) } in
  ref state

let register_move (state : t) (x : float) (y : float) =
  let new_event = match (!state).last with
    | DRAG details -> DRAG { details with finish = (x, y) }
    | CLICK (sx, sy) -> DRAG { start = (sx, sy); finish = (x, y) } in
  state := { !state with last = new_event };
  !state.cb new_event

let register_mouse_down (state : t) (x : float) (y : float) =
  state := { !state with last = CLICK (x, y) }

let register_mouse_up (state : t) (x : float) (y : float) =
  let new_event = match (!state).last with
    | DRAG details -> DRAG { details with finish = (x, y) }
    | CLICK _ -> CLICK (x, y) in
  state := { !state with last = new_event };
  !state.cb new_event
