module Point =
  struct
    type t = float * float

    let zero = 0.0, 0.0
    
    let add (a : t) (b : t) =
      let (ax, ay), (bx, by) = a, b in
      ax +. bx, ay +. by

    let sub (a : t) (b : t) =
      let (ax, ay), (bx, by) = a, b in
      ax -. bx, ay -. by
    
    let scale (a : t) (s : float) =
      let x, y = a in x *. s, y *. s

    let distance (a : t) (b : t) =
      let (ax, ay), (bx, by) = a, b in
      Float.sqrt (Float.pow (ax -. bx) 2. +. Float.pow (ay -. by) 2.)

    let mag (a : t) =
      distance a zero
    
    let norm (a : t) =
      scale a (1. /. mag a)
  end

module Rect =
  struct
    (* x, y, width, height *)
    type t = float * float * float * float

    let x (rect : t) = let x, _, _, _ = rect in x
    let y (rect : t) = let _, y, _, _ = rect in y
    let width  (rect : t) = let _, _, w, _ = rect in w
    let height (rect : t) = let _, _, _, h = rect in h

    let position (rect : t) = let x, y, _, _ = rect in x, y

    let set_position (rect : t) (pos : Point.t) =
      let x, y = pos in
      (x, y, width rect, height rect)

    let translate (rect : t) (by : Point.t) =
      set_position rect (Point.add (position rect) by)
    
    let is_inside (rect : t) (point : Point.t) =
      let rx, ry, w, h = rect in
      let px, py = point in
      px >= rx && px <= rx +. w
      && py >= ry && py <= ry +. h

  end
