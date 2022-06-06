let arc ?rev ?fn ?(plane = Plane.xy) ?wedge ~centre ~radius ~start angle =
  Arc2.arc ?rev ?fn ?wedge ~centre:(Plane.project plane centre) ~radius ~start angle
  |> List.map (Plane.lift plane)

let arc_about_centre ?rev ?fn ?dir ?wedge ~centre p1 p2 =
  let plane = Plane.make centre p1 p2 in
  let project = Plane.project plane
  and lift = List.map (Plane.lift plane) in
  let p1' = project p1
  and p2' = project p2
  and centre' = project centre in
  lift @@ Arc2.arc_about_centre ?rev ?dir ?fn ?wedge ~centre:centre' p1' p2'

let arc_through ?rev ?fn ?wedge p1 p2 p3 =
  let plane = Plane.make p3 p1 p2 in
  let project = Plane.project plane
  and lift = List.map (Plane.lift plane) in
  let p1' = project p1
  and p2' = project p2
  and p3' = project p3 in
  lift @@ Arc2.arc_through ?rev ?fn ?wedge p1' p2' p3'
