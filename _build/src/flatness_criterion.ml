open Geometry_tools
open Printf
open BatFloat
(*
little implementation of the flatness criterion for the curves,
we use a data structure of points p=(x,y,z) on the cartesian
coordinates
*)

(*the first test: given an epsilon epsi test if the sum
 * of the cords f(a)f(m) f(m)f(b) is the almost the same
 * lenght as f(a)f(b), using the distance dist
 * *)
(*not to be used with dist1 ! *)
let cord_test_2D p_f_a p_f_b p_f_m epsi dist =
  let d_a_m = dist p_f_a p_f_m in
  let d_b_m = dist p_f_m p_f_b in
  let d_a_b = dist p_f_a p_f_b in
  abs_float (d_a_m +. d_b_m -. d_a_b) < epsi

(*return true if the distance between the point (m,fm) and
 * the line across the points (a,fa) and (b,fb) is less than
 * epsi for the distance dist, return false otherwise*)
let distance_point_cord_test_2D p_f_a p_f_b p_f_m epsi dist =
  let l_f_a_b = Geometry_tools.create_line_2D p_f_a
                (Geometry_tools.minus_2D p_f_a p_f_b) in
  (Geometry_tools.distance_line_point_2D l_f_a_b p_f_m dist) < epsi


(*criterion for the triangle area,
 * return true if the area of the triangle is less
 * than espi for the distance dist, false otherwise*)
let triangle_area_test_2D p_f_a p_f_b p_f_m epsi dist=
  let l_f_a_b = Geometry_tools.create_line_2D p_f_a
                (Geometry_tools.minus_2D p_f_a p_f_b) in
  let dist_f_m_l = Geometry_tools.distance_line_point_2D l_f_a_b p_f_m dist in
  let area = ((dist p_f_a p_f_b) *. (dist_f_m_l))/.4. in
  area < epsi

(* test if the angle defined by p_f_a = (a, f(a)), p_f_m = (m, f(m)) and
 * p_f_b = (b, f(b)) is smaller than epsi for the norm norm*)
let angle_test_2D p_f_a p_f_b p_f_m epsi d =
    abs_float (((Geometry_tools.angle_vector_2D
                   (Geometry_tools.minus_2D p_f_a p_f_m)
                   (Geometry_tools.minus_2D p_f_b p_f_m)) -. BatFloat.pi))
                   < epsi

(*if the function is supposed deriveable, and we have acces to the
 * derivative fonction f', test if the tangents of the points a b
 * are parallel, returns true if the derivative of points a and b
 * are the same or if the difference between 1 and the ration f'b/f'a
 * is less than epsi, returns false otherway*)
let derivative_parallel_test f' a b epsi =
  let dfa = f' a in
  let dfb = f' b in
  if dfa=0. && dfb=0. then true
  else if dfa=0. && dfb!=0. then false
  else abs_float(dfb/.dfa -.1.) < epsi


let cord_test_3D p_f_a p_f_b p_f_m epsi dist =
  let d_a_m = dist p_f_a p_f_m in
  let d_b_m = dist p_f_m p_f_b in
  let d_a_b = dist p_f_a p_f_b in
  abs_float (d_a_m +. d_b_m -. d_a_b) < epsi

(*return true if the distance between the point (m,fm) and
 * the line across the points (a,fa) and (b,fb) is less than
 * epsi for the distance dist, return false otherwise*)
let distance_point_cord_test_3D p_f_a p_f_b p_f_m epsi dist =
  let l_f_a_b = Geometry_tools.create_line_3D p_f_a
                (Geometry_tools.minus_3D p_f_a p_f_b) in
  (Geometry_tools.distance_line_point_3D l_f_a_b p_f_m dist) < epsi


(*criterion for the triangle area,
 * return true if the area of the triangle is less
 * than espi for the distance dist, false otherwise*)
let triangle_area_test_3D p_f_a p_f_b p_f_m epsi dist=
  let l_f_a_b = Geometry_tools.create_line_3D p_f_a
                (Geometry_tools.minus_3D p_f_a p_f_b) in
  let dist_f_m_l = Geometry_tools.distance_line_point_3D l_f_a_b p_f_m dist in
  let area = ((dist p_f_a p_f_b) *. (dist_f_m_l))/.4. in
  area < epsi

(* test if the angle defined by p_f_a = (a, f(a)), p_f_m = (m, f(m)) and
 * p_f_b = (b, f(b)) is smaller than epsi for the norm norm*)
let angle_test_3D p_f_a p_f_b p_f_m epsi d =
    (abs_float (Geometry_tools.angle_vector_3D
                              (Geometry_tools.minus_3D p_f_a p_f_m)
                              (Geometry_tools.minus_3D p_f_b p_f_m))) < epsi


