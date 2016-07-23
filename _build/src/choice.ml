open Random;;
open Printf
open Geometry_tools
(*contains the function to choice the point at the interior of the
 * interval in the argument, simply choices as the midpoint, or
 * using disturbed interior points, disturbed meaning using a combination
 * of determined choice and a random choice*)


(*returns a float, the midpoint of the interval [a,b]*)
let midpoint_choice a b = (a+.b)/.2.

(*returns a float, a random point around the midpoint of
 * the interval [a,b]*)
let perturbed_midpoint_choice a b =
  Random.self_init ();
  (min a b)+.(0.45+.(Random.float 0.10))*.(abs_float (b-.a))

let perturbed_three_points_choice a b =
  Random.self_init ();
  let ab_distance = abs_float (b-.a) in
  let ab_min = min a b in
  let a1 =ab_min+.(0.23+.(Random.float 0.10))*.ab_distance in
  let a2 = ab_min+.(0.56 +.(Random.float 0.10))*.ab_distance in
  (a1,a2)
;;
(*
(*we suppose that always the the points given as arguments are
 * the two supperior points of the square*)
let square_perturbed_center_point p1 p2 =
  if p1.n<>p2.n  then failwith "points can't have different dimension"
  else
    let square_edge_length = abs_float (p1.x-.p2.x) in
    let a = (min p1.x p2.x)+.(0.45+.(Random.float 0.10))*.square_edge_length in
    let b = (min p1.y p2.y)+.(0.45+.(Random.float 0.10))*.square_edge_length in
    let c = (min p1.z p2.z)+.(0.45+.(Random.float 0.10))*.square_edge_length in
    create_point ~n:p1.n ~x:a ~y:b ~z:c 
;;
*)
