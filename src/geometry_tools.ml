open Printf

(*
*define a simple data structure of cartesian point p= (a,b)
*and those of three_dimensionsal plane p=(a,b,c)
*and tools like distance between points, lines, angle calculation,
*etc
*)

type point_2D = {x : float; y : float} (*point defined as floats*)

let create_point_2D ~x:a ~y:b = {x=a; y=b}

let equals_2D p1 p2 = p1.x=p2.x && p1.y=p2.y

let is_origin_2D p = equals_2D p {x=0.; y=0.}

(*we define the three usual norms in the cartesian plane*)
let norm1_2D p = (abs_float p.x) +. (abs_float p.y)

let norm2_2D p = sqrt ((p.x)**2. +. (p.y)**2.)

let norm_infinity_2D p = max (abs_float p.x)  (abs_float p.y)

(*algebraic operations for the point type*)
let sum_2D p1 p2 = create_point_2D ~x:(p1.x+.p2.x) ~y:(p1.y+.p2.y)

let negative_2D p = create_point_2D ~x:(-.p.x) ~y:(-.p.y)

let minus_2D p1 p2 = sum_2D p1 (negative_2D p2)

(*induced distance by the respectives norms*)
let dist1_2D p1 p2 = norm1_2D (minus_2D p1 p2)

let dist2_2D p1 p2 = norm2_2D (minus_2D p1 p2)

(*like distance2 but without the square root, is still
 * an indicator of proximity
 * *)
let dist2_variant_2D p1 p2 =
  let p = minus_2D p1 p2 in
  p.x**2. +. p.y**2.

let dist_infinity_2D p1 p2 = norm_infinity_2D (minus_2D p1 p2)

(*test if the point p_test is a multiple of the
 * point p_tester*)
let is_multiple_2D ~p_test:p_test ~p_tester:p_tester =
  if equals_2D p_test p_tester then true
  else if is_origin_2D p_test then true
  else if is_origin_2D p_tester then false
  else if p_tester.x=0. && p_test.x!=0. then false
  else if p_tester.y=0. && p_test.y!=0. then false
  else if p_tester.x=0. || p_tester.y=0. then true
  else (p_test.x /. p_tester.x) = (p_test.y /. p_tester.y)

(*test if the vector1 is multiple of vector2, that is to say,
if they are collinear, return false for the degenerate vector*)
let collinear_2D vector1 vector2 =
  if is_origin_2D vector1 || is_origin_2D vector2 then false
  else if equals_2D vector1 vector2 then true
  else if (vector1.x=0. && vector2.x!=0.) || (vector1.x!=0. && vector2.x=0.)
  then false
  else if (vector1.y=0. && vector2.y!=0.) || (vector1.y!=0. && vector2.y=0.)
  then false
  else
    (vector1.x /. vector2.x) = (vector1.y /. vector2.y)

(*a line type, defined by a point p_1 and a vector v,
 * the vector being a point to*)
type line_2D = {p_1 : point_2D; v : point_2D}
(*we use the duality between vectors and points*)

(*test for the line being a point or not*)
let line_is_degenerate_2D li = equals_2D li.v (create_point_2D ~x:0. ~y:0.)

(*create a line from a point and a vector*)
let create_line_2D ~point:p_point ~vector:vector = {p_1=p_point; v=vector}

let print_point_2D p = printf "(%f, %f)" p.x p.y

let print_line_2D li = print_string "point=";
                    print_point_2D li.p_1;
                    print_string ", vector=";
                    print_point_2D li.v

(*test if a point p belongs to the line li*)
let point_belongs_line_2D p li =
  if line_is_degenerate_2D li then false
  else if p =li.p_1 then true
  else is_multiple_2D (minus_2D p li.p_1) li.v

(*gives an integer to caracterize the relation between the lines
 * li1, li2, being 0 for zero points of intersection, 1 for only one
 * point of intersection and 2 otherway
 * *)
let relation_lines_2D li1 li2 =
  if line_is_degenerate_2D li1 || line_is_degenerate_2D li2 then
    failwith "lines can't be degenerates"
  else if is_multiple_2D li1.v li2.v && point_belongs_line_2D li1.p_1 li2 then
    2
  else if is_multiple_2D li1.v li2.v &&
       not(point_belongs_line_2D li1.p_1 li2) then 0
  else 1


(*return a float as the distance dist between a line li and a point p*)
let distance_line_point_2D li p dist =
  if line_is_degenerate_2D li then dist p li.p_1 (*if the line is
  degererate, is simply the distance between points*)
  else
    let vector = li.v in
    let lambda1 = (vector.x*.(p.x-.li.p_1.x)+.vector.y*.(p.y-.li.p_1.y)) in
    let lambda2 = 1./.( vector.x**2. +. vector.y**2.) in
    let lambda = lambda1*. lambda2 in (*the lambda(s) are simply the value where
                                        this distance function reach its
                                        minimum, we divide the definition, to
                                        make easier the lecture*)
    let p_intermedi = (create_point_2D ~x:(lambda*.vector.x)
                                       ~y:(lambda*.vector.y)) in
    dist (sum_2D li.p_1 p_intermedi) p

(*return the "interior" angle in radian between two vectors v1 and v2
 *represented by points, norm is the used norm*)
let angle_vector_2D v1 v2 =
    if is_origin_2D v1 || is_origin_2D v2 then
      failwith "Impossible to compute the angle with a degenerated vector"
    else acos((v1.x*.v2.x +. v1.y*.v2.y) /. ((norm2_2D v1) *. (norm2_2D v2)))
;;


(*
 *the same as two-dimenionsal points, but three-dimensional points are
 *implemented
 *)
type point_3D = {x : float; y : float; z : float} (*point defined as floats*)

let create_point_3D ~x:a ~y:b ~z:c = {x=a; y=b; z=c}

let equals_3D p1 p2 = p1.x=p2.x && p1.y=p2.y && p1.z=p2.z

let is_origin_3D p = equals_3D p {x=0.; y=0.; z=0.}

(*we define the three usual norms in the cartesian plane*)
let norm1_3D p = (abs_float p.x) +. (abs_float p.y) +. (abs_float p.z)

let norm2_3D p = sqrt ((p.x)**2. +. (p.y)**2. +. (p.z)**2.)

let norm_infinity_3D p = max (abs_float p.x)
                         (max (abs_float p.y) (abs_float p.z))

(*algebraic operations for the point type*)
let sum_3D p1 p2 =
  create_point_3D ~x:(p1.x+.p2.x) ~y:(p1.y+.p2.y) ~z:(p1.z+.p2.z)

let negative_3D p = create_point_3D ~x:(-.p.x) ~y:(-.p.y) ~z:(-.p.z)

let minus_3D p1 p2 = sum_3D p1 (negative_3D p2)

(*induced distance by the respectives norms*)
let dist1_3D p1 p2 = norm1_3D (minus_3D p1 p2)

let dist2_3D p1 p2 = norm2_3D (minus_3D p1 p2)

(*like distance2 but without the square root, is still
 * an indicator of proximity
 * *)
let dist2_variant_3D p1 p2 =
  let p = minus_3D p1 p2 in
  (p.x)**2. +. (p.y)**2. +. (p.z)**2.

let dist_infinity_3D p1 p2 = norm_infinity_3D (minus_3D p1 p2)

(*test if the point p_test is a multiple of the
 * point p_tester*)
let is_multiple_3D ~p_test:p_test ~p_tester:p_tester =
  if equals_3D p_test p_tester then true
  else if is_origin_3D p_test then true
  else if is_origin_3D p_tester then false
  else if p_tester.x=0. && p_test.x<>0. then false
  else if p_tester.y=0. && p_test.y<>0. then false
  else if p_tester.z=0. && p_test.z<>0. then false
  else if p_tester.x=0. || p_tester.y=0. || p_tester.z=0. then true
  else (p_test.x /. p_tester.x) = (p_test.y /. p_tester.y) &&
       (p_test.x /. p_tester.x) = (p_test.z /. p_tester.z)

(*test if the vector1 is multiple of vector2, that is to say,
if they are collinear, return false for the degenerate vector*)
let collinear_3D vector1 vector2 =
  if is_origin_3D vector1 || is_origin_3D vector2 then false
  else if equals_3D vector1 vector2 then true
  else if (vector1.x=0. && vector2.x<>0.) || (vector1.x<>0. && vector2.x=0.)
  then false
  else if (vector1.y=0. && vector2.y<>0.) || (vector1.y<>0. && vector2.y=0.)
  then false
  else if (vector1.z=0. && vector2.z<>0.) || (vector1.z<>0. && vector2.z=0.)
  then false
  else
    (vector1.x /. vector2.x) = (vector1.y /. vector2.y) &&
    (vector1.x /. vector2.x) = (vector1.z /. vector2.z)

(*a line type, defined by a point p_1 and a vector v,
 * the vector being a point to*)
type line_3D = {p_1 : point_3D; v : point_3D}
(*we use the duality between vectors and points*)

(*test for the line being a point or not*)
let line_is_degenerate_3D li =
  equals_3D li.v (create_point_3D ~x:0. ~y:0. ~z:0.)

(*create a line from a point and a vector*)
let create_line_3D ~point:p_point ~vector:vector = {p_1=p_point; v=vector}

let print_point_3D p = printf "(%f, %f, %f)" p.x p.y p.z

let print_line_3D li = print_string "point=";
                    print_point_3D li.p_1;
                    print_string ", vector=";
                    print_point_3D li.v

(*test if a point p belongs to the line li*)
let point_belongs_line_3D p li =
  if line_is_degenerate_3D li then false
  else if p =li.p_1 then true
  else is_multiple_3D (minus_3D p li.p_1) li.v

(*gives an integer to caracterize the relation between the lines
 * li1, li2, being 0 for zero points of intersection, 1 for only one
 * point of intersection and 2 otherway
 * *)
let relation_lines_3D li1 li2 =
  if line_is_degenerate_3D li1 || line_is_degenerate_3D li2 then
    failwith "lines can't be degenerates"
  else if is_multiple_3D li1.v li2.v && point_belongs_line_3D li1.p_1 li2 then
    2
  else if is_multiple_3D li1.v li2.v &&
       not(point_belongs_line_3D li1.p_1 li2) then 0
  else 1


(*return a float as the distance dist between a line li and a point p*)
let distance_line_point_3D li p dist =
  if line_is_degenerate_3D li then dist p li.p_1 (*if the line is
  degererate, is simply the distance between points*)
  else
    let vector = li.v in
    let lambda1 = (vector.x*.(p.x-.li.p_1.x)+.vector.y*.(p.y-.li.p_1.y)
                   +.vector.z*.(p.z-.li.p_1.z)) in
    let lambda2 = 1./.( vector.x**2. +. vector.y**2. +. vector.z**2.) in
    let lambda = lambda1*. lambda2 in (*the lambda(s) are simply the value where
                                        this distance function reach its
                                        minimum, we divide the definition, to
                                        make easier the lecture*)
    let p_intermedi = (create_point_3D ~x:(lambda*.vector.x)
                                       ~y:(lambda*.vector.y)
                                       ~z:(lambda*.vector.z)) in
    dist (sum_3D li.p_1 p_intermedi) p

(*return the "interior" angle in radian between two vectors v1 and v2
 *represented by points, norm is the used norm*)
let angle_vector_3D v1 v2 =
    if is_origin_3D v1 || is_origin_3D v2 then
      failwith "Impossible to compute the angle with a degenerated vector"
    else
      acos((v1.x*.v2.x+.v1.y*.v2.y+.v1.z*.v2.z)/.((norm2_3D v1)*.(norm2_3D v2)))

let to_3D p =
  let a,b = p.x, p.y in
  create_point_3D ~x:a ~y:b ~z:0.
