type point_2D
(*type point of the R^2 cartesian plane, of the form (x,y)*)

val create_point_2D : x:float -> y:float -> point_2D
(*[creates_point_2D x:a y:b] returns a point, p=(a,b)*)

val equals_2D : point_2D -> point_2D -> bool
(*[equals_2D p1=(x1,y1) p2=(x2,y2)] returns true if x1=x2 and y1=y2*)

val is_origin_2D : point_2D -> bool
(* [is_origin_2D p] returns true if p=(0,0)*)

val norm1_2D : point_2D -> float
(*[norm1_2D p=(x,y)] returns |x|+|y|*)

val norm2_2D : point_2D -> float
(*[norm2_2D p=(x,y)] returns sqrt(x^2 + y^2)*)

val norm_infinity_2D : point_2D -> float
(*[norm_infinity_2D p=(x,y)] returns max(|x|,|y|)*)

val sum_2D : point_2D -> point_2D -> point_2D
(*[sum_2D p1=(x1,y1) p2=(x2,y2)] returns a point p=(x1+x2,y1+y2)*)

val negative_2D : point_2D -> point_2D
(*[negative_2D p=(x,y)] returns a point p=(-x,-y)*)

val minus_2D : point_2D -> point_2D -> point_2D
(*[minus_2D p1=(x1,y1) p2=(x2,y2)] returns a point p=(x1-x2,y1-y2)*)

val dist1_2D : point_2D -> point_2D -> float
(*
*[dist1_2D p1=(x1,y1) p2=(x2,y2)] returns the norm1_2D of the
*point p=(x1-x2,y1-y2)
*)

val dist2_2D : point_2D -> point_2D -> float
(*
*[dist2_2D p1=(x1,y1) p2=(x2,y2)] returns the norm2_2D of the
*point p=(x1-x2,y1-y2)
*)

val dist2_variant_2D : point_2D -> point_2D -> float
(*[dist2_variant_2D p1=(x1,y1) p2=(x2,y2)] returns (x1-x2)^2+(y1-y2)^2*)

val dist_infinity_2D : point_2D -> point_2D -> float
(*
 *[dist_infinity_2D p1=(x1,y1) p2=(x2,y2)] returns the norm_infinity_2D of
 *the point p=(x1-x2,y1-y2)
 *)

val is_multiple_2D : p_test:point_2D -> p_tester:point_2D -> bool
(*
 *[is_multiple_2D p_test p_tester] returns true if the point p_test can be
 *found as the multiplication of a real number and the point p_tester
 *)

val collinear_2D : point_2D -> point_2D -> bool
(*
 *[collinear_2D vector1 vector2] returns true if the vectors are collinear,
 *if one or two of the vectors are degenerate returns false,
 *the vectors are identified as points, as the representation is the same
 *)
type line_2D
(*
 *a line in the two dimensional space, is represented by a point p_1
 *which belongs to the line and another point v, wich is a direction
 *vector
 *)

val line_is_degenerate_2D : line_2D -> bool
(*[line_isdenerate_2D li] returns true if the direction vector is null*)

val create_line_2D : point:point_2D -> vector:point_2D -> line_2D
(*[create_line_2D p v] returns a line l=(point:p, vecteur:v)*)

val print_point_2D : point_2D -> unit
(*[print_point_2D p=(x,y)] prints the point p in the form "(x,y)"*)

val print_line_2D : line_2D -> unit
(*
 *[print_line_2D l=(p=(x,y),v=(v1,v2))] prints the line in the form
 *"point=(x,y), vector=(v1,v2)"
 *)

val point_belongs_line_2D : point_2D -> line_2D -> bool
(*
 *[point_belong_line_2D p l] returns true if the point p belongs to the line l
 *)

val relation_lines_2D : line_2D -> line_2D -> int
(*
 *[relation_lines_2D l1 l2] returns 0 if the lines l1 and l2 has no
 *intersection points, 1 if only one intersection point, 2 otherway
 *)

val distance_line_point_2D : line_2D -> point_2D
                             -> (point_2D -> point_2D -> float) -> float
(*[distance_lin_point_2D l p d] returns the distance between the point p
 *and the line l, using the distance d, the distance between a point p and a
 *line l been the distance between the point p and the point of the line l
 *closest to p
 *)

val angle_vector_2D : point_2D -> point_2D -> float
(*
 *[angle_vector_2D v1 V2] returns the angle between the vectors v1 v2 as they
 *part of the origin
 *)

 type point_3D
(*type point of the R^3 cartesian plane, of the form (x,y,z)*)

val create_point_3D : x:float -> y:float -> z:float -> point_3D
(*[creates_point_3D x:a y:b z:c] returns a point, p=(a,b,c)*)

val equals_3D : point_3D -> point_3D -> bool
(*
 *[equals_3D p1=(x1,y1,z1) p2=(x2,y2,z2)] returns true if x1=x2 and
 *y1=y2 and z1=z2
 *)

val is_origin_3D : point_3D -> bool
(* [is_origin_3D p] returns true if p=(0,0,0)*)

val norm1_3D : point_3D -> float
(*[norm1_3D p=(x,y,z)] returns |x|+|y|+|z|*)

val norm2_3D : point_3D -> float
(*[norm2_3D p=(x,y,z)] returns sqrt(x^2 + y^2 + z^2)*)

val norm_infinity_3D : point_3D -> float
(*[norm_infinity_3D p=(x,y,z)] returns max(|x|,|y|,|z|)*)

val sum_3D : point_3D -> point_3D -> point_3D
(*[sum_3D p1=(x1,y1,z1) p2=(x2,y2,z2)] returns a point p=(x1+x2,y1+y2,z1+z2)*)

val negative_3D : point_3D -> point_3D
(*[negative_3D p=(x,y,z)] returns a point p= (-x,-y,-z)*)

val minus_3D : point_3D -> point_3D -> point_3D
(*
 *[minus_3D p1=(x1,y1,z1) p2=(x2,y2,z2)] returns
 *a point p= (x1-x2,y1-y2,z1-z2)
 *)

val dist1_3D : point_3D -> point_3D -> float
(*
*[dist1_3D p1=(x1,y1,z1) p2=(x2,y2,z2)] returns the norm1_3D of the
*point p=(x1-x2,y1-y2,z1-z2)
*)

val dist2_3D : point_3D -> point_3D -> float
(*
*[dist2_3D p1=(x1,y1,z1) p2=(x2,y2,z2)] returns the norm2_3D of the
*point p=(x1-x2,y1-y2,z1-z2)
*)

val dist2_variant_3D : point_3D -> point_3D -> float
(*
 *[dist2_variant_3D p1=(x1,y1,z1) p2=(x2,y2,z2)] returns
 *(x1-x2)^2+(y1-y2)^2(z1-z2)^2
 *)

val dist_infinity_3D : point_3D -> point_3D -> float
(*
 *[dist_infinity_3D p1=(x1,y1,z1) p2=(x2,y2,z2)] returns the norm_infinity_3D
 *of the point p=(x1-x2,y1-y2,z1-z2)
 *)

val is_multiple_3D : p_test:point_3D -> p_tester:point_3D -> bool
(*
 *[is_multiple_3D p_test p_tester] returns true if the point p_test can be
 *found as the multiplication of a real number and the point p_tester
 *)

val collinear_3D : point_3D -> point_3D -> bool
(*
 *[collinear_3D vector1 vector2] returns true if the vectors are collinear,
 *if one or two of the vectors are degenerate returns false,
 *the vectors are identified as points, as the representation is the same
 *)
type line_3D
(*
 *a line in the three dimensional space, is represented by a point p_1
 *which belongs to the line and another point v, wich is a direction
 *vector
 *)

val line_is_degenerate_3D : line_3D -> bool
(*[line_isdenerate_3D li] returns true if the direction vector is null*)

val create_line_3D : point:point_3D -> vector:point_3D -> line_3D
(*[create_line_3D p v] returns a line l=(point:p, vecteur:v)*)

val print_point_3D : point_3D -> unit
(*[print_point_3D p=(x,y,z)] prints the point p in the form "(x,y,z)"*)

val print_line_3D : line_3D -> unit
(*
 *[print_line_3D l=(p=(x,y,z),v=(v1,v2,v3))] prints the line in the form
 *"point=(x,y,z), vector=(v1,v2,v3)"
 *)

val point_belongs_line_3D : point_3D -> line_3D -> bool
(*
 *[point_belong_line_3D p l] returns true if the point p belongs to the line l
 *)

val relation_lines_3D : line_3D -> line_3D -> int
(*
 *[relation_lines_3D l1 l2] returns 0 if the lines l1 and l2 has no
 *intersection points, 1 if only one intersection point, 2 otherway
 *)

val distance_line_point_3D : line_3D -> point_3D
                             -> (point_3D -> point_3D -> float) -> float
(*[distance_lin_point_3D l p d] returns the distance between the point p
 *and the line l, using the distance d, the distance between a point p and a
 *line l been the distance between the point p and the point of the line l
 *closest to p
 *)

val angle_vector_3D : point_3D -> point_3D -> float
(*
 *[angle_vector_3D v1 V2] returns the angle between the vectors v1 v2 as they
 *part of the origin
 *)

(*
val to_3D : point_2D -> point_3D
(*[to_3D p=(x,y)] returns the three dimensional point (x,y,0)*)
*)
