open Geometry_tools

val cord_test_2D : point_2D -> point_2D -> point_2D -> float
                   -> (point_2D -> point_2D -> float) -> bool
(*[cord_test_2D p1 p2 p3 e d] returns true if, using the distance d,
 *the difference between the sum of the lengths of the cords p1-p3
 *and p2-p3 and the length of the cord p1-p2 is less than e in
 *absolute value, that is, abs((len(p1-p3)+len(p2-p3))-len(p1-p2)) < e
 *)

val distance_point_cord_test_2D : point_2D -> point_2D -> point_2D -> float
                                  -> (point_2D -> point_2D -> float)
                                  -> bool
(*[distance_point_cord_test p1 p2 p3 e d] returns true if, using the
 *distance d, the distance between the point p3 and the line definded
 *by the points p1 p2 is less than e
 *)

val triangle_area_test_2D : point_2D -> point_2D -> point_2D -> float
                            -> (point_2D -> point_2D -> float) -> bool
(*[triangle_area_test p1 p2 p3 e d] returns if, using the distance d, the
 *area of the triangle defined by p1, p2 and p3 is less than e
 *)

val angle_test_2D : point_2D -> point_2D -> point_2D -> float
                             -> (point_2D -> point_2D -> float) -> bool
(*[angle_test p1 p2 p3 e] returns true if the angle defined by the vectors
 *p1-p3 and p2-p3 is close to 180 degres given in radians, that is, close to
 * pi with e as margin
 *)

(*if the function is supposed deriveable, test if the tangents are almost
 * parallels, returns true if the difference between 1 and the ration
 * between the derivative of the extremes of the interval is less than espi,
 * false otherwise*)
val derivative_parallel_test : (float -> float) -> float -> float -> float ->
  bool

val cord_test_3D : point_3D -> point_3D -> point_3D -> float
                   -> (point_3D -> point_3D -> float) -> bool
(*[cord_test_2D p1 p2 p3 e d] returns true if, using the distance d,
 *the difference between the sum of the lengths of the cords p1-p3
 *and p2-p3 and the length of the cord p1-p2 is less than e in
 *absolute value, that is, abs((len(p1-p3)+len(p2-p3))-len(p1-p2)) < e
 *)

val distance_point_cord_test_3D : point_3D -> point_3D -> point_3D -> float
                                  -> (point_3D -> point_3D -> float)
                                  -> bool
(*[distance_point_cord_test p1 p2 p3 e d] returns true if, using the
 *distance d, the distance between the point p3 and the line definded
 *by the points p1 p2 is less than e
 *)

val triangle_area_test_3D : point_3D -> point_3D -> point_3D -> float
                            -> (point_3D -> point_3D -> float) -> bool
(*[triangle_area_test p1 p2 p3 e d] returns if, using the distance d, the
 *area of the triangle defined by p1, p2 and p3 is less than e
 *)

 val angle_test_3D : point_3D -> point_3D -> point_3D -> float
                             -> (point_3D -> point_3D -> float) -> bool
(*[angle_test p1 p2 p3 e] returns true if the angle defined by the vectors
 *p1-p3 and p2-p3 is close to 180 degres given in radians, that is, close to
 * pi with e as margin
 *)

