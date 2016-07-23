open Geometry_tools

val sampling_2D : left:float -> right:float -> fonction:(float -> float) ->
                  epsi:float -> ?test:(point_2D -> point_2D -> point_2D ->
                  float -> (point_2D -> point_2D -> float) -> bool) ->
                  ?dist:(point_2D -> point_2D -> float) ->
                  ?d_points:(float)list -> unit ->
                  (float option ref * float option ref) BatDllist.node_t
(*[recursion_2D left:a right:b fonction:f epsi:e ?test:t ?dist:d ()] creates a
 *BatDllist of the form (ref option x, ref option fx) by sampling the interval
 *[a,b] for the function f, using the test Flatness_criterion.t, the distance
 *Geometry_tools.d and epsilon e as soil, to be used with functions
 *from R to R
 *)

val preset_sampling_2D : left:float -> right:float ->
                         fonction:(float -> float) -> ?test:int ->
                         ?mode:char -> ?epsi:float -> unit ->
                         (float option ref * float option ref)
                         BatDllist.node_t
(*[preset_sampling_2D left:a right:b fonction:f ?test:t ?mode:m ?epsi]
 *as sampling_2D returns a BatDllist by sampling the interval [a,b] for
 *the function f, it has a predetermined combination of
 *Flatness_criterion.t tests ans Geometry_tools.d distances:
 *t=1: distance_point_cord_test, dist2_variant
 *t=2: triangle_area_test, dist_infinity
 *t=3: triangle_area_test, dist1
 *t=4: distance_point_cord_test, dist_infinity
 *t=5: angle_test, dist2_variant
 *t=6: angle_test, dist1,
 *the value mode has the values:
 *'l' for low curvature functions
 *'m' for medium curvature functions
 *'h' for high curvature functions
*)

val sampling_3D : left:float -> right:float ->  fonction:(float -> float*float)
                  -> epsi:float -> ?test:(point_3D -> point_3D -> point_3D ->
                  float -> (point_3D -> point_3D -> float) -> bool) ->
                  ?dist:(point_3D -> point_3D -> float) -> 
                  ?d_points:(float)list -> unit ->
                  (float option ref * float option ref * float option ref)
                  BatDllist.node_t
(*[recursion_3D left:a right:b fonction:f epsi:e ?test:t ?dist:d ()] creates a
 *BatDllist of the form (ref option x, ref option f1x, ref option f2x) by
 *sampling the interval [a,b] for the function f, using the test
 *Flatness_criterion.t, the distance Geometry_tools.d and epsilon e as soil,
 *to be used with functions from R to R^2
 *)

 val preset_sampling_3D : left:float -> right:float ->
                         fonction:(float -> float*float) -> ?test:int ->
                         ?mode:char -> ?epsi:float -> unit ->
                         (float option ref*float option ref*float option ref)
                         BatDllist.node_t
(*[preset_sampling_3D left:a right:b fonction:f ?test:t ?mode:m ?epsi]
 *as sampling_3D returns a BatDllist by sampling the interval [a,b] for
 *the function f, it has a predetermined combination of
 *Flatness_criterion.t tests ans Geometry_tools.d distances:
 *t=1: distance_point_cord_test, dist2_variant
 *t=2: triangle_area_test, dist_infinity
 *t=3: triangle_area_test, dist1
 *t=4: distance_point_cord_test, dist_infinity
 *t=5: angle_test, dist2_variant
 *t=6: angle_test, dist1,
 *the value mode has the values:
 *'l' for low curvature functions
 *'m' for medium curvature functions
 *'h' for high curvature functions
*)
