open Geometry_tools
open Choice
open Flatness_criterion
open Recursions

open BatList
open BatDllist
open Printf
open LDlist_manipulation
open Utilities
(*
 * possibles test:
   * cord_test
   * distance_point_cord_test
   * triangle_area_test
 * possibles dist:
   * dist1
   * dist2
   * dist2_variant
   * dist_infinity
 *)

(*
 *constructor model:
 *[samplin_DD ~left:float ~right:float ~fonction:(float ->...) ?epsi:float
 *?test:(Flatness_criterion.t) ?dist:(Geometry_tools.d) ?d_points:(float list)
 *)

let sampling_2D ~left:a ~right:b ~fonction:f ~epsi:e
                ?test:(t=Flatness_criterion.distance_point_cord_test_2D)
                ?dist:(d=Geometry_tools.dist2_variant_2D) ?d_points:(l=[]) () =
    let n1 = if 0.1 < ((abs_float (b-.a))/.50.) then 0.1 else
                      ((abs_float (b-.a))/.50.) in
    let n = int_of_float (1./.n1) in
    let dl = LDlist_manipulation.create_dlist_2D ~cuts:n ~left:a ~right:b
             ~fonction:f ~inlist:l in
    Recursions.recursion_2D ~fonction:f ~lDlist:dl ~epsi:e ~test:t ~dist:d;
    dl

let preset_sampling_2D ~left:a ~right:b ~fonction:f ?test:(t=1) ?mode:(m='m')
                       ?epsi:(f_e=1.) ()=
    let n = (if m='l' then 75 else if m='m' then 120 else if m='h'
             then 200 else failwith "invalid mode") in
    let r_e = 1./. (float n) in
    match t with
    |1 -> sampling_2D ~left:a ~right:b ~fonction:f ~epsi:(f_e*.r_e*.1e-6)
               ~test:Flatness_criterion.distance_point_cord_test_2D
                ~dist:Geometry_tools.dist2_variant_2D ()
    |2 -> sampling_2D ~left:a ~right:b ~fonction:f ~epsi:(f_e*.r_e*.1e-7)
                ~test:Flatness_criterion.triangle_area_test_2D
                ~dist:Geometry_tools.dist_infinity_2D ()
    |3 -> sampling_2D ~left:a ~right:b ~fonction:f ~epsi:(f_e*.r_e*.1.)
                ~test:Flatness_criterion.triangle_area_test_2D
                ~dist:Geometry_tools.dist1_2D ()
    |4 -> sampling_2D ~left:a ~right:b ~fonction:f ~epsi:(f_e*.r_e*.1e-4)
                ~test:Flatness_criterion.distance_point_cord_test_2D
                ~dist:Geometry_tools.dist_infinity_2D ()
    |5 -> sampling_2D ~left:a ~right:b ~fonction:f ~epsi:(f_e*.1e-1)
                ~test:Flatness_criterion.angle_test_2D
                ~dist:Geometry_tools.dist2_variant_2D ()
    |6 -> sampling_2D ~left:a ~right:b ~fonction:f ~epsi:(f_e*.1e-1)
                ~test:Flatness_criterion.angle_test_2D
                ~dist:Geometry_tools.dist1_2D ()
    |_ -> failwith "invalid test to run"



let sampling_3D ~left:a ~right:b ~fonction:f ~epsi:e
                ?test:(t=Flatness_criterion.distance_point_cord_test_3D)
                ?dist:(d=Geometry_tools.dist2_variant_3D) ?d_points:(l=[]) () =
    let n1 = if 0.1 < ((abs_float (b-.a))/.50.) then 0.1 else
                      ((abs_float (b-.a))/.50.) in
    let n = int_of_float (1./.n1) in
    let dl = LDlist_manipulation.create_dlist_3D ~cuts:n ~left:a ~right:b
             ~fonction:f ~inlist:l in
    Recursions.recursion_3D ~fonction:f ~lDlist:dl ~epsi:e ~test:t ~dist:d;
    dl

let preset_sampling_3D ~left:a ~right:b ~fonction:f ?test:(t=1) ?mode:(m='m')
                       ?epsi:(f_e=4.) ()=
    let n = (if m='l' then 75 else if m='m' then 120 else if m='h'
             then 200 else failwith "invalid mode") in
    let r_e = 1./. (float n) in
    match t with
    |1 -> sampling_3D ~left:a ~right:b ~fonction:f ~epsi:(f_e*.r_e*.1e-4)
               ~test:Flatness_criterion.distance_point_cord_test_3D
                ~dist:Geometry_tools.dist2_variant_3D ()
    |2 -> sampling_3D ~left:a ~right:b ~fonction:f ~epsi:(f_e*.r_e*.1e-5)
                ~test:Flatness_criterion.triangle_area_test_3D
                ~dist:Geometry_tools.dist_infinity_3D ()
    |3 -> sampling_3D ~left:a ~right:b ~fonction:f ~epsi:(f_e*.r_e*.10.)
                ~test:Flatness_criterion.triangle_area_test_3D
                ~dist:Geometry_tools.dist1_3D ()
    |4 -> sampling_3D ~left:a ~right:b ~fonction:f ~epsi:(f_e*.r_e*.1e-2)
                ~test:Flatness_criterion.distance_point_cord_test_3D
                ~dist:Geometry_tools.dist_infinity_3D ()
    |5 -> sampling_3D ~left:a ~right:b ~fonction:f ~epsi:(f_e*.1e-1)
                ~test:Flatness_criterion.angle_test_3D
                ~dist:Geometry_tools.dist2_variant_3D ()
    |6 -> sampling_3D ~left:a ~right:b ~fonction:f ~epsi:(f_e*.1e-1)
                ~test:Flatness_criterion.angle_test_3D
                ~dist:Geometry_tools.dist1_3D ()
    |_ -> failwith "invalid test to run"
;;
