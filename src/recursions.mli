open Geometry_tools
open Choice
open Flatness_criterion
open BatDllist
open Printf
open LDlist_manipulation

val recursion_2D : fonction:(float -> float) -> lDlist:
                  (float option ref * float option ref)
                  BatDllist.node_t -> epsi:float ->
                  test:(point_2D -> point_2D -> point_2D -> float ->
                  (point_2D -> point_2D -> float) -> bool) -> dist:
                  (point_2D -> point_2D -> float) -> unit
(*[recursion_2D fonction:f lDlist:dl epsi:e test:t dist:d] recursive function,
 *evaluate any two consecutives nodes of the linked list dl with the test t
 *using the distance d, the soil of tolerance is e, if the test is negative,
 *creates a new node in between, this function is to be used with the functions
 *from R to R
 *)

val recursion_3D : fonction:(float -> float * float) -> lDlist:
                  (float option ref * float option ref * float option ref)
                  BatDllist.node_t -> epsi:float ->
                  test:(point_3D -> point_3D -> point_3D -> float ->
                  (point_3D -> point_3D -> float) -> bool) -> dist:
                  (point_3D -> point_3D -> float) -> unit
(*[recursion_3D fonction:f lDlist:dl epsi:e test:t dist:d] recursive function,
 *evaluate any two consecutives nodes of the linked list dl with the test t
 *using the distance d, the soil of tolerance is e, if the test is negative,
 *creates a new node in between, this function is to be used with the functions
 *from R to R^2
 *)
