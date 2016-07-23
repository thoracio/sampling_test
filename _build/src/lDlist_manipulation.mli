(*we use the BatDllist module for our double linked lists, the double lists in
 *BatDllist are cyclic, so the last element points to the first, we use them
 *to store the values (x,fx) in the form
 *(ref BatOption x, ref BatOption fx)
 *where fx may be None if x haven not been evaluated yet,
 *for the function from R to R^2 is the same with
 *(ref BatOption x, ref BatOption f1_x, ref BatOption f2_x)
 *)





val update_2D : fonction:(float -> float)
                      -> node:(float option ref * float option ref)
                      BatDllist.node_t-> unit
(*[update_2D fonction:f node:n=(ref option x, ref option y)] if the value in y is
 *None, the value is updated with (f x)
 *)

val is_last_2D : node:(float option ref * float option ref)
              BatDllist.node_t -> bool
(*[is_last_2D node:n=(ref option x, ref option fx)] returns true if x>y, where
 * n_next=(ref option y, ref option fy) is the node after n
 *)

val create_dlist_2D : cuts:int -> left:float -> right:float
                      -> fonction:(float -> float) -> inlist:(float) list
                      -> (float option ref * float option ref) BatDllist.node_t
(*[create_dlist_2D cuts:n left:a right:b fonction:f inlist:l] returns a double
 *linked list of nodes (x,fx) of a subdivision with at least n+1 elements and
 if the list l is not empty, their elements are inserted respecting the order
 *)

val to_list_tuple_2D : lDlist:(float option ref * float option ref)
                       BatDllist.node_t -> (float * float) list
(*[to_list_tuple_2D lDlist:dl] returns a list where the elements are of the
 *form (x,y) where the nodes of dl are of the form (ref option x, ref option y)
 *)

val to_list_2D : lDlist:(float option ref * float option ref)
                BatDllist.node_t -> (float) list
(*[to_list_2D lDlist:dl] returns a list where the elements are of the form x
 *where the nodes of dl are of the form (ref option x, ref option y)
 *)

val minimax_2D : lDlist:(float option ref * float option ref)
                   BatDllist.node_t -> (float * float * float * float)
(*[minimax_2D lDlist:dl] returns (x_min, x_max, y_min, y_max) the minimum
 *and maximum values for the linked list dl
 *)

val to_string_2D : lDlist:(float option ref * float option ref)
                   BatDllist.node_t -> ?l_e_b:string ->
                   ?r_e_b:string -> ?e_s:string -> ?i_s:string ->
                   ?l_i_b:string -> ?r_i_b:string -> unit-> string
(*[to_string_2D lDlist:dl ?l_e_b:leb ?r_e_b:reb ?e_s:es ?l_i_b:lib
 *?r_i_b:rib ?i_s:is ()] returns a string of the form
    "leb lib x1 is y1 rib ... rib lib xn is yn rib reb"
    the default values are
    "{(x1,y1), ... , (xn,y1)}"
 *where the nodes of dl are of the form (ref option x, ref option y)
 *)

val update_3D : fonction:(float -> float*float)
                      -> node:(float option ref * float option ref *
                      float option ref) BatDllist.node_t -> unit
(*[update_3D fonction:f node:n=(ref option x, ref option y, ref option z)] if the
 *value in y is None or the value in z in None, the value of y is updated with
 *(f1 x) and the value of z is updated with (f2 x) where f(t)=(f1 t, f2 t)
 *)

val is_last_3D : node:(float option ref * float option ref * float option ref)
                 BatDllist.node_t -> bool
(*[is_last_3D node:n=(ref option x, ref option f1x, ref option f2x)] returns true
 *if x>y, where n_next=(ref option y, ref option f1y, ref option f2y) is
 *the node after n
 *)

val create_dlist_3D : cuts:int -> left:float -> right:float
                      -> fonction:(float -> float*float) -> inlist:(float) list
                      -> (float option ref*float option ref*float option ref)
                      BatDllist.node_t
(*[create_dlist_3D cuts:n left:a right:b fonction:f inlist:l] returns a double
 *linked list of nodes (x, f1x, f2x) of a subdivision with at least n+1
 *elements and if the list l is not empty, their elements are inserted
 *respecting the order
 *)

val to_list_tuple_3D : lDlist:
                       (float option ref * float option ref * float option ref)
                       BatDllist.node_t -> (float * float * float) list
(*[to_list_tuple_3D lDlist:dl] returns a list where the elements are of the
 *form (x,y,z) where the nodes of dl are of the form
 *(ref option x, ref option y, ref option y)
 *)

val to_list_3D : lDlist:(float option ref * float option ref * float option ref)
                BatDllist.node_t -> (float) list
(*[to_list_3D lDlist:dl] returns a list where the elements are of the form x
 *where the nodes of dl are of the form
 *(ref option x, ref option y, ref option z)
 *)

val minimax_3D : lDlist:(float option ref * float option ref * float option ref)
                   BatDllist.node_t ->
                   (float * float * float * float * float * float)
(*[minimax_3D lDlist:dl] returns (x_min, x_max, y_min, y_max, z_min, z_max) the
 *minimum and maximum values for each for the linked list dl
 *)

val to_string_3D : lDlist:(float option ref * float option ref * float option ref)
                   BatDllist.node_t -> ?l_e_b:string ->
                   ?r_e_b:string -> ?e_s:string -> ?i_s:string -> ?l_i_b:string ->
                   ?r_i_b:string ->  unit -> string
(*[to_string_3D lDlist:dl ?l_e_b:leb ?r_e_b:reb ?e_s:es ?l_i_b:lib
 *?r_i_b:rib ?i_s:is ()] returns a string of the form
 *  "leb lib x1 is y1 is z1 rib ... rib lib xn is yn is zn rib reb"
 *   the default values are
 *   "{(x1,y1,z1), ... , (xn,yn,zn)}"
 *where the nodes of dl are of the form
 *(ref option x, ref option y, ref option z)
 *)
