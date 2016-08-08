val to_tikz_2D : lDlist:(float option ref * float option ref)
                 BatDllist.node_t -> file:string -> ?divisions:int ->
                 unit -> int
(*[to_tikz_2D lDlist:dl file:name ?divisions:d ()] writes the
 *necessary code into a .tex file to visualize the data from
 *the linked list dl
 *)

val to_tikz_3D : lDlist:
                 (float option ref*float option ref*float option ref)
                 BatDllist.node_t -> file:string -> ?divisions:int ->
                 unit -> int
(*[to_tikz_3D lDlist:dl file:name ?divisions:d ()] writes the
 *necessary code into a .tex file to visualize the data from
 *the linked list dl
 *)