open Internals;;
open Printf;;

let f t = (sin t, cos t);;

let dl = Sampling.preset_sampling_3D ~left:(-.1.) ~right:1. ~test:4
                                     ~mode:'h' ~fonction:f ();;

Utilities.to_file ~text:(LDlist_manipulation.to_string_3D dl ())
~file:"test_3D_1.txt";;
Tikz_trans.to_tikz_3D ~lDlist:dl ~file:"test_3D_1" ();;