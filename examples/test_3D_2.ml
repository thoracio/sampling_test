open Internals;;
open Printf;;

let f t = (t*.(sin t)/.10., t*.(cos t)/.10.);;

let dl = Sampling.preset_sampling_3D ~left:(0.5) ~right:20. ~test:4
                                     ~mode:'h' ~fonction:f ();;

Utilities.to_file ~text:(LDlist_manipulation.to_string_3D dl ())
~file:"test_3D_2.txt";;
Tikz_trans.to_tikz_3D ~lDlist:dl ~file:"test_3D_2" ();;