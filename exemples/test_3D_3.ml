open Internals;;
open Printf;;

let f t = (10.*.(sin t)/.t, 10.*.(1.-. (cos t)/.t));;

let dl = Sampling.preset_sampling_3D ~left:(0.1) ~right:20. ~test:4
                                     ~mode:'h' ~fonction:f ~epsi:4. ();;

Utilities.to_file ~text:(LDlist_manipulation.to_string_3D dl ())
~file:"test_3D_3.txt";;
Tikz_trans.to_tikz_3D ~lDlist:dl ~file:"test_3D_3" ();;