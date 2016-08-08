open Internals;;
open Printf;;

let f t = if t=0. then (0.,0.) else (sin (1./.t), exp (sin (1./.t)));;

let dl = Sampling.preset_sampling_3D ~left:(0.1) ~right:4. ~test:4
                                     ~mode:'h' ~fonction:f ~epsi:4. ();;

Utilities.to_file ~text:(LDlist_manipulation.to_string_3D dl ())
~file:"test_3D_4.txt";;
Tikz_trans.to_tikz_3D ~lDlist:dl ~file:"test_3D_4" ();;