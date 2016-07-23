open Internals;;
open Printf;;

let f t = if t=0. then (0.,0.) else (sin (1./.t), exp t);;

let dl = Sampling.preset_sampling_3D ~left:(0.01) ~right:3. ~test:4
                                     ~mode:'h' ~fonction:f ~epsi:7. ();;

Utilities.to_file ~text:(LDlist_manipulation.to_string_3D dl ())
~file:"test_3D_4.txt";;
Tikz_trans.to_tikz_3D ~lDlist:dl ~file:"test_3D_4" ();;
