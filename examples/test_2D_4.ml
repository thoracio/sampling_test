open Internals;;
open Printf;;

let f t = sin (1.2/.(t +. 0.3));;

let dl = Sampling.preset_sampling_2D ~left:(0.) ~right:3. ~test:4
                                     ~mode:'h' ~fonction:f ();;

Utilities.to_file ~text:(LDlist_manipulation.to_string_2D dl ())
~file:"test_2D_4.txt";;
Tikz_trans.to_tikz_2D ~lDlist:dl ~file:"test_2D_4" ();;