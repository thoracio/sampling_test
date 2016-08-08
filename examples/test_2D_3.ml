open Internals;;

let f t = sqrt (1. -. t**2.);;

let dl = Sampling.preset_sampling_2D ~left:(-.1.) ~right:1. ~test:5
                                     ~mode:'h' ~fonction:f ();;

Utilities.to_file ~text:(LDlist_manipulation.to_string_2D dl ())
~file:"test_2D_3.txt";;
Tikz_trans.to_tikz_2D ~lDlist:dl ~file:"test_2D_3" ();;