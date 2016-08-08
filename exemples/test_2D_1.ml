open Internals;;

let f t = t**8.;;

let dl = Sampling.preset_sampling_2D ~left:(-.1.) ~right:1. 
                                     ~fonction:f ();;

Utilities.to_file ~text:(LDlist_manipulation.to_string_2D dl ())
~file:"test_2D_1.txt";;
Tikz_trans.to_tikz_2D ~lDlist:dl ~file:"test_2D_1" ();;