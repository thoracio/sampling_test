open Internals;;

let f t = t**(1./.5.);;

let dl = Sampling.preset_sampling_2D ~left:(0.) ~right:8. ~test:2
                                     ~mode:'m' ~fonction:f ();;

Utilities.to_file ~text:(LDlist_manipulation.to_string_2D dl ())
~file:"test_2D_2.txt";;
Tikz_trans.to_tikz_2D ~lDlist:dl ~file:"test_2D_2" ();;