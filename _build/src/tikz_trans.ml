open Sampling
open Printf
open BatDllist
open BatOption
open BatList
open LDlist_manipulation

let rec create_axis_helper ~left:a ~right:b ~divisions:d ~step:i ~inlist:l =
	if i=d then begin
	  let step_a = (b-.a) /. (float d) in
	  let str = Printf.sprintf "%1.f}" (a+. (float i)*.step_a) in
    (BatList.append l (BatList.singleton str))
    end
    else if i=0 then begin
	  let step_a = (b-.a) /. (float d) in
	  let str = Printf.sprintf "{%1.f," (a+. (float i)*.step_a) in
	  let l_str = BatList.singleton str in
    create_axis_helper ~left:a ~right:b ~divisions:d ~step:(i+1)
	  ~inlist:(BatList.append l l_str)
    end
    else begin
	  let step_a = (b-.a) /. (float d) in
	  let str = Printf.sprintf "%1.f," (a+. (float i)*.step_a) in
	  let l_str = BatList.singleton str in
	  create_axis_helper ~left:a ~right:b ~divisions:d ~step:(i+1)
	  ~inlist:(BatList.append l l_str)
    end

let create_axis ~left:a ~right:b ~divisions:d=
	let l_str = create_axis_helper ~left:a ~right:b ~divisions:d
	          ~step:0 ~inlist:[] in
	(BatString.concat "" l_str)

let to_tikz_2D_helper ~lDlist:dl ~x_min_b:x_min ~x_max_b:x_max ~y_min_b:y_min
               ~y_max_b:y_max ~file:name ?color:(c="black") ?divisions:(d=20)
               ?font_size:(f_s=10) () =
  let f_out = open_out (BatString.concat "" [name;".tex"]) in
  let scale = if (x_max-.x_min) >= (y_max-.y_min) then (x_max-.x_min)
              else (y_max-.y_min) in
  fprintf f_out "\\documentclass[%i]{article}\n" f_s;
  fprintf f_out "\\usepackage{pgf, tikz}\n";
  fprintf f_out "\\usetikzlibrary{arrows}\n";
  fprintf f_out "\\pagestyle{empty}\n";
  fprintf f_out "\\begin{document}\n";
  fprintf f_out "\\begin{tikzpicture}[line cap=round,line join=round,>=triangle
                  45,x=%fcm,y=%fcm]\n" (20.*.(1./.scale)) (20.*.(1./.scale));
  fprintf f_out "\\draw[->,color=black] (%f,0)--(%f,0) ;\n" x_min x_max;
  fprintf f_out "\\foreach \\x in ";
  output_string f_out (create_axis ~left:x_min ~right:x_max ~divisions:d);
  fprintf f_out "\n";
  fprintf f_out "\\draw[shift={(\\x,0)},color=black] (0pt,2pt) -- (0pt,-2pt) 
                node[below] {\\footnotesize $\\x$};\n";
  fprintf f_out "\\draw[->,color=black] (0,%f) -- (0,%f);\n" y_min y_max;
  fprintf f_out "\\foreach \\y in ";
  output_string f_out (create_axis ~left:y_min ~right:y_max ~divisions:d);
  fprintf f_out "\n";
  fprintf f_out "\\draw[shift={(0,\\y)},color=black] (2pt,0pt) -- (-2pt,0pt) 
                 node[left] {\\footnotesize $\\y$};\n";
  fprintf f_out "\\draw[color=black] (0pt,-10pt) node[right] {\\footnotesize 
                 $0$};";
  fprintf f_out "\\clip(%f,%f) rectangle (%f,%f);\n" x_min y_min x_max y_max;
  fprintf f_out "\\draw ";
  output_string f_out (LDlist_manipulation.to_string_2D ~lDlist:dl ~l_e_b:""
                 ~r_e_b:"" ~e_s:"--" ~i_s:","
                 ~l_i_b:"(" ~r_i_b:")" ());
  fprintf f_out ";\n";

  fprintf f_out "\\end{tikzpicture}\n";
  fprintf f_out "\\end{document}";
  close_out f_out
;;

let to_tikz_2D ~lDlist:dl ~file:name ?divisions:(d=10) ()=
  let (x_min, x_max, y_min, y_max) = LDlist_manipulation.minimax_2D dl in
  let (x_min_b, x_max_b, y_min_b, y_max_b) =
      (x_min -. 0.5, x_max +. 0.5, y_min -. 0.5, y_max +. 0.5) in
  to_tikz_2D_helper ~lDlist:dl ~x_min_b:x_min_b ~x_max_b:x_max_b ~y_min_b:y_min_b
               ~y_max_b:y_max_b ~file:name ~divisions:d ();
  let cmd_1 = BatString.concat "" ["pdflatex "; name; ".tex"] in
  let cmd_2 = BatString.concat ""
              ["rm -r "; name; ".aux && rm -r "; name; ".log"] in
  let cmd_3 = BatString.concat "" ["xdg-open "; name; ".pdf"] in
  Sys.command cmd_1;
  Sys.command cmd_2;
  Sys.command cmd_3;
;;

let to_tikz_3D_helper ~lDlist:dl ~x_min_b:x_min ~x_max_b:x_max ~y_min_b:y_min
               ~y_max_b:y_max ~z_min_b:z_min ~z_max_b:z_max ~file:name
               ?color:(c="black") ?divisions:(d=20)
               ?font_size:(f_s=10) () =
  let f_out = open_out (BatString.concat "" [name;".tex"]) in
  let scale = (if (x_max-.x_min)>=(y_max-.y_min) && 
                 (x_max-.x_min)>=(z_max-.z_min) then (x_max-.x_min) 
               else if (y_max-.y_min)>=(x_max-.x_min) && 
                 (y_max-.y_min)>=(z_max-.z_min) then (y_max-.y_min)
               else (z_max-.z_min) )in
  fprintf f_out "\\documentclass[%i]{article}\n" f_s;
  fprintf f_out "\\usepackage{pgf, tikz}\n";
  fprintf f_out "\\usetikzlibrary{arrows}\n";
  fprintf f_out "\\pagestyle{empty}\n";
  fprintf f_out "\\begin{document}\n";
  fprintf f_out "\\begin{tikzpicture}[line cap=round,line join=round,>=triangle
                  45,x=%fcm,y=%fcm]\n" (20.*.(1./.scale)) (20.*.(1./.scale));
  fprintf f_out "\\draw[thick,->] (%f,0,0) -- (%f,0,0) 
                   node[anchor=north east]{$x$};\n" x_min x_max;
  fprintf f_out "\\foreach \\x in ";
  output_string f_out (create_axis ~left:x_min ~right:x_max ~divisions:d);
  fprintf f_out "\n";
  fprintf f_out "\\draw[shift={(\\x,0)},color=black] (0pt,2pt) -- (0pt,-2pt) 
                node[below] {\\footnotesize $\\x$};\n";
  fprintf f_out "\\draw[thick,->] (0,%f,0) -- (0,%f,0) 
                  node[anchor=north west]{$y$};\n" y_min y_max;
  fprintf f_out "\\foreach \\y in ";
  output_string f_out (create_axis ~left:y_min ~right:y_max ~divisions:d);
  fprintf f_out "\n";
  fprintf f_out "\\draw[shift={(0,\\y)},color=black] (2pt,0pt) -- (-2pt,0pt) 
                 node[left] {\\footnotesize $\\y$};\n";
  fprintf f_out "\\draw[color=black] (0pt,-10pt) node[right] {\\footnotesize 
                 $0$};";

  fprintf f_out "\\draw[thick,->] (0,0,%f) -- (0,0,%f) 
                  node[anchor=south]{$z$};\n" z_min z_max;
  fprintf f_out "\\foreach \\z in ";
  output_string f_out (create_axis ~left:z_min ~right:z_max ~divisions:d);
  fprintf f_out "\n";
  fprintf f_out "\\draw[shift={(0,\\z)},color=black] (2pt,0pt) -- (-2pt,0pt) 
                 node[left] {\\footnotesize $\\z$};\n";
  fprintf f_out "\\draw[color=black] (0pt,-10pt) node[right] {\\footnotesize 
                 $0$};";

  fprintf f_out "\\clip(%f,%f) rectangle (%f,%f);\n" x_min y_min x_max y_max;
  fprintf f_out "\\draw ";
  output_string f_out (LDlist_manipulation.to_string_3D ~lDlist:dl ~l_e_b:""
                 ~r_e_b:"" ~e_s:"--" ~i_s:","
                 ~l_i_b:"(" ~r_i_b:")" ());
  fprintf f_out ";\n";

  fprintf f_out "\\end{tikzpicture}\n";
  fprintf f_out "\\end{document}";
  close_out f_out;
  let cmd_1 = BatString.concat "" ["pdflatex "; name; ".tex"] in
  let cmd_2 = BatString.concat ""
              ["rm -r "; name; ".aux && rm -r "; name; ".log"] in
  let cmd_3 = BatString.concat "" ["xdg-open "; name; ".pdf"] in
  Sys.command cmd_1;
  Sys.command cmd_2;
  Sys.command cmd_3;
;;


let to_tikz_3D ~lDlist:dl ~file:name ?divisions:(d=20) ()=
  let (x_min, x_max, y_min, y_max, z_min, z_max) =
       LDlist_manipulation.minimax_3D dl in
  let (x_min_b, x_max_b, y_min_b, y_max_b, z_min_b, z_max_b) =
      (x_min-.0.5,x_max+.0.5,y_min-.0.5,y_max+.0.5,z_min-.0.5,z_max+.0.5) in
  to_tikz_3D_helper ~lDlist:dl ~x_min_b:x_min_b ~x_max_b:x_max_b ~y_min_b:y_min_b
               ~y_max_b:y_max_b ~z_min_b:z_min_b ~z_max_b:z_max_b 
               ~file:name ~divisions:d ()
;;
