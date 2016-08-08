open BatDllist
open BatList
open Printf
(*
 we use the BatDllist module, our linked lists have an specific form,
 each node is of the form
 (ref (BatOption float), ref (BatOption float))
 where
 (x, fx)
 is the form we associate
 all of the functions here defined are for this specific form
 *)

(*
the node have two fields, one for the argument, one for the image
 if the image haven't been calculated yet, we update the value
 *)
let update_2D ~fonction:f ~node:n =
  let (a,b) = BatDllist.get n in
  if BatOption.is_none (!b) then b:= Some ( f (BatOption.get (!a))) (*begin
    let fa = f (BatOption.get (!a)) in
    if (BatFloat.is_special fa) then
      b:= Some 0.
    else
      b:= Some fa
  end*)

(*
as the linked list in BatDlist are cyclique, in our case the last element is
the one who has greater value than the next node
*)
let is_last_2D ~node:n =
   let n_next = BatDllist.next n in
   let (n_value, f_n_value) = BatDllist.get n in
   let (n_next_value, f_n_next_value) = BatDllist.get n_next in
   let n_float = BatOption.get (!n_value) in
   let n_next_float = BatOption.get(!n_next_value) in
   if n_float=n_next_float then true else n_float > n_next_float

(*helper function for the create_dlist_2D function*)
let rec create_dlist_helper_2D ~cuts:n ~step:i ~step_size:s ~left:a ~right:b
                               ~fonction:f ~inlist:l =
  if n <= 0 then failwith "the number on divisions must be positive"
  else if i < 0 || i > n then failwith "invalid number of steps"
  else if (i=n && (BatList.is_empty l)) then begin
    BatDllist.create (ref (Some b), ref (Some (f b))) end
  else if (i=n && not(BatList.is_empty l)) then begin
    let l_value = BatList.at l 0 in
    let f_l_value = f l_value in
    let l_node = BatDllist.create (ref (Some l_value),ref (Some f_l_value)) in
    (BatDllist.splice l_node (create_dlist_helper_2D ~cuts:n ~step:i
                             ~step_size:s ~left:a ~right:b ~fonction:f
                             ~inlist:(BatList.tl l)));
    l_node end
  else begin
    let i_value = a+.(float i)*.s in
    let f_i_value = f i_value in
    let i_node = BatDllist.create (ref(Some i_value),ref(Some f_i_value)) in
    if (BatList.is_empty l) then begin
      (BatDllist.splice i_node (create_dlist_helper_2D ~cuts:n ~step:(i+1)
                               ~step_size:s ~left:a ~right:b
                               ~fonction:f ~inlist:l));
    i_node
    end
    else if not(BatList.is_empty l) && (BatList.at l 0) > i_value then begin
      (BatDllist.splice i_node (create_dlist_helper_2D ~cuts:n ~step:(i+1)
                               ~step_size:s ~left:a ~right:b
                               ~fonction:f ~inlist:l));
      i_node
    end
    else if not(BatList.is_empty l) && (BatList.at l 0) = i_value then begin
      (BatDllist.splice i_node (create_dlist_helper_2D ~cuts:n ~step:(i+1)
                               ~step_size:s ~left:a ~right:b ~fonction:f
                               ~inlist:(BatList.tl l)));
      i_node
    end
    else begin
      let l_value = BatList.at l 0 in
      let f_l_value = f l_value in
      let l_node = BatDllist.create (ref (Some l_value),ref (Some f_l_value)) in
      (BatDllist.splice l_node (create_dlist_helper_2D ~cuts:n ~step:(i)
                               ~step_size:s ~left:a ~right:b ~fonction:f
                               ~inlist:(BatList.tl l)));
      l_node
    end
  end

(*
creates a double linked list with the subdivisions of the interval
[a,b], inserts the element of the list l if l is not empty
*)
let create_dlist_2D ~cuts:n ~left:a ~right:b ~fonction:f ~inlist:l =
  if n<0 then failwith "the number of divisions must be positive"
  else begin
  let s = (abs_float (b-.a)) /. (float n) in
  create_dlist_helper_2D ~cuts:n ~step:0 ~step_size:s ~left:a ~right:b
                         ~fonction:f ~inlist:l
  end

(*helper function for the to_list2 function*)
let rec to_list_tuple_2D_helper ~lDlist:dl ~inlist:l =
  if (BatDllist.length dl) = 0 then failwith "empty linked list"
  else
    let (ref_some_x, ref_some_y) = BatDllist.get dl in
    let x = BatOption.get (!ref_some_x) in
    let y = BatOption.get (!ref_some_y) in
    if (is_last_2D dl) then (BatList.append l [(x,y)])
    else to_list_tuple_2D_helper ~lDlist:(BatDllist.next dl)
                         ~inlist:(BatList.append l [(x,y)])

let rec minimax_2D_helper ~lDlist:dl ~c_arg_max:arg_max ~c_arg_min:arg_min
                          ~c_img_max:img_max ~c_img_min:img_min =
  let (ref_some_x, ref_some_y) = BatDllist.get dl in
  if (BatOption.is_none !ref_some_x) || (BatOption.is_none !ref_some_y) then
    failwith "the linked list must be complete"
  else begin
  let (x,y) = (BatOption.get !ref_some_x, BatOption.get !ref_some_y) in
  if (is_last_2D dl) then begin
    if x<arg_min && y<img_min then (x, arg_max, y, img_max)
    else if x<arg_min && y>img_max then (x, arg_max, img_min, y)
    else if x>arg_max && y<img_min then (arg_min, x, y, img_max)
    else if x>arg_max && y>img_max then (arg_min, x, img_min, y)
    else (arg_min, arg_max, img_min, img_max)
  end
  else begin
    let next = BatDllist.next dl in
    if x<arg_min && y<img_min then
      minimax_2D_helper ~lDlist:next ~c_arg_max:arg_max ~c_arg_min:x
                        ~c_img_max:img_max ~c_img_min:y
    else if x<arg_min && y>img_max then
      minimax_2D_helper ~lDlist:next ~c_arg_max:arg_max ~c_arg_min:x
                        ~c_img_max:y ~c_img_min:img_min
    else if x>arg_max && y<img_min then
      minimax_2D_helper ~lDlist:next ~c_arg_max:x ~c_arg_min:arg_min
                        ~c_img_max:img_max ~c_img_min:y
    else if x>arg_max && y>img_max then
      minimax_2D_helper ~lDlist:next ~c_arg_max:x ~c_arg_min:arg_min
                        ~c_img_max:y ~c_img_min:img_min
    else if y<img_min then
      minimax_2D_helper ~lDlist:next ~c_arg_max:arg_max ~c_arg_min:arg_min
                        ~c_img_max:img_max ~c_img_min:y
    else if x>arg_max then
      minimax_2D_helper ~lDlist:next ~c_arg_max:x ~c_arg_min:arg_min
                        ~c_img_max:img_max ~c_img_min:img_min
    else if x<arg_min  then
      minimax_2D_helper ~lDlist:next ~c_arg_max:arg_max ~c_arg_min:x
                        ~c_img_max:img_max ~c_img_min:img_min
    else if y>img_max then
      minimax_2D_helper ~lDlist:next ~c_arg_max:x ~c_arg_min:arg_min
                        ~c_img_max:y ~c_img_min:img_min
    else minimax_2D_helper ~lDlist:next ~c_arg_max:arg_max ~c_arg_min:arg_min
                           ~c_img_max:img_max ~c_img_min:img_min
  end
  end

let minimax_2D ~lDlist:dl =
  let (ref_some_x, ref_some_y) = BatDllist.get dl in
  let (x,y) = (BatOption.get !ref_some_x, BatOption.get !ref_some_y) in
  minimax_2D_helper ~lDlist:dl ~c_arg_max:x ~c_arg_min:x ~c_img_max:y
                    ~c_img_min:y




(*create a list of couples of floats from the linked list dl*)
(*this function is to be used for the implicit function sampling*)
let to_list_tuple_2D ~lDlist:dl =
  to_list_tuple_2D_helper ~lDlist:dl ~inlist:[]

let fos f =
  let str = Printf.sprintf "%.20f" f in
  str

(*helper function for the to_list function*)
let rec to_list_2D_helper ~lDlist:dl ~inlist:l =
  if (BatDllist.length dl) = 0 then failwith "empty linked list"
  else
    let (ref_some_x, ref_some_y) = BatDllist.get dl in
    let x = BatOption.get (!ref_some_x) in
    if (is_last_2D dl) then (BatList.append l [x])
    else to_list_2D_helper ~lDlist:(BatDllist.next dl)
                           ~inlist:(BatList.append l [x])

(*same as to_list, returns only a list of floats*)
let to_list_2D ~lDlist:dl = to_list_2D_helper ~lDlist:dl ~inlist:[]


(*
 * create an string containing one node of a lDlist
 *)
let to_string_dlist_2D_step ~node:n ~i_s:i_s ~l_i_b:l_i_b ~r_i_b:r_i_b =
  let (ref_some_x, ref_some_y) = BatDllist.get n in
  let some_x = !ref_some_x in
  let some_y = !ref_some_y in
  if (BatOption.is_none some_x) && (BatOption.is_none some_y) then
    let str = BatString.concat "" [l_i_b; "none" ;i_s; "none"; r_i_b] in
    str
  else if BatOption.is_none some_y then
    let arg = fos (BatOption.get some_x) in
    let str = BatString.concat "" [l_i_b; arg; i_s; "none"; r_i_b] in
    str
  else
    let arg = fos (BatOption.get some_x) in
    let img = fos (BatOption.get some_y) in
    let str = BatString.concat "" [l_i_b; arg; i_s; img; r_i_b] in
    str

(*
 * helper function for the to_string function
 *)
let rec to_string_dlist_2D_helper ~lDlist:l ~r_length:r_len ~length:len
         ~l_e_b:l_e_b ~r_e_b:r_e_b ~e_s:e_s ~i_s:i_s ~l_i_b:l_i_b
         ~r_i_b:r_i_b =
  if len > (BatDllist.length l) then failwith "can't print more element than
                                               thoe in the list"
  else
    match (r_len, len) with
    |(0,0) -> "nothing to print"
    |(1,1) -> let str = BatString.concat ""
                        [l_e_b;(to_string_dlist_2D_step ~node:l ~i_s:i_s
                        ~l_i_b:l_i_b ~r_i_b:r_i_b);r_e_b] in
              str
    |(_,1) -> let str = BatString.concat ""
                        [(to_string_dlist_2D_step ~node:l ~i_s:i_s
                        ~l_i_b:l_i_b ~r_i_b:r_i_b);r_e_b] in
              str
    |(_,_) -> if r_len=len then begin
               let next = BatDllist.next l in
               BatString.concat "" [l_e_b;(to_string_dlist_2D_step ~node:l
                                    ~i_s:i_s ~l_i_b:l_i_b ~r_i_b:r_i_b);e_s;
               (to_string_dlist_2D_helper ~lDlist:next ~r_length:r_len
               ~length:(len -1) ~l_e_b:l_e_b ~r_e_b:r_e_b ~e_s:e_s ~i_s:i_s
               ~l_i_b:l_i_b ~r_i_b:r_i_b)]
              end
              else begin
                let next = BatDllist.next l in
                BatString.concat "" [(to_string_dlist_2D_step ~node:l ~i_s:i_s
                                     ~l_i_b:l_i_b ~r_i_b:r_i_b);e_s;
                (to_string_dlist_2D_helper ~lDlist:next ~r_length:r_len
                ~length:(len -1) ~l_e_b:l_e_b ~r_e_b:r_e_b ~e_s:e_s ~i_s:i_s
                ~l_i_b:l_i_b ~r_i_b:r_i_b)]
              end

(*
 * create an string containtig the lDlist l
 *)
let to_string_2D ~lDlist:l ?l_e_b:(l_e_b="{") ?r_e_b:(r_e_b="}")
                 ?e_s:(e_s=",") ?i_s:(i_s=",") ?l_i_b:(l_i_b="(")
                 ?r_i_b:(r_i_b=")") () =
  to_string_dlist_2D_helper ~lDlist:l ~r_length:(BatDllist.length l)
  ~length:(BatDllist.length l) ~l_e_b:l_e_b ~r_e_b:r_e_b ~e_s:e_s
  ~i_s:i_s ~l_i_b:l_i_b  ~r_i_b:r_i_b
;;

(*for the functions from R to R^2 the nodes are of the form
 * (ref option x, ref option f1_x, ref option f2_x)
 *)
let update_3D ~fonction:f ~node:n =
  let (a,b,c) = BatDllist.get n in
  if BatOption.is_none (!b) || BatOption.is_none (!c) then begin
    let (x,y)= f (BatOption.get (!a)) in
    b := Some x;
    c := Some y
  end

(*
as the linked list in BatDlist are cyclique, in our case the last element is the one who has greater value than the next node
*)
let is_last_3D ~node:n =
   let n_next = BatDllist.next n in
   let (n_value, f1_n_value, f2_n_value) = BatDllist.get n in
   let (n_next_value,f1_n_next_value,f2_n_next_value)=BatDllist.get n_next in
   let n_float = BatOption.get (!n_value) in
   let n_next_float = BatOption.get(!n_next_value) in
   if n_float = n_next_float then true else n_float > n_next_float

(*helper function for the create_dlist_3D function*)
let rec create_dlist_helper_3D ~cuts:n ~step:i ~step_size:s ~left:a ~right:b
                               ~fonction:f ~inlist:l =
  if n <= 0 then failwith "the number on divisions must be positive"
  else if i < 0 || i > n then failwith "invalid number of steps"
  else if (i=n && (BatList.is_empty l)) then begin
    let (f1_b, f2_b) = f b in
    BatDllist.create (ref (Some b), ref (Some f1_b), ref (Some f2_b)) end
  else if (i=n && not(BatList.is_empty l)) then begin
    let l_value = BatList.at l 0 in
    let (f1_l_value, f2_l_value) = f l_value in
    let l_node = BatDllist.create (ref(Some l_value),
                                  ref(Some f1_l_value),ref(Some f2_l_value)) in
    (BatDllist.splice l_node (create_dlist_helper_3D ~cuts:n ~step:i
                             ~step_size:s ~left:a ~right:b ~fonction:f
                             ~inlist:(BatList.tl l)));
    l_node end
  else begin
    let i_value = a+.(float i)*.s in
    let (f1_i_value, f2_i_value) = f i_value in
    let i_node = BatDllist.create (ref(Some i_value),
                                  ref(Some f1_i_value),ref(Some f2_i_value)) in
    if (BatList.is_empty l) then begin
      (BatDllist.splice i_node (create_dlist_helper_3D ~cuts:n ~step:(i+1)
                               ~step_size:s ~left:a ~right:b
                               ~fonction:f ~inlist:l));
    i_node
    end
    else if not(BatList.is_empty l) && (BatList.at l 0) > i_value then begin
      (BatDllist.splice i_node (create_dlist_helper_3D ~cuts:n ~step:(i+1)
                               ~step_size:s ~left:a ~right:b
                               ~fonction:f ~inlist:l));
      i_node
    end
    else if not(BatList.is_empty l) && (BatList.at l 0) = i_value then begin
      (BatDllist.splice i_node (create_dlist_helper_3D ~cuts:n ~step:(i+1)
                               ~step_size:s ~left:a ~right:b ~fonction:f
                               ~inlist:(BatList.tl l)));
      i_node
    end
    else begin
      let l_value = BatList.at l 0 in
      let (f1_l_value, f2_i_value) = f l_value in
      let l_node=BatDllist.create (ref(Some l_value),
                                  ref(Some f1_l_value),ref(Some f2_i_value)) in
      (BatDllist.splice l_node (create_dlist_helper_3D ~cuts:n ~step:(i)
                               ~step_size:s ~left:a ~right:b ~fonction:f
                               ~inlist:(BatList.tl l)));
      l_node
    end
  end

(*
creates a double linked list with the subdivisions of the interval
[a,b], inserts the element of the list l if l is not empty
*)
let create_dlist_3D ~cuts:n ~left:a ~right:b ~fonction:f ~inlist:l =
  if n<0 then failwith "the number of divisions must be positive"
  else begin
  let s = (abs_float (b-.a)) /. (float n) in
  create_dlist_helper_3D ~cuts:n ~step:0 ~step_size:s ~left:a ~right:b
                         ~fonction:f ~inlist:l
  end


(*
 * create an string containing one node of a lDlist
 *)
let to_string_dlist_3D_step ~node:n ~i_s:i_s ~l_i_b:l_i_b ~r_i_b:r_i_b =
  let (ref_some_x, ref_some_y, ref_some_z) = BatDllist.get n in
  let some_x = !ref_some_x in
  let some_y = !ref_some_y in
  let some_z = !ref_some_z in
  if (BatOption.is_none some_x) && (BatOption.is_none some_y) then
    let str = BatString.concat ""
              [l_i_b; "none" ;i_s; "none"; i_s; "none"; r_i_b] in
    str
  else if BatOption.is_none some_y then
    let arg = fos (BatOption.get some_x) in
    let str = BatString.concat ""
              [l_i_b; arg; i_s; "none"; i_s; "none"; r_i_b] in
    str
  else
    let arg = fos (BatOption.get some_x) in
    let img1 = fos (BatOption.get some_y) in
    let img2 = fos (BatOption.get some_z) in
    let str = BatString.concat ""
              [l_i_b; arg; i_s; img1; i_s; img2; r_i_b] in
    str

(*
 * helper function for the to_string function
 *)
let rec to_string_dlist_3D_helper ~lDlist:l ~r_length:r_len ~length:len 
         ~l_e_b:l_e_b ~r_e_b:r_e_b ~e_s:e_s ~i_s:i_s ~l_i_b:l_i_b 
         ~r_i_b:r_i_b =
  if len > (BatDllist.length l) then failwith "can't print more element than
                                               thoe in the list"
  else
    match (r_len, len) with
    |(0,0) -> "nothing to print"
    |(1,1) -> let str = BatString.concat ""
                        [l_e_b;(to_string_dlist_3D_step ~node:l ~i_s:i_s
                        ~l_i_b:l_i_b ~r_i_b:r_i_b);r_e_b] in
              str
    |(_,1) -> let str = BatString.concat ""
                        [(to_string_dlist_3D_step ~node:l ~i_s:i_s
                        ~l_i_b:l_i_b ~r_i_b:r_i_b);r_e_b] in
              str
    |(_,_) -> if r_len=len then begin
               let next = BatDllist.next l in
               BatString.concat "" [l_e_b;(to_string_dlist_3D_step ~node:l
               ~i_s:i_s ~l_i_b:l_i_b ~r_i_b:r_i_b);e_s;
               (to_string_dlist_3D_helper ~lDlist:next ~r_length:r_len
               ~length:(len -1) ~l_e_b:l_e_b ~r_e_b:r_e_b ~e_s:e_s ~i_s:i_s
               ~l_i_b:l_i_b ~r_i_b:r_i_b)]
              end
              else begin
                let next = BatDllist.next l in
                BatString.concat "" [(to_string_dlist_3D_step ~node:l ~i_s:i_s
                        ~l_i_b:l_i_b ~r_i_b:r_i_b);e_s;
                (to_string_dlist_3D_helper ~lDlist:next ~r_length:r_len
                ~length:(len -1) ~l_e_b:l_e_b ~r_e_b:r_e_b ~e_s:e_s ~i_s:i_s
                ~l_i_b:l_i_b ~r_i_b:r_i_b)]
              end

(*
 * create an string containtig the lDlist l
 *)
let to_string_3D ~lDlist:l ?l_e_b:(l_e_b="{") ?r_e_b:(r_e_b="}")
                 ?e_s:(e_s=",") ?i_s:(i_s=",") ?l_i_b:(l_i_b="(")
                 ?r_i_b:(r_i_b=")") () =
  to_string_dlist_3D_helper ~lDlist:l ~r_length:(BatDllist.length l)
  ~length:(BatDllist.length l) ~l_e_b:l_e_b ~r_e_b:r_e_b ~e_s:e_s
  ~i_s:i_s ~l_i_b:l_i_b  ~r_i_b:r_i_b
;;


let rec minimax_3D_helper ~lDlist:dl ~c_x_max:x_max ~c_x_min:x_min
                          ~c_y_max:y_max ~c_y_min:y_min ~c_z_min:z_min
                          ~c_z_max:z_max=
  let (ref_some_x, ref_some_y, ref_some_z) = BatDllist.get dl in
  if (BatOption.is_none !ref_some_x) || (BatOption.is_none !ref_some_y)
     || (BatOption.is_none !ref_some_z) then
    failwith "the linked list must be complete"
  else begin
  let (x,y,z) = (BatOption.get !ref_some_x, BatOption.get !ref_some_y,
                 BatOption.get !ref_some_z) in
  if (is_last_3D dl) then begin
    if x<x_min && y<y_min && z<z_min then (x, x_max, y, y_max, z, z_max)
    else if x<x_min && y<y_min && z>z_max then (x, x_max, y, y_max, z_min, z)
    else if x<x_min && y>y_max && z<z_min then (x, x_max, y_min, y, z, z_max)
    else if x<x_min && y>y_max && z>z_max then (x, x_max, y_min, y, z_min, z)
    else if x>x_max && y<y_min && z<z_min then (x_min, x, y, y_max, z, z_max)
    else if x>x_max && y<y_min && z>z_max then (x_min, x, y, y_max, z_min, z)
    else if x>x_max && y>y_max && z<z_min then (x_min, x, y_min, y, z, z_max)
    else if x>x_max && y>y_max && z>z_max then (x_min, x, y_min, y, z_min, z)

    else if x<x_min && y<y_min then (x, x_max, y, y_max, z_min, z_max)
    else if x<x_min && y>y_max then (x, x_max, y_min, y, z_min, z_max)
    else if x>x_max && y<y_min then (x_min, x, y, y_max, z_min, z_max)
    else if x>x_max && y>y_max then (x_min, x, y_min, y, z_min, z_max)

    else if x<x_min && z<z_min then (x, x_max, y_min, y_max, z, z_max)
    else if x<x_min && z>z_max then (x, x_max, y_min, y_max, z_min, z)
    else if x>x_max && z<z_min then (x_min, x, y_min, y_max, z, z_max)
    else if x>x_max && z>z_max then (x_min, x, y_min, y_max, z_min, z)

    else if y<y_min && z<z_min then (x_min, x_max, y, y_max, z, z_max)
    else if y<y_min && z>z_max then (x_min, x_max, y, y_max, z_min, z)
    else if y>y_max && z<z_min then (x_min, x_max, y_min, y, z, z_max)
    else if y>y_max && z>z_max then (x_min, x_max, y_min, y, z_min, z)

    else if x<x_min then (x, x_max, y_min, y_max, z_min, z_max)
    else if y<y_min  then (x_min, x_max, y, y_max, z_min, z_max)
    else if z<z_min then (x_min, x_max, y_min, y_max, z, z_max)

    else if x>x_max then (x_min, x, y_min, y_max, z_min, z_max)
    else if y>y_max then (x_min, x_max, y_min, y, z_min, z_max)
    else if z>z_max then (x_min, x_max, y_min, y_max, z_min, z)

    else (x_min, x_max, y_min, y_max, z_min, z_max)
  end
  else begin
    let next = BatDllist.next dl in
    if x<x_min && y<y_min && z<z_min then
      minimax_3D_helper ~lDlist:next ~c_x_max:x_max ~c_x_min:x ~c_y_max:y_max
                        ~c_y_min:y ~c_z_max:z_max ~c_z_min:z
    else if x<x_min && y<y_min && z>z_max then
      minimax_3D_helper ~lDlist:next ~c_x_max:x_max ~c_x_min:x ~c_y_max:y_max
                   ~c_y_min:y ~c_z_max:z ~c_z_min:z_min
    else if x<x_min && y>y_max && z<z_min then
      minimax_3D_helper ~lDlist:next ~c_x_max:x_max ~c_x_min:x ~c_y_max:y
      ~c_y_min:y_min ~c_z_max:z_max ~c_z_min:z
    else if x<x_min && y>y_max && z>z_max then
      minimax_3D_helper ~lDlist:next ~c_x_max:x_max ~c_x_min:x ~c_y_max:y
      ~c_y_min:y_min ~c_z_max:z ~c_z_min:z
    else if x>x_max && y<y_min && z<z_min then
      minimax_3D_helper ~lDlist:next ~c_x_max:x ~c_x_min:x_min ~c_y_max:y_max
      ~c_y_min:y ~c_z_max:z_max ~c_z_min:z
    else if x>x_max && y<y_min && z>z_max then
      minimax_3D_helper ~lDlist:next ~c_x_max:x ~c_x_min:x_min ~c_y_max:y_max
      ~c_y_min:y ~c_z_max:z ~c_z_min:z_min
    else if x>x_max && y>y_max && z<z_min then
      minimax_3D_helper ~lDlist:next ~c_x_max:x ~c_x_min:x_min ~c_y_max:y
      ~c_y_min:y_min ~c_z_max:z_max ~c_z_min:z
    else if x>x_max && y>y_max && z>z_max then
          minimax_3D_helper ~lDlist:next ~c_x_max:x ~c_x_min:x_min ~c_y_max:y
                        ~c_y_min:y_min ~c_z_max:z ~c_z_min:z_min

    else if x>x_max && y>y_max then
          minimax_3D_helper ~lDlist:next ~c_x_max:x ~c_x_min:x_min ~c_y_max:y
                        ~c_y_min:y_min ~c_z_max:z_max ~c_z_min:z_min
    else if x>x_max && y<y_min then
          minimax_3D_helper ~lDlist:next ~c_x_max:x ~c_x_min:x_min ~c_y_max:y_max
                        ~c_y_min:y_min ~c_z_max:z_max ~c_z_min:z_min
    else if x<x_min && y>y_max then
          minimax_3D_helper ~lDlist:next ~c_x_max:x_max ~c_x_min:x ~c_y_max:y
                        ~c_y_min:y_min ~c_z_max:z_max ~c_z_min:z_min
    else if x<x_min && y<y_min then
          minimax_3D_helper ~lDlist:next ~c_x_max:x_max ~c_x_min:x ~c_y_max:y_max
                        ~c_y_min:y~c_z_max:z_max ~c_z_min:z_min
                        
    else if x>x_max && z>z_max then
          minimax_3D_helper ~lDlist:next ~c_x_max:x ~c_x_min:x_min ~c_y_max:y_max
                        ~c_y_min:y_min ~c_z_max:z ~c_z_min:z_min
    else if x>x_max && z<z_min then
          minimax_3D_helper ~lDlist:next ~c_x_max:x ~c_x_min:x_min ~c_y_max:y_max
                        ~c_y_min:y_min ~c_z_max:z_max ~c_z_min:z
    else if x<x_min && z>z_max then
          minimax_3D_helper ~lDlist:next ~c_x_max:x_max ~c_x_min:x ~c_y_max:y_max
                        ~c_y_min:y_min ~c_z_max:z ~c_z_min:z_min
    else if x<x_min && z<z_min then
          minimax_3D_helper ~lDlist:next ~c_x_max:x_max ~c_x_min:x ~c_y_max:y_max
                        ~c_y_min:y_min ~c_z_max:z_max ~c_z_min:z
    

    else if y>y_max && z>z_max then
          minimax_3D_helper ~lDlist:next ~c_x_max:x_max ~c_x_min:x_min ~c_y_max:y
                        ~c_y_min:y_min ~c_z_max:z ~c_z_min:z_min
    else if y>y_max && z<z_min then
          minimax_3D_helper ~lDlist:next ~c_x_max:x_max ~c_x_min:x_min ~c_y_max:y
                        ~c_y_min:y_min ~c_z_max:z_max ~c_z_min:z
    else if y<y_min && z>z_max then
          minimax_3D_helper ~lDlist:next ~c_x_max:x_max ~c_x_min:x_min ~c_y_max:y_max
                        ~c_y_min:y ~c_z_max:z ~c_z_min:z_min
    else if y<y_min && z<z_min then
          minimax_3D_helper ~lDlist:next ~c_x_max:x_max ~c_x_min:x_min ~c_y_max:y_max
                        ~c_y_min:y ~c_z_max:z_max ~c_z_min:z
                        

    else if x>x_max then
          minimax_3D_helper ~lDlist:next ~c_x_max:x ~c_x_min:x_min ~c_y_max:y_max
                        ~c_y_min:y_min ~c_z_max:z_max ~c_z_min:z_min
    else if y>y_max then
          minimax_3D_helper ~lDlist:next ~c_x_max:x_max ~c_x_min:x_min ~c_y_max:y
                        ~c_y_min:y_min ~c_z_max:z_max ~c_z_min:z_min
    else if  z>z_max then
          minimax_3D_helper ~lDlist:next ~c_x_max:x_max ~c_x_min:x_min ~c_y_max:y_max
                        ~c_y_min:y_min ~c_z_max:z ~c_z_min:z_min
                        
    else if x<x_min then
          minimax_3D_helper ~lDlist:next ~c_x_max:x_max ~c_x_min:x ~c_y_max:y_max
                        ~c_y_min:y_min ~c_z_max:z_max ~c_z_min:z_min
    else if y<y_min then
          minimax_3D_helper ~lDlist:next ~c_x_max:x_max ~c_x_min:x_min ~c_y_max:y_max
                        ~c_y_min:y ~c_z_max:z_max ~c_z_min:z_min
    else if z<z_min then
          minimax_3D_helper ~lDlist:next ~c_x_max:x_max ~c_x_min:x_min ~c_y_max:y_max
                        ~c_y_min:y_min ~c_z_max:z_max ~c_z_min:z
    else
      minimax_3D_helper ~lDlist:next ~c_x_max:x_max ~c_x_min:x_min
                        ~c_y_max:y_max ~c_y_min:y_min ~c_z_max:z_max
                        ~c_z_min:z_min
  end
  end

let minimax_3D ~lDlist:dl =
  let (ref_some_x, ref_some_y, ref_some_z) = BatDllist.get dl in
  let (x,y,z) = (BatOption.get !ref_some_x, BatOption.get !ref_some_y,
                 BatOption.get !ref_some_z) in
  minimax_3D_helper ~lDlist:dl ~c_x_max:x ~c_x_min:x ~c_y_max:y ~c_y_min:y
                    ~c_z_max:z ~c_z_min:z


(*helper function for the to_list2 function*)
let rec to_list_tuple_3D_helper ~lDlist:dl ~inlist:l =
  if (BatDllist.length dl) = 0 then failwith "empty linked list"
  else
    let (ref_some_x, ref_some_y, ref_some_z) = BatDllist.get dl in
    let x = BatOption.get (!ref_some_x) in
    let y = BatOption.get (!ref_some_y) in
    let z = BatOption.get (!ref_some_z) in
    if (is_last_3D dl) then (BatList.append l [(x,y,z)])
    else to_list_tuple_3D_helper ~lDlist:(BatDllist.next dl)
                         ~inlist:(BatList.append l [(x,y,z)])

(*create a list of couples of floats from the linked list dl*)
(*this function is to be used for the implicit function sampling*)
let to_list_tuple_3D ~lDlist:dl =
  to_list_tuple_3D_helper ~lDlist:dl ~inlist:[]

(*helper function for the to_list function*)
let rec to_list_3D_helper ~lDlist:dl ~inlist:l =
  if (BatDllist.length dl) = 0 then failwith "empty linked list"
  else
    let (ref_some_x, ref_some_y, ref_some_z) = BatDllist.get dl in
    let x = BatOption.get (!ref_some_x) in
    if (is_last_3D dl) then (BatList.append l [x])
    else to_list_3D_helper ~lDlist:(BatDllist.next dl)
                           ~inlist:(BatList.append l [x])

(*same as to_list, returns only a list of floats*)
let to_list_3D ~lDlist:dl = to_list_3D_helper ~lDlist:dl ~inlist:[]


(*

(*
 * creates a linked list from a list of floats
 *)
let from_list_floats ~inlist:l =
  if (BatList.is_empty l) then failwith "nothing to transform"
  else
    let a = BatList.at l 0 in
    let dl = BatDllist.create ((ref (Some a), ref None)) in
    from_list_floats_helper ~lDlist:dl ~inlist:(BatList.tl l)

(*
 * helper function for the from_list_tuple function
 *)
let rec from_list_tuple_helper ~lDlist:dl ~inlist:l =
  if (BatList.is_empty l) then (BatDllist.next dl)
  else
    let (a, fa) = BatList.at l 0 in
    let dl_next = (ref (Some a), ref (Some fa)) in
    let dl_next_node = (if (BatDllist.length dl)=0
           then BatDllist.create dl_next
           else BatDllist.append dl dl_next) in
    from_list_tuple_helper ~lDlist:dl_next_node ~inlist:(BatList.tl l)

(*
 * creates a linked list from a list of tuples of floats
 *)
let from_list_tuple ~inlist:l =
  if (BatList.is_empty l) then failwith "nothing to transform"
  else
    let (a, fa) = BatList.at l 0 in
    let dl = BatDllist.create (ref (Some a), ref (Some fa)) in
    from_list_tuple_helper ~lDlist:dl ~inlist:(BatList.tl l)

*)
