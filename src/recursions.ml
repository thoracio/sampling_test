open Geometry_tools
open Choice
open Flatness_criterion
open BatDllist
open Printf
open LDlist_manipulation

let rec recursion_2D ~fonction:f ~lDlist:dl ~epsi:e ~test:t ~dist:d =
  if not(LDlist_manipulation.is_last_2D dl) then begin
   LDlist_manipulation.update_2D f dl;
   LDlist_manipulation.update_2D f (BatDllist.next dl);
   let (ref_some_a, ref_some_fa) = BatDllist.get dl in
   let (ref_some_b, ref_some_fb) = BatDllist.get (BatDllist.next dl) in
   let a = BatOption.get (!ref_some_a) in
   let b = BatOption.get (!ref_some_b) in
   let fa = BatOption.get (!ref_some_fa) in
   let fb = BatOption.get (!ref_some_fb) in
   let m = Choice.perturbed_midpoint_choice a b in
   let fm = f m in
   let p_f_a = Geometry_tools.create_point_2D ~x:a ~y:fa in
   let p_f_b = Geometry_tools.create_point_2D ~x:b ~y:fb in
   let p_f_m = Geometry_tools.create_point_2D ~x:m ~y:fm in
   if not (t p_f_a p_f_b p_f_m e d) then begin
    let m_node = BatDllist.create (ref (Some m), ref (Some fm)) in
     let inter_node = BatDllist.next dl in
     BatDllist.splice dl m_node;
     BatDllist.splice m_node inter_node;
     recursion_2D ~fonction:f ~lDlist:dl ~epsi:e ~test:t ~dist:d;
   end
   else begin
     let inter_node = BatDllist.next dl in
     recursion_2D ~fonction:f ~lDlist:inter_node ~epsi:e ~test:t ~dist:d;
    end
   end

let rec recursion_3D ~fonction:f ~lDlist:dl ~epsi:e ~test:t ~dist:d =
   LDlist_manipulation.update_3D f dl;
   LDlist_manipulation.update_3D f (BatDllist.next dl);
   let (ref_some_a, ref_some_f1a, ref_some_f2a) = BatDllist.get dl in
   let (ref_some_b, ref_some_f1b, ref_some_f2b) = BatDllist.get
                                                  (BatDllist.next dl) in
   let a = BatOption.get (!ref_some_a) in
   let b = BatOption.get (!ref_some_b) in
   let f1a = BatOption.get (!ref_some_f1a) in
   let f2a = BatOption.get (!ref_some_f2a) in
   let f1b = BatOption.get (!ref_some_f1b) in
   let f2b = BatOption.get (!ref_some_f2b) in
   let m = Choice.perturbed_midpoint_choice (BatOption.get !ref_some_a)
                                            (BatOption.get !ref_some_b) in
   let (f1m,f2m) = f m in
   let p_f_a = Geometry_tools.create_point_3D ~x:a ~y:f1a ~z:f2a in
   let p_f_b = Geometry_tools.create_point_3D ~x:b ~y:f1b ~z:f2b in
   let p_f_m = Geometry_tools.create_point_3D ~x:m ~y:f1m ~z:f2m in
   if not (t p_f_a p_f_b p_f_m e d) &&
      not(LDlist_manipulation.is_last_3D ~node:(BatDllist.next dl))then begin
     let m_node = BatDllist.create
                  (ref (Some m), ref (Some f1m), ref (Some f2m)) in
     let inter_node = BatDllist.next dl in
     BatDllist.splice dl m_node;
     BatDllist.splice m_node inter_node;
     recursion_3D ~fonction:f ~lDlist:dl ~epsi:e ~test:t ~dist:d;
   end
   else if not (t p_f_a p_f_b p_f_m e d) &&
           (LDlist_manipulation.is_last_3D ~node:(BatDllist.next dl))then begin
     let m_node = BatDllist.create
                  (ref (Some m), ref (Some f1m), ref (Some f2m)) in
     let inter_node = BatDllist.next dl in
     BatDllist.splice dl m_node;
     BatDllist.splice m_node inter_node;
     recursion_3D ~fonction:f ~lDlist:dl ~epsi:e ~test:t ~dist:d;
   end
   else if (t p_f_a p_f_b p_f_m e d) &&
           not(LDlist_manipulation.is_last_3D
           ~node:(BatDllist.next dl))then begin
     let inter_node = BatDllist.next dl in
     recursion_3D ~fonction:f ~lDlist:inter_node ~epsi:e ~test:t ~dist:d;
    end


