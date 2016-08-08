open Printf

type 'a avltree =
  |Leaf
  |Node of 'a avltree * 'a * 'a avltree * int
(* (left_subtree * data * right_subtree * height *)

type 'a avlt_ref = 'a avltree ref

type compare = 'a -> 'a -> int
(*[compare c1 c2] returns 1 if c1 is greater than c2 for the
 *compare function 0 if the same and -1 if c2 is the
 *the greater one
 *)

type rotation = 'a avlt_ref -> unit

let make_empty_avltree () = ref Leaf

let singleton data = ref Node(Leaf, data, Leaf, 1)

let is_empty avlt = !avlt = Leaf

(*balance factor := height(left subtree) - height(right subtree) *)

let height avlt =
  if is_empty !avlt then 0 else
  let (l_subtree, data, r_subtree, h) = !avlt in h

let left avlt =
  if is_empty !avlt then failwith "AvlTree: empty tree" else
  let (l_subtree, data, r_subtree, h) = !avlt in l_subtree

let right avlt =
  if is_empty !avlt then failwith "AvlTree: empty tree" else
  let (l_subtree, data, r_subtree, h) = !avlt in r_subtree

let balance_factor avlt = height (left !alvt) - height (right !alvt)

let left_rotation avlt =
  if not(abs (balance_factor !avlt) <= 1) then
  match !avlt with
  |(l_subtree, data, Leaf, h) -> failwith "AvlTree: can't perform left
                                           rotation with an empty right
                                           subtree"
  |(Leaf, data, r_subtree, h) ->
    let (r_l_subtree, r_data, r_r_subtree, r_h) = r_subtree in
    let h_r_l_subtree = height r_l_subtree in
    let h_r_r_subtree = height r_r_subtree in
    let new_h = 1 + (max (1+h_r_l_subtree) h_r_r_subtree) in
    avlt := Node( Node(Leaf, data, r_l_subtree, 1+h_r_l_subtree),
              r_data, r_r_subtree, new_h)
  |(l_subtree, data, r_subtree, h) ->
    let (r_l_subtree, r_data, r_r_subtree, r_h) = r_subtree in
    let (l_l_subtree, l_data, l_r_subtree, l_h) = l_subtree in
    let h_r_l_subtree = height r_l_subtree in
    let new_h_l_subtree = 1 + (max l_h h_r_l_subtree) in
    avlt := Node(Node(l_subtree, data, r_l_subtree, new_h_l_subtree),
                 r_data, r_r_subtree, 1+(max new_h_l_subtree (height
                 r_r_subtree)))
  |_ -> failwith "AvlTree: not an avltree"

let right_rotation avlt =
  if not(abs (balance_factor !avlt) <= 1) then
  match !avlt with
  |(Leaf, data, r_subtree, h) -> failwith "AvlTree: can't perform right
                                           rotation with an empty left
                                           subtree"
  |(l_subtree, data, Leaf, h) ->
    let (l_l_subtree, l_data, l_r_subtree, l_h) = l_subtree in
    let new_h_l_subtree = 1 + (height l_r_subtree) in
    avlt := Node(l_l_subtree, l_data,
                 Node(l_l_subtree, data, Leaf, new_h_l_subtree),
                 1+(max (height l_l_subtree) new_h_l_subtree))
  |(l_subtree, data, r_subtree, h) ->
    let (r_l_subtree, r_data, r_r_subtree, r_h) = r_subtree in
    let (l_l_subtree, l_data, l_r_subtree, l_h) = l_subtree in
    let new_h_l_subtree = 1 + (max (height l_r_subtree) r_h) in
    let new_h = 1 + (max new_h_l_subtree (height l_l_subtree)) in 
    avlt := Node(l_l_subtree, l_data,
                 Node(l_r_subtree, data, r_subtree, new_h_l_subtree),
                 new_h)
  |_ -> failwith "AvlTree: not an avltree"

let left_right_rotation avlt =
  if (is_empty !avlt) then failwith "AvlTree: can't perform left-right
                                     rotation with an empty tree"
  else if(is_empty (left !avlt)) || (is_empty (right (left !avlt))) then
    failwith "AvlTree: can't perform left-right rotation with an emtpy
              subtree"
  else if not(abs (balance_factor !avlt) <= 1) then
  match !avlt with
  |(l_subtree, data, Leaf, h) ->
    match l_subtree with
    |(Leaf, l_data, l_r_subtree, _) ->
        let (l_l_r_subtree, l_r_data, r_l_r_subtree, l_r_h) = l_r_subtree in
        let l_l_r_h = height l_l_r_subtree in
        avlt := Node(Node(Leaf, l_data, l_l_r_subtree, 1+ l_l_r_h),
                     l_r_data,
                     Node(r_l_r_subtree, data, Leaf), h - 1)
    |(l_l_subtree, l_data, r_l_subtree, l_h) ->
       let (l_l_r_subtree, l_r_data, r_l_r_subtree, l_r_h) = l_r_subtree in
       let (l_l_l_subtree, l_l_data, r_l_l_subtree, l_l_h) = l_l_subtree in
       let l_l_r_h = height l_l_r_subtree in
       let r_l_r_h = height r_l_r_subtree in
       avlt := Node(Node(l_l_subtree, l_data, l_l_r_subtree, 1+ l_l_r_h),
                    l_r_data,
                    Node(r_l_r_subtree, data, Leaf), h - 1)


