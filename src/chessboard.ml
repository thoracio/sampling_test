open Printf

type chessboard = {up_left_corner : float*float; size: float;
                   rows : int; columns : int}
(*the chessboard can generate the elements of a matrix defining a zone in the
 *plane, we keep as data:
 *the up left corner of the matrix, the size, and the number of
 *elements by row and column
 *)

let make_chessboard (u_l_c : float*float) (s : float)
                    (r : int) (c : int) =
                      {up_left_corner = u_l_c; size = s; rows = r; columns = c}
(*create a chessboard from the input data*)

let init_chessboard xmin xmax ymin ymax nb e =
  let nb_block = int_of_float (sqrt (float nb)) in
  let nx = int_of_float ((xmax -. xmin)/.nb_block) in
  let ny = int_of_float ((ymax -. ymin)/.nb_block) in


let is_nan (x : float) = x<>x

let pi = acos (-.1.)

let is_valid cb =
  let {u_l_c; w; h; r; c} = cb in
  if r<=1 || c<=1 || w=0. || h=0. || (is_nan u_l_c) then false else true
(*test if the the chessboard cb is a valid chessboard*)

let up_left_corner cb =
  let {u_l_c; w; h; r; c} = cb in u_l_c

let rows cb =
  let {u_l_c; w; h; r; c} = cb in r

let columns cb =
  let {u_l_c; w; h; r; c} = cb in c

let width cb =
  let {u_l_c; w; h; r; c} = cb in w

let height cb =
  let {u_l_c; w; h; r; c} = cb in h

let w_step cb =
  if is_valid cb then
  (width cb) /. (float ((columns cb) - 1))
  else failwith "Chessboard: invalid chessboard."
(*the space between two adjacent elements in a column in the chessboard cb*)

let h_step cb =
  if is_valid cb then
  (heith cb) /. (float ((rows cb) - 1))
  else failwith "Chessboard: invalid chessboard."
(*the space between two adjacent elements in a row in the chessboard cb*)

let nb_blocks cb =
  if is_valid cb then
  ((rows cb) - 1) * ((columns cb) - 1)
  else failwith "Chessboard: invalid chessboard."
(*returns the number of blocks in the chessboard cb*)
(*a block is four elements generated by the chessboard such that:
 *their form a rectangle in the plane
 *any of the four elements is adjacent to other two
 *)

let nb_points cb =
  if is_valid cb then
  (rows cb) * (columns cb)
  else failwith "Chessboard: invalid chessboard."
(*returns the number of points in the chessboard cb*)

let to_coor cb nb =
  let r = rows cb in
  let c = columns cb in
  if nb mod c = 0 then
    (nb / c), c
  else
    ((nb / c) + 1), (nb mod c)
(*transform the given number in his equivalent in (i,j) coordinates for the
 *given chessboard cb
 *)

let to_nb cb (i,j) =
  let c = columns cb in
  (c * (i-1)) + j
(*transform the given matrix index (i,j) in his number equivalent for the
 *chessboard cb
 *)

let pick_point cb nb =
  if is_valid cb then begin
  if nb > (nb_points cb) then failwith "Chessboard: index out of bonds."
  else
    let x0,y0 = up_left_corner cb in
    let c = columns cb in
    let r = rows cb in
    let w_s = w_step cb in
    let h_s = h_step cb in
    if nb mod c = 0 then
      (x0 -. w_s*.(float (c-1))),
      (y0 -. h_s*.(float (nb/c - 1)))
    else
      (x0 -. w_s*.(float ((nb mod c) -1))),
      (y0 -. h_s*.(float (nb/c)))
  end
  else failwith "Chessboard: invalid chessboard."
(*pick a point from the chessboard by his number identifiant*)

let pick_point_coor cb (i,j) =
  if is_valid cb then
    pick_point cb (to_nb cb (i,j))
  else failwith "Chessboard: invalid chessboard."
(*pcik a point from the chessboard by his coordinate identifiant*)

let pick_block cb nb =
  if is_valid cb then begin
    let c = columns cb in
    let r = rows cb in
    if nb > (c-1)*(r-1) then failwith "Chessboard: index out of bonds."
    else begin
    if nb mod (c-1) = 0 then
      let n = nb + ((nb / (c-1))-1) in
      (pick_point n), (pick_point (n+1)), (pick_point (n+c)),
      (pick_point (n+c+1))
    else
      let n = nb + (nb / (c-1)) in
      (pick_point n), (pick_point (n+1)), (pick_point (n+c)),
      (pick_point (n+c+1))
    end
  end
  else failwith "Chessboard: invalid chessboard."
(*pick a whole block from the chessboard by his number identifiant*)

let print_elem cb (i,j) =
  if i>(rows cb) || j>(columns cb) then failwith "Chessboard: index out
                                                  of bonds"
  else
  let x,y = pick_point_coor cb (i,j) in
  printf "(%f, %f)" x y

let print_block cb nb =
  if nb > (nb_blocks cb) then failwith "Chessboard: index out of bonds"
  else
  let (a1,a2), (b1,b2), (c1,c2), (d1,d2) = bk in
  printf "(%f, %f) (%f, %f)\n(%f, %f) (%f, %f)" a1 a2 b1 b2 c1 c2 d1 d2

let print_chessboard_line cb l=
  let c = columns cb in
  for i=1 to (c-1) do
    print_elem cb (l,i);
    printf " ";
  done;
  print_elem cb (l,c)

let print_chessboard cb =
  let r = rows cb in
  for i=1 to r do
    print_chessboard_line cb i;
    printf "\n"
  done;

(*---------------------------------------------------------------------------*)
(*---------------------block treatment---------------------------------------*)


(*---------------------------------------------------------------------------*)
(*some neccesary fonctions*)
let dist_2D p0 p1 =
  let x0,y0 = p1 in
  let x1,y1 = p2 in
  sqrt ((x0 -. x1)**2. +. (y0 -. y1)**2.)
(*euclidean distance for the cartesian plane*)

let equals_2D p0 p1 =
  let x0,y0 = p0 && x1,y1 = p1 in
  x0=x1 && y0=y1
(*equality between points*)

let common_coor p0 p1 =
  let x0,y0 = p0 && x1,y1 = p1 in
  if x0=x1 && y0<> y1 then 1
  else if x0<>x1 && y0=y1 then 2
  else if x0=x1 && y0=y1 then 3
  else 0
(*test if the points p0 p1 have coordinates in common, returns 1 if only the
 *first coordinate, 2 if only the second one, 3 if both of them and 0 if
 *none
 *)

let in_line p0 p1 p =
  if equals_2D p0 p1 then false else begin
  let x0,y0 = p0 && x1,y1 = p1 && x,y = p in
  if x0=x1 && x<>x0 then false
  else if x0=x1 && x=x0 then true
  else if y0=y1 && y<>y0 then false
  else if y0=y1 && y=y0 then true
  else ((x-.x0)/.(x1-.x0)) = ((y-.y0)/.(y1-.y0))
  end
(*returns true if the point p lies in the line defined by the points p0, p1 *)

let midpoint p0 p1 =
  let x0,y0 = p0 && x1,y1 = p1 in
  ((x0+.x1)/.2.),((y0+.y1)/.2.)
(*returns midpoint of the points p0 p1*)

let rec false_position_helper f p0 p1 i n =
  if i = n then begin
    if (f p0) = 0. then true, p0, p0
    else if (f p1) = 0. then true, p1, p1
    else false, p0, p1
  end
  else begin
  if equals_2D p0 p1 then failwith "Module.Chessboard.false_position:
                                    cannot compute with equals points"
  else begin
    let x0,y0 = p0 && x1,y1 = p1 in
    let fp0 = f p0 && fp1 = f p1 in
    if fp0=0. then true, p0, p0
    else if fp1=0. then true, p1, p1
    else if fp0*.fp1 > 0 then failwith
            "Module.Chessboard.false_position: points images have the same
             sign, something went wrong"
    else begin
      let lambda = fp1 /. (fp1 -. fp0) in
      let p2 = (x1 -. lambda*.(x1-.x0), y1 -. lambda*.(y1-.y0)) in
      let fp2 = f p2 in
      if fp2=0. then true, p2, p2
      else begin
        if fp0*.fp2 > 0 then
          false_position_helper f p1 p2 (i+1) n
        else false_position_helper f p0 p2 (i+1) n
      end
    end
  end
  end

let false_position f p0 p1 n =
  false_position_helper f p0 p1 0 n
(*false_position for root finding algorithm, iterates n steps of the procedure
 *)

let slope_test fp0 fp1 length slope_tol =
  ((abs_float (fp0 +. fp1)) /. length) > slope_tol
(*with a seuil of tolerance of slope_tol, returns true if the slope between the
 *points is too high to the function f have a root in the segment between the
 *points p0 and p1
 *)

(*---------------------------------------------------------------------------*)

let test_block bk f slope_tol =
  let (a1,a2), (b1,b2), (c1,c2), (d1,d2) = bk in
  let a = f a1 a2 and b = f b1 b2 in
  let c = f c1 c2 and d = f d1 d2 in
  let len = abs_float (a1-.b1) in (*we need to assure un square in all times
                                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!*)
  if a=0. && b=0. && c=0. && d=0. then 0
  else if a=0. && b<>0. && c<>0. && d<>0. then 1
  else if a<>0. && b=0. && c<>0. && d<>0. then 2
  else if a<>0. && b<>0. && c=0. && d<>0. then 3
  else if a<>0. && b<>0. && c<>0. && d=0. then 4
  else if a=0. && b=0. && c<>0. && d<>0. then 5
  else if a=0. && b<>0. && c=0. && d<>0. then 6
  else if a=0. && b<>0. && c<>0. && d=0. then 7
  else if a<>0. && b=0. && c=0. && d<>0. then 8
  else if a<>0. && b=0. && c<>0. && d=0. then 9
  else if a<>0. && b<>0. && c=0. && d=0. then 10
  else if a=0. && b=0. && c=0. && d<>0. then 11
  else if a=0. && b<>0. && c=0. && d=0. then 12
  else if a=0. && b=0. && c<>0. && d=0. then 13
  else if a<>0. && b=0. && c=0. && d=0. then 14
  else if a*.b<0. || a*.c<0. || b*.d<0. then -1
  else if (slope_test a b len slope_tol) && (slope_test a c len slope_tol) &&
          (slope_test b c  len slope_tol) && (slope_test b d slope_tol) then 15
  else -1
(*0 : all four vertices are roots
 *1 : only the first is a root
 *2 : only the second is
 *3 : only the third
 *4 : only the fourth
 *5 : only the two firsts
 *6 : only the first and the third
 *7 : only the first and fourth
 *8 : only the second and the third
 *9 : only the second and the fourth
 *10: only the third and the fourth
 *11: all but the fourth are roots
 *12: all but the second one
 *13: all but the third
 *14: all but the first
 *15: the slope test order to not divide
 *-1: sign change, divide
 *)
(*this test is to initialize the block for further treatement*)

type block = {xmin : float; ymax : float; size : float; per_points : int;
              act_points : int; min_eps : float;
              tr_list : ((float*float)*(float*float)*(float*float)) list;
              side_values : (int*float) ref list;
              bottom_values : (int*float) ref list}
(*block type, implements an square of the plane, with
 *the upper left corner : (xmin,xmax)
 *the length of the side : size
 *points permitted in this square for sampling : per_points
 *point already calculated in this square : act_points
 *epsilon tolerance for the division algorithm : min_eps
 *a list of untreated triangles by the sampling algorithm : tr_list
 *a list of possibly already calculated values for the side of
  the square : side_values
 *a list of possibly already calculated values for the bottom of the
  square : bottom_values
 *)

let init_block bk pp ap e sv bv=
  let (a1,a2), (b1,b2), (c1,c2), (d1,d2) = bk in
  let s = abs_float (a1-.b1) in
  if s <> abs_float (a1-.c1) then failwith
    "Module.Chessboard.init_block: the block is not an square, cannot
     initialize the block"
  else
  let midp = (a1+.b1)/.2., (a2+.b2)/.2. in
  let tr_l = [((a1,a2), (b1,b2), midp); ((a1,a2), (c1,c2), midp);
              ((b1,b2), (d1,d2), midp); ((c1,c2), (d1,d2), midp)] in
  {xmin = a1; ymax = a2; size = s; per_points = pp; act_points = ap;
   tr_list = tr_l; side_values = sv; bottom_values = bv}
(*initialize the block for treatment, take as arguments
 *bk : four points of the cartesian plane
 *pp : number of permitted points for sampling in this square
 *ap : number of points already calculated and noted for sampling in this
  square
 *sv : a list of the side values
 *bv : a list of the bottom values
 *)

let get_pointer bk =
  let {xmin=x; ymax=y; size=_; per_points=_; act_points=_; min_eps=_;
       tr_list=_; side_values=_; bottom_values=_} = bk in
  (x,y)
(*returns the upper left corner of the block*)

let get_size bk =
  let {xmin=x; ymax=y; size=s; per_points=_; act_points=_; min_eps=_;
       tr_list=_; side_values=_; bottom_values=_} = bk in
  s
(*returns the length of the block*)

let divide_triangle tr =
  let p0, p1, p2 = tr in
  let (x0,y0), (x1,y1), (x2,y2) = p0, p1, p2 in
  if equals_2D p0 p1 || equals_2D p0 p2 ||
     equals_2D p1 p2 then
       failwith "Module.Chessboard.divide_triangle: must pass three differents
                 points, cannot divide if equals points present"
  (*two sides equality*)
  if x0=x1 && y0=y2 then
    midpoint p1 p2
  else if x0=x1 && y1=y2 then
    midpoint p0 p2
  else if x1=x2 && y1=y0 then
    midpoint p0 p2
  else if x1=x2 && y2=y0 then
    midpoint p1 p2
  else if x0=x2 && y0=y1 then
    midpoint p1 p2
  else if x0=x2 && y1=y2 then
    midpoint p0 p1
  (*one side equality*)
  else if x0=x1 || y0=y1 then
    midpoint p0 p1
  else if x0=x2 || y0=y2 then
    midpoint p0 p2
  else if x1=x2 || y1=y2 then
    midpoint p1 p2
  else failwith "Module.Chessboard.divide_triangle: bad triangle, somewhere
                 division went wrong"
(*take a triangle as three points of the plane, returns the point which
 *divides the triangle according to the sampling algorithm
 *)

let decompose_triangle tr =
  let p0, p1, p2 = tr in
  let vertex = divide_triangle p0 p1 p2 in
  if (in_line p0 p1 vertex) then
    (p0, p2, vertex), (vertex, p2, p1)
  else if (in_line p0 p2 vertex) then
    (p0, p1, vertex), (vertex, p1, p2)
  else
    (p1, p0, vertex), (vertex, p0, p2)
(*take a triangle as argument, returns two new triangles as a result of
 *dividing the first
 *)

(*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 *need to passe the decreasing length as a parameter to avoid further
 *calculation
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*)
let test_triangle tr f slope_tol =
  let p0, p1, p2 = tr in
  let (x0,y0), (x1,y1), (x2,y2) = p0, p1, p2 in
  let fp0 = f p0 && fp1 = f p1 && fp2 = f p2 in
  if fp0=0. && fp1=0. && fp2=0. then 0
  else if fp0=0. && fp1<>0. && fp2<>0. then 1
  else if fp0<>0. && fp1=0. && fp2<>0. then 2
  else if fp0<>0. && fp1<>0. && fp2=0. then 3
  else if fp0=0. && fp1=0. && fp2<>0. then 4
  else if fp0=0. && fp1<>0. && fp2=0. then 5
  else if fp0<>0. && fp1=0. && fp2=0. then 6
  else if fp0*.fp1 < 0. || fp0*.fp2<0. || fp1*.fp2<0. then -1
  else if (slope_test fp0 fp1 (dist_2D p0 p1) slope_tol) &&
          (slope_test fp0 fp2 (dist_2D p0 p2) slope_tol) &&
          (slope_test fp1 fp2 (dist_2D p1 p2) slope_tol) then 7
  else -1


(*0 : all vertices are roots
 *1 : only the first one is a root
 *2 : only the second one
 *3 : only the third
 *4 : only the two firsts
 *5 : the first and the third
 *6 : the second and third
 *7 : the slope test order to not divide
 *-1: sign change, divide
 *)


let end_treatemnt_trinagle tr =
  let p0, p1, p2 = tr in
  let (x0,y0), (x1,y1), (x2,y2) = p0, p1, p2 in
