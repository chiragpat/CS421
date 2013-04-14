open List
open Mp11common

exception NotImplemented

let origin = (0.0,0.0)

let ( ++ ) ((x0,y0):point) ((x1,y1):point) : point = (x0+.x1, y0+.y1)
let ( -- ) ((x0,y0):point) ((x1,y1):point) : point = (x0-.x1, y0-.y1)
let ( *** ) ((x0,y0):point) ((x1,y1):point) : point = (x0*.x1, y0*.y1)

type transformation = point -> point

type picture = transformation -> draw_cmd list

(* Transformations *)

let id_trans = fun pt -> pt

type trans_mat = (float * float * float) * (float * float * float)

(* Problem 1: Apply trans2, then trans1. *)
let compose (trans1:transformation) (trans2:transformation) : transformation
   = fun (p:point) -> trans1 (trans2 p)   (* 1 line *)

let matrix_transform (((m1,m2,m3),(n1,n2,n3)):trans_mat) : transformation =
   fun (x,y) -> let newX = (m1*.x) +. (m2*.y) +. m3
                and newY = (n1*.x) +. (n2*.y) +. n3
                in (newX, newY)

let scaleWithPoint (pic:picture) (x1,y1) (fixedX, fixedY) =
    let newMatrix = ((x1, 0.0, -.fixedX*.x1+.fixedX),
                     (0.0, y1, -.fixedY*.y1+.fixedY))
    in fun phi -> pic (compose phi (matrix_transform newMatrix))

let rotateAroundPoint (pic:picture) (degrees:float) ((centerX, centerY):point) =
    let x = (degrees /. 360.0) *. 6.28318
    in let cosX = cos x
       and sinX = sin x
       in let first = (-.(centerX*.cosX))+.(centerY*.sinX)+.centerX
          and second = (-.(centerX*.sinX))-.(centerY*.cosX)+.centerY
          in let newMatrix = ((cosX, -.sinX, first), (sinX, cosX, second))
             in fun phi -> pic (compose (matrix_transform newMatrix) phi)

(* Problem 2: add offset to all points (before other transformations) *)
let translate (pic:picture) (offset:point) : picture
    = fun (trans:transformation) -> pic (compose trans (fun (p1:point) -> p1 ++ offset)) (* 1 line *)

(* Basic pictures *)
let emptypic = fun phi -> []

let line (pt1:point) (pt2:point) : picture
     = fun phi -> [Line(phi pt1, phi pt2)]

let oval (sw:point) (se:point) (ne:point) : picture
     = fun phi -> let sw' = phi sw
                  and se' = phi se
                  and ne' = phi ne
                  in [Oval(sw',se',ne')]

(* Picture-combining operations *)

(* Problem 3:  Combined picture will produce combined set of
               drawing commands, after transformation. *)
let ( && ) (pic1:picture) (pic2:picture) : picture
   = fun (trans:transformation) -> (pic1 trans)@(pic2 trans) (* 1 line *)

(* Problem 4: Combine a list of pictures, using &&.  Do *not* use
              recursion directly; use fold_left in List module *)
let join_list (piclis: picture list) 
   = fold_left (fun (pic1:picture) (pic2:picture) -> pic1 && pic2) emptypic piclis  (* 1 line *)

(* Latex output *)

let sof = string_of_float

let round (x:float) = float_of_int (truncate(x *. 1000.0)) /. 1000.0;;

let draw (pic: picture) : unit =
    let clis = pic id_trans
    in let strlist =
          map (fun cmd -> match cmd with
             Line ((x0,y0), (x1,y1)) ->
                    let xdelta = round (x1-.x0)
                    and ydelta = round (y1-.y0)
                    in let length = if xdelta=0.0 then ydelta else xdelta
                       in "\\put(" ^ (sof (round x0)) ^ "," ^ (sof (round y0))
                          ^ "){" ^ "\\line(" ^ sof xdelta ^ "," ^ sof ydelta
                          ^ "){" ^ (sof (abs_float length)) ^ "}}\n")
           clis
    in print_string (fold_right ( ^ ) strlist "")

(* User-defined pictures *)

let circle ((x,y):point) (r:float) : picture
   = let sw = (x-.r,y-.r)
     and se = (x+.r,y-.r)
     and ne = (x+.r,y+.r)
     in oval sw se ne

let box ((x0,y0):point) ((x1,y1):point) ((x2,y2):point)
   = join_list [line (x0,y0) (x1,y1); line (x1,y1) (x2,y2);
                line (x2,y2) (x0,y2); line (x0,y2) (x0,y0)]

let triangle (pt1:point) (pt2:point) (pt3:point) : picture =
   join_list [line pt1 pt2; line pt2 pt3; line pt3 pt1] 

let unit_triangle = triangle origin (1.0,0.0) (0.5,0.866)

let rec sierpinski n : picture =
    if n=0
    then unit_triangle
    else let s = scaleWithPoint (sierpinski (n-1)) (0.5,0.5) origin
         in join_list [s; translate s (0.5,0.0); translate s (0.25,0.5)]

type linefun = point -> point -> picture

(* Problem 5: shorten takes a line function and modifies it as follows:
              given two points p1 and p2, it uses the original line
              function, but doesn't draw the line all the way; instead,
              it draws a line from pt1 + delta to pt1 - delta, where
              delta is .25 * distance from pt1 to pt2. 
              E.g. shorten lf (0.0,0.0) (4.0,4.0) uses lf to draw a
              line from (1.0,1.0) to (3.0,3.0). *)
              
let shorten (lf:linefun) : linefun
  = fun (p1:point) (p2:point) ->
      let (x,y) = p2 -- p1
        in let (x',y') = (0.25*.x, 0.25*.y)
          in lf (p1 ++ (x', y')) (p2 -- (x', y'))  (* 5 lines *)

type tree = picture * float * float (* picture with height and width *)

let leaf (pic:picture) (wd:float) (ht:float) = (pic, wd, ht)

let node (pic:picture) (w0:float) (h0:float)
         ((p1,w1,h1):tree) ((p2,w2,h2):tree)
         (lf:linefun)
   = let vsep = h0 /. 4.0
     and hsep = (w1 +. w2) /. 4.0
     and child_h = max h1 h2
     in let w = max (w1 +. hsep +. w2) w0
        and h = h0 +. child_h +. vsep
        in let mid_w = w /. 2.0
           in let rootloc = (mid_w -. w0 /. 2.0, child_h +. vsep)
              and child1loc = (0.0, child_h -. h1)
              and child2loc = (w -. w2, child_h -. h2)
              and root_bottom = (mid_w, h -. h0)
              and child1top = (w1 /. 2.0, child_h)
              and child2top = (w -. w2 /. 2.0, child_h)
              in (translate pic rootloc
                  && translate p1 child1loc
                  && translate p2 child2loc
                  && lf root_bottom child1top
                  && lf root_bottom child2top, w, h)

let manline ((x0,y0) as pt0:point) ((x1,y1) as pt1:point) : picture
   = let ymid = (y0 +. y1) /. 2.0
     in let corner1 = (x0, ymid)
        and corner2 = (x1, ymid)
        in line pt0 corner1 && line corner1 corner2 && line corner2 pt1


(* Problem 6: Rewrite nodelist, using no explicitly recursive functions
              (i.e. no "let rec"), but instead only using higher-order
              functions like map, fold_right, fold_left, map2 - functions
              that are found in the List module.
              Our solution is about 30 lines of code, compared to about
              45 for this function. *)
let nodelist (pic:picture) (w0:float) (h0:float)
            (children:tree list) (lf:linefun)
 = let pics tlis = map (fun (p,_,_) -> p) tlis
   and widths tlis = map (fun (_,w,_) -> w) tlis
   and heights tlis = map (fun (_,_,h) -> h) tlis
   and sum lis = fold_right (fun a sum -> a +. sum) lis 0.0
   and maxelt lis = fold_right (fun a b -> if a > b then a else b) lis 0.0
   and mkpairs lis1 lis2 = map2 (fun a b -> (a,b)) lis1 lis2
   and draw_pics pics locs = map2 (fun p l -> translate p l) pics locs
   and draw_lines p pts = map (fun pt -> line p pt) pts
   in let vsep = h0 /. 4.0
      and hsep = sum (widths children)
                         /. (float_of_int (length children)) /. 2.0
      and child_h = maxelt (heights children)
      and pics = pics children
      and widths = widths children
      and heights = heights children
      and n = length children
      in let w = sum widths +. hsep *. ((float_of_int )n -. 1.0)
         and h = child_h +. vsep +. h0
         in let xlocs wlis xloc =
                    if (length wlis) = 0 then []
                    else fold_left (fun locLis w -> 
                                      if (length locLis)=(length wlis) then locLis 
                                      else locLis@[(nth locLis ((length locLis)-1)) +. w +. hsep]) 
                              [xloc] wlis 
                and ylocs hlis = map (fun h -> child_h -. h) hlis
                in let toplocs wlis xloc =
                    if (length wlis) = 0 then []
                    else let xloclis = xlocs wlis xloc
                            in map2 (fun xl w -> (xl +. w /. 2.0, child_h)) xloclis wlis
                in let child_locs = mkpairs (xlocs widths 0.0) (ylocs heights)
                   and toplocs = toplocs widths 0.0
                   and rootloc = (w/.2.0 -. w0/.2.0, child_h +. vsep)
                   and rootbottom = (w/.2.0, child_h +. vsep)
                   in (join_list (draw_pics (pic::pics)
                                            (rootloc :: child_locs)
                                 @ draw_lines rootbottom toplocs),
                       w, h)


(* Correct output for these examples is given below.
   (Most are shown in the mp write-up.)              *)
(*
(* Examples *)
let box1 = box (0.0,0.0) (1.0,0.0) (1.0,1.0);;
let leaf1 = (box1, 1.0, 1.0);;
let t1 = node box1 1.0 1.0 leaf1 leaf1 line;;
let t2 = node box1 1.0 1.0 t1 leaf1 line;; 
let (p,_,_) = t2 in draw p;;

(* This example has the same output as the previous one
   example above using "node" *)
let t3 = nodelist box1 1.0 1.0 [leaf1; leaf1] line;;
let t4 = nodelist box1 1.0 1.0 [t3; leaf1] line;; 
let (p,_,_) = t4 in draw p;;

let t5 = node box1 1.0 1.0 leaf1 leaf1 manline;;
let t6 = node box1 1.0 1.0 t5 leaf1 manline;; 
let (p,_,_) = t6 in draw p;;

let t7 = node box1 1.0 1.0 leaf1 leaf1 (shorten manline);;
let t8 = node box1 1.0 1.0 t7 leaf1 (shorten manline);; 
let (p,_,_) = t8 in draw p;;

let t9 = node box1 1.0 1.0 leaf1 leaf1 (shorten line);;
let t10 = node box1 1.0 1.0 t9 leaf1 (shorten line);; 
let (p,_,_) = t10 in draw p;;

let treepic (pic,_,_) = pic
let t11 = rotateAroundPoint (treepic t2) 180.0 (1.0,1.0);;
draw t11;;

draw (sierpinski 3);;
draw (sierpinski 4);;
*)
(*
(* let (p,_,_) = t2 in draw p;; *)
\put(1.687,2.5){\line(1.,0.){1.}}
\put(2.687,2.5){\line(0.,1.){1.}}
\put(2.687,3.5){\line(-1.,0.){1.}}
\put(1.687,3.5){\line(0.,-1.){1.}}
\put(0.75,1.25){\line(1.,0.){1.}}
\put(1.75,1.25){\line(0.,1.){1.}}
\put(1.75,2.25){\line(-1.,0.){1.}}
\put(0.75,2.25){\line(0.,-1.){1.}}
\put(0.,0.){\line(1.,0.){1.}}
\put(1.,0.){\line(0.,1.){1.}}
\put(1.,1.){\line(-1.,0.){1.}}
\put(0.,1.){\line(0.,-1.){1.}}
\put(1.5,0.){\line(1.,0.){1.}}
\put(2.5,0.){\line(0.,1.){1.}}
\put(2.5,1.){\line(-1.,0.){1.}}
\put(1.5,1.){\line(0.,-1.){1.}}
\put(1.25,1.25){\line(-0.75,-0.25){0.75}}
\put(1.25,1.25){\line(0.75,-0.25){0.75}}
\put(3.375,1.25){\line(1.,0.){1.}}
\put(4.375,1.25){\line(0.,1.){1.}}
\put(4.375,2.25){\line(-1.,0.){1.}}
\put(3.375,2.25){\line(0.,-1.){1.}}
\put(2.187,2.5){\line(-0.937,-0.25){0.937}}
\put(2.187,2.5){\line(1.687,-0.25){1.687}}

(* let (p,_,_) = t4 in draw p;; *)
\put(1.687,2.5){\line(1.,0.){1.}}
\put(2.687,2.5){\line(0.,1.){1.}}
\put(2.687,3.5){\line(-1.,0.){1.}}
\put(1.687,3.5){\line(0.,-1.){1.}}
\put(0.75,1.25){\line(1.,0.){1.}}
\put(1.75,1.25){\line(0.,1.){1.}}
\put(1.75,2.25){\line(-1.,0.){1.}}
\put(0.75,2.25){\line(0.,-1.){1.}}
\put(0.,0.){\line(1.,0.){1.}}
\put(1.,0.){\line(0.,1.){1.}}
\put(1.,1.){\line(-1.,0.){1.}}
\put(0.,1.){\line(0.,-1.){1.}}
\put(1.5,0.){\line(1.,0.){1.}}
\put(2.5,0.){\line(0.,1.){1.}}
\put(2.5,1.){\line(-1.,0.){1.}}
\put(1.5,1.){\line(0.,-1.){1.}}
\put(1.25,1.25){\line(-0.75,-0.25){0.75}}
\put(1.25,1.25){\line(0.75,-0.25){0.75}}
\put(3.375,1.25){\line(1.,0.){1.}}
\put(4.375,1.25){\line(0.,1.){1.}}
\put(4.375,2.25){\line(-1.,0.){1.}}
\put(3.375,2.25){\line(0.,-1.){1.}}
\put(2.187,2.5){\line(-0.937,-0.25){0.937}}
\put(2.187,2.5){\line(1.687,-0.25){1.687}}

(* let (p,_,_) = t6 in draw p;; *)
\put(1.687,2.5){\line(1.,0.){1.}}
\put(2.687,2.5){\line(0.,1.){1.}}
\put(2.687,3.5){\line(-1.,0.){1.}}
\put(1.687,3.5){\line(0.,-1.){1.}}
\put(0.75,1.25){\line(1.,0.){1.}}
\put(1.75,1.25){\line(0.,1.){1.}}
\put(1.75,2.25){\line(-1.,0.){1.}}
\put(0.75,2.25){\line(0.,-1.){1.}}
\put(0.,0.){\line(1.,0.){1.}}
\put(1.,0.){\line(0.,1.){1.}}
\put(1.,1.){\line(-1.,0.){1.}}
\put(0.,1.){\line(0.,-1.){1.}}
\put(1.5,0.){\line(1.,0.){1.}}
\put(2.5,0.){\line(0.,1.){1.}}
\put(2.5,1.){\line(-1.,0.){1.}}
\put(1.5,1.){\line(0.,-1.){1.}}
\put(1.25,1.25){\line(0.,-0.125){0.125}}
\put(1.25,1.125){\line(-0.75,0.){0.75}}
\put(0.5,1.125){\line(0.,-0.125){0.125}}
\put(1.25,1.25){\line(0.,-0.125){0.125}}
\put(1.25,1.125){\line(0.75,0.){0.75}}
\put(2.,1.125){\line(0.,-0.125){0.125}}
\put(3.375,1.25){\line(1.,0.){1.}}
\put(4.375,1.25){\line(0.,1.){1.}}
\put(4.375,2.25){\line(-1.,0.){1.}}
\put(3.375,2.25){\line(0.,-1.){1.}}
\put(2.187,2.5){\line(0.,-0.125){0.125}}
\put(2.187,2.375){\line(-0.937,0.){0.937}}
\put(1.25,2.375){\line(0.,-0.125){0.125}}
\put(2.187,2.5){\line(0.,-0.125){0.125}}
\put(2.187,2.375){\line(1.687,0.){1.687}}
\put(3.875,2.375){\line(0.,-0.125){0.125}}

(* let (p,_,_) = t8 in draw p;; *)
\put(1.687,2.5){\line(1.,0.){1.}}
\put(2.687,2.5){\line(0.,1.){1.}}
\put(2.687,3.5){\line(-1.,0.){1.}}
\put(1.687,3.5){\line(0.,-1.){1.}}
\put(0.75,1.25){\line(1.,0.){1.}}
\put(1.75,1.25){\line(0.,1.){1.}}
\put(1.75,2.25){\line(-1.,0.){1.}}
\put(0.75,2.25){\line(0.,-1.){1.}}
\put(0.,0.){\line(1.,0.){1.}}
\put(1.,0.){\line(0.,1.){1.}}
\put(1.,1.){\line(-1.,0.){1.}}
\put(0.,1.){\line(0.,-1.){1.}}
\put(1.5,0.){\line(1.,0.){1.}}
\put(2.5,0.){\line(0.,1.){1.}}
\put(2.5,1.){\line(-1.,0.){1.}}
\put(1.5,1.){\line(0.,-1.){1.}}
\put(1.062,1.187){\line(0.,-0.062){0.062}}
\put(1.062,1.125){\line(-0.375,0.){0.375}}
\put(0.687,1.125){\line(0.,-0.062){0.062}}
\put(1.437,1.187){\line(0.,-0.062){0.062}}
\put(1.437,1.125){\line(0.375,0.){0.375}}
\put(1.812,1.125){\line(0.,-0.062){0.062}}
\put(3.375,1.25){\line(1.,0.){1.}}
\put(4.375,1.25){\line(0.,1.){1.}}
\put(4.375,2.25){\line(-1.,0.){1.}}
\put(3.375,2.25){\line(0.,-1.){1.}}
\put(1.953,2.437){\line(0.,-0.062){0.062}}
\put(1.953,2.375){\line(-0.468,0.){0.468}}
\put(1.484,2.375){\line(0.,-0.062){0.062}}
\put(2.609,2.437){\line(0.,-0.062){0.062}}
\put(2.609,2.375){\line(0.843,0.){0.843}}
\put(3.453,2.375){\line(0.,-0.062){0.062}}

(* let (p,_,_) = t10 in draw p;; *)
\put(1.687,2.5){\line(1.,0.){1.}}
\put(2.687,2.5){\line(0.,1.){1.}}
\put(2.687,3.5){\line(-1.,0.){1.}}
\put(1.687,3.5){\line(0.,-1.){1.}}
\put(0.75,1.25){\line(1.,0.){1.}}
\put(1.75,1.25){\line(0.,1.){1.}}
\put(1.75,2.25){\line(-1.,0.){1.}}
\put(0.75,2.25){\line(0.,-1.){1.}}
\put(0.,0.){\line(1.,0.){1.}}
\put(1.,0.){\line(0.,1.){1.}}
\put(1.,1.){\line(-1.,0.){1.}}
\put(0.,1.){\line(0.,-1.){1.}}
\put(1.5,0.){\line(1.,0.){1.}}
\put(2.5,0.){\line(0.,1.){1.}}
\put(2.5,1.){\line(-1.,0.){1.}}
\put(1.5,1.){\line(0.,-1.){1.}}
\put(1.062,1.187){\line(-0.375,-0.125){0.375}}
\put(1.437,1.187){\line(0.375,-0.125){0.375}}
\put(3.375,1.25){\line(1.,0.){1.}}
\put(4.375,1.25){\line(0.,1.){1.}}
\put(4.375,2.25){\line(-1.,0.){1.}}
\put(3.375,2.25){\line(0.,-1.){1.}}
\put(1.953,2.437){\line(-0.468,-0.125){0.468}}
\put(2.609,2.437){\line(0.843,-0.125){0.843}}
- : unit = ()
draw t11;;
val treepic : 'a * 'b * 'c -> 'a = <fun>
val t11 : transformation -> draw_cmd list = <fun>
\put(0.312,-0.499){\line(-0.999,0.){0.999}}
\put(-0.687,-0.499){\line(0.,-0.999){0.999}}
\put(-0.687,-1.499){\line(0.999,0.){0.999}}
\put(0.312,-1.499){\line(0.,0.999){0.999}}
\put(1.249,0.749){\line(-0.999,0.){0.999}}
\put(0.249,0.75){\line(0.,-0.999){0.999}}
\put(0.249,-0.249){\line(0.999,0.){0.999}}
\put(1.249,-0.25){\line(0.,0.999){0.999}}
\put(2.,1.999){\line(-0.999,0.){0.999}}
\put(1.,1.999){\line(0.,-0.999){0.999}}
\put(0.999,1.){\line(0.999,0.){0.999}}
\put(1.999,0.999){\line(0.,0.999){0.999}}
\put(0.5,2.){\line(-0.999,0.){0.999}}
\put(-0.499,2.){\line(0.,-0.999){0.999}}
\put(-0.499,1.){\line(0.999,0.){0.999}}
\put(0.5,1.){\line(0.,0.999){0.999}}
\put(0.749,0.75){\line(0.75,0.249){0.75}}
\put(0.749,0.75){\line(-0.749,0.25){0.749}}
\put(-1.375,0.75){\line(-0.999,0.){0.999}}
\put(-2.375,0.75){\line(0.,-0.999){0.999}}
\put(-2.375,-0.249){\line(0.999,0.){0.999}}
\put(-1.375,-0.249){\line(0.,0.999){0.999}}
\put(-0.187,-0.499){\line(0.937,0.249){0.937}}
\put(-0.187,-0.499){\line(-1.687,0.25){1.687}}

(* draw (sierpinski 3);; *)
\put(0.,0.){\line(0.125,0.){0.125}}
\put(0.125,0.){\line(-0.062,0.108){0.062}}
\put(0.062,0.108){\line(-0.062,-0.108){0.062}}
\put(0.125,0.){\line(0.125,0.){0.125}}
\put(0.25,0.){\line(-0.062,0.108){0.062}}
\put(0.187,0.108){\line(-0.062,-0.108){0.062}}
\put(0.062,0.125){\line(0.125,0.){0.125}}
\put(0.187,0.125){\line(-0.062,0.108){0.062}}
\put(0.125,0.233){\line(-0.062,-0.108){0.062}}
\put(0.25,0.){\line(0.125,0.){0.125}}
\put(0.375,0.){\line(-0.062,0.108){0.062}}
\put(0.312,0.108){\line(-0.062,-0.108){0.062}}
\put(0.375,0.){\line(0.125,0.){0.125}}
\put(0.5,0.){\line(-0.062,0.108){0.062}}
\put(0.437,0.108){\line(-0.062,-0.108){0.062}}
\put(0.312,0.125){\line(0.125,0.){0.125}}
\put(0.437,0.125){\line(-0.062,0.108){0.062}}
\put(0.375,0.233){\line(-0.062,-0.108){0.062}}
\put(0.125,0.25){\line(0.125,0.){0.125}}
\put(0.25,0.25){\line(-0.062,0.108){0.062}}
\put(0.187,0.358){\line(-0.062,-0.108){0.062}}
\put(0.25,0.25){\line(0.125,0.){0.125}}
\put(0.375,0.25){\line(-0.062,0.108){0.062}}
\put(0.312,0.358){\line(-0.062,-0.108){0.062}}
\put(0.187,0.375){\line(0.125,0.){0.125}}
\put(0.312,0.375){\line(-0.062,0.108){0.062}}
\put(0.25,0.483){\line(-0.062,-0.108){0.062}}
\put(0.5,0.){\line(0.125,0.){0.125}}
\put(0.625,0.){\line(-0.062,0.108){0.062}}
\put(0.562,0.108){\line(-0.062,-0.108){0.062}}
\put(0.625,0.){\line(0.125,0.){0.125}}
\put(0.75,0.){\line(-0.062,0.108){0.062}}
\put(0.687,0.108){\line(-0.062,-0.108){0.062}}
\put(0.562,0.125){\line(0.125,0.){0.125}}
\put(0.687,0.125){\line(-0.062,0.108){0.062}}
\put(0.625,0.233){\line(-0.062,-0.108){0.062}}
\put(0.75,0.){\line(0.125,0.){0.125}}
\put(0.875,0.){\line(-0.062,0.108){0.062}}
\put(0.812,0.108){\line(-0.062,-0.108){0.062}}
\put(0.875,0.){\line(0.125,0.){0.125}}
\put(1.,0.){\line(-0.062,0.108){0.062}}
\put(0.937,0.108){\line(-0.062,-0.108){0.062}}
\put(0.812,0.125){\line(0.125,0.){0.125}}
\put(0.937,0.125){\line(-0.062,0.108){0.062}}
\put(0.875,0.233){\line(-0.062,-0.108){0.062}}
\put(0.625,0.25){\line(0.125,0.){0.125}}
\put(0.75,0.25){\line(-0.062,0.108){0.062}}
\put(0.687,0.358){\line(-0.062,-0.108){0.062}}
\put(0.75,0.25){\line(0.125,0.){0.125}}
\put(0.875,0.25){\line(-0.062,0.108){0.062}}
\put(0.812,0.358){\line(-0.062,-0.108){0.062}}
\put(0.687,0.375){\line(0.125,0.){0.125}}
\put(0.812,0.375){\line(-0.062,0.108){0.062}}
\put(0.75,0.483){\line(-0.062,-0.108){0.062}}
\put(0.25,0.5){\line(0.125,0.){0.125}}
\put(0.375,0.5){\line(-0.062,0.108){0.062}}
\put(0.312,0.608){\line(-0.062,-0.108){0.062}}
\put(0.375,0.5){\line(0.125,0.){0.125}}
\put(0.5,0.5){\line(-0.062,0.108){0.062}}
\put(0.437,0.608){\line(-0.062,-0.108){0.062}}
\put(0.312,0.625){\line(0.125,0.){0.125}}
\put(0.437,0.625){\line(-0.062,0.108){0.062}}
\put(0.375,0.733){\line(-0.062,-0.108){0.062}}
\put(0.5,0.5){\line(0.125,0.){0.125}}
\put(0.625,0.5){\line(-0.062,0.108){0.062}}
\put(0.562,0.608){\line(-0.062,-0.108){0.062}}
\put(0.625,0.5){\line(0.125,0.){0.125}}
\put(0.75,0.5){\line(-0.062,0.108){0.062}}
\put(0.687,0.608){\line(-0.062,-0.108){0.062}}
\put(0.562,0.625){\line(0.125,0.){0.125}}
\put(0.687,0.625){\line(-0.062,0.108){0.062}}
\put(0.625,0.733){\line(-0.062,-0.108){0.062}}
\put(0.375,0.75){\line(0.125,0.){0.125}}
\put(0.5,0.75){\line(-0.062,0.108){0.062}}
\put(0.437,0.858){\line(-0.062,-0.108){0.062}}
\put(0.5,0.75){\line(0.125,0.){0.125}}
\put(0.625,0.75){\line(-0.062,0.108){0.062}}
\put(0.562,0.858){\line(-0.062,-0.108){0.062}}
\put(0.437,0.875){\line(0.125,0.){0.125}}
\put(0.562,0.875){\line(-0.062,0.108){0.062}}
\put(0.5,0.983){\line(-0.062,-0.108){0.062}}

(* draw (sierpinski 4);; *)
\put(0.,0.){\line(0.062,0.){0.062}}
\put(0.062,0.){\line(-0.031,0.054){0.031}}
\put(0.031,0.054){\line(-0.031,-0.054){0.031}}
\put(0.062,0.){\line(0.062,0.){0.062}}
\put(0.125,0.){\line(-0.031,0.054){0.031}}
\put(0.093,0.054){\line(-0.031,-0.054){0.031}}
\put(0.031,0.062){\line(0.062,0.){0.062}}
\put(0.093,0.062){\line(-0.031,0.054){0.031}}
\put(0.062,0.116){\line(-0.031,-0.054){0.031}}
\put(0.125,0.){\line(0.062,0.){0.062}}
\put(0.187,0.){\line(-0.031,0.054){0.031}}
\put(0.156,0.054){\line(-0.031,-0.054){0.031}}
\put(0.187,0.){\line(0.062,0.){0.062}}
\put(0.25,0.){\line(-0.031,0.054){0.031}}
\put(0.218,0.054){\line(-0.031,-0.054){0.031}}
\put(0.156,0.062){\line(0.062,0.){0.062}}
\put(0.218,0.062){\line(-0.031,0.054){0.031}}
\put(0.187,0.116){\line(-0.031,-0.054){0.031}}
\put(0.062,0.125){\line(0.062,0.){0.062}}
\put(0.125,0.125){\line(-0.031,0.054){0.031}}
\put(0.093,0.179){\line(-0.031,-0.054){0.031}}
\put(0.125,0.125){\line(0.062,0.){0.062}}
\put(0.187,0.125){\line(-0.031,0.054){0.031}}
\put(0.156,0.179){\line(-0.031,-0.054){0.031}}
\put(0.093,0.187){\line(0.062,0.){0.062}}
\put(0.156,0.187){\line(-0.031,0.054){0.031}}
\put(0.125,0.241){\line(-0.031,-0.054){0.031}}
\put(0.25,0.){\line(0.062,0.){0.062}}
\put(0.312,0.){\line(-0.031,0.054){0.031}}
\put(0.281,0.054){\line(-0.031,-0.054){0.031}}
\put(0.312,0.){\line(0.062,0.){0.062}}
\put(0.375,0.){\line(-0.031,0.054){0.031}}
\put(0.343,0.054){\line(-0.031,-0.054){0.031}}
\put(0.281,0.062){\line(0.062,0.){0.062}}
\put(0.343,0.062){\line(-0.031,0.054){0.031}}
\put(0.312,0.116){\line(-0.031,-0.054){0.031}}
\put(0.375,0.){\line(0.062,0.){0.062}}
\put(0.437,0.){\line(-0.031,0.054){0.031}}
\put(0.406,0.054){\line(-0.031,-0.054){0.031}}
\put(0.437,0.){\line(0.062,0.){0.062}}
\put(0.5,0.){\line(-0.031,0.054){0.031}}
\put(0.468,0.054){\line(-0.031,-0.054){0.031}}
\put(0.406,0.062){\line(0.062,0.){0.062}}
\put(0.468,0.062){\line(-0.031,0.054){0.031}}
\put(0.437,0.116){\line(-0.031,-0.054){0.031}}
\put(0.312,0.125){\line(0.062,0.){0.062}}
\put(0.375,0.125){\line(-0.031,0.054){0.031}}
\put(0.343,0.179){\line(-0.031,-0.054){0.031}}
\put(0.375,0.125){\line(0.062,0.){0.062}}
\put(0.437,0.125){\line(-0.031,0.054){0.031}}
\put(0.406,0.179){\line(-0.031,-0.054){0.031}}
\put(0.343,0.187){\line(0.062,0.){0.062}}
\put(0.406,0.187){\line(-0.031,0.054){0.031}}
\put(0.375,0.241){\line(-0.031,-0.054){0.031}}
\put(0.125,0.25){\line(0.062,0.){0.062}}
\put(0.187,0.25){\line(-0.031,0.054){0.031}}
\put(0.156,0.304){\line(-0.031,-0.054){0.031}}
\put(0.187,0.25){\line(0.062,0.){0.062}}
\put(0.25,0.25){\line(-0.031,0.054){0.031}}
\put(0.218,0.304){\line(-0.031,-0.054){0.031}}
\put(0.156,0.312){\line(0.062,0.){0.062}}
\put(0.218,0.312){\line(-0.031,0.054){0.031}}
\put(0.187,0.366){\line(-0.031,-0.054){0.031}}
\put(0.25,0.25){\line(0.062,0.){0.062}}
\put(0.312,0.25){\line(-0.031,0.054){0.031}}
\put(0.281,0.304){\line(-0.031,-0.054){0.031}}
\put(0.312,0.25){\line(0.062,0.){0.062}}
\put(0.375,0.25){\line(-0.031,0.054){0.031}}
\put(0.343,0.304){\line(-0.031,-0.054){0.031}}
\put(0.281,0.312){\line(0.062,0.){0.062}}
\put(0.343,0.312){\line(-0.031,0.054){0.031}}
\put(0.312,0.366){\line(-0.031,-0.054){0.031}}
\put(0.187,0.375){\line(0.062,0.){0.062}}
\put(0.25,0.375){\line(-0.031,0.054){0.031}}
\put(0.218,0.429){\line(-0.031,-0.054){0.031}}
\put(0.25,0.375){\line(0.062,0.){0.062}}
\put(0.312,0.375){\line(-0.031,0.054){0.031}}
\put(0.281,0.429){\line(-0.031,-0.054){0.031}}
\put(0.218,0.437){\line(0.062,0.){0.062}}
\put(0.281,0.437){\line(-0.031,0.054){0.031}}
\put(0.25,0.491){\line(-0.031,-0.054){0.031}}
\put(0.5,0.){\line(0.062,0.){0.062}}
\put(0.562,0.){\line(-0.031,0.054){0.031}}
\put(0.531,0.054){\line(-0.031,-0.054){0.031}}
\put(0.562,0.){\line(0.062,0.){0.062}}
\put(0.625,0.){\line(-0.031,0.054){0.031}}
\put(0.593,0.054){\line(-0.031,-0.054){0.031}}
\put(0.531,0.062){\line(0.062,0.){0.062}}
\put(0.593,0.062){\line(-0.031,0.054){0.031}}
\put(0.562,0.116){\line(-0.031,-0.054){0.031}}
\put(0.625,0.){\line(0.062,0.){0.062}}
\put(0.687,0.){\line(-0.031,0.054){0.031}}
\put(0.656,0.054){\line(-0.031,-0.054){0.031}}
\put(0.687,0.){\line(0.062,0.){0.062}}
\put(0.75,0.){\line(-0.031,0.054){0.031}}
\put(0.718,0.054){\line(-0.031,-0.054){0.031}}
\put(0.656,0.062){\line(0.062,0.){0.062}}
\put(0.718,0.062){\line(-0.031,0.054){0.031}}
\put(0.687,0.116){\line(-0.031,-0.054){0.031}}
\put(0.562,0.125){\line(0.062,0.){0.062}}
\put(0.625,0.125){\line(-0.031,0.054){0.031}}
\put(0.593,0.179){\line(-0.031,-0.054){0.031}}
\put(0.625,0.125){\line(0.062,0.){0.062}}
\put(0.687,0.125){\line(-0.031,0.054){0.031}}
\put(0.656,0.179){\line(-0.031,-0.054){0.031}}
\put(0.593,0.187){\line(0.062,0.){0.062}}
\put(0.656,0.187){\line(-0.031,0.054){0.031}}
\put(0.625,0.241){\line(-0.031,-0.054){0.031}}
\put(0.75,0.){\line(0.062,0.){0.062}}
\put(0.812,0.){\line(-0.031,0.054){0.031}}
\put(0.781,0.054){\line(-0.031,-0.054){0.031}}
\put(0.812,0.){\line(0.062,0.){0.062}}
\put(0.875,0.){\line(-0.031,0.054){0.031}}
\put(0.843,0.054){\line(-0.031,-0.054){0.031}}
\put(0.781,0.062){\line(0.062,0.){0.062}}
\put(0.843,0.062){\line(-0.031,0.054){0.031}}
\put(0.812,0.116){\line(-0.031,-0.054){0.031}}
\put(0.875,0.){\line(0.062,0.){0.062}}
\put(0.937,0.){\line(-0.031,0.054){0.031}}
\put(0.906,0.054){\line(-0.031,-0.054){0.031}}
\put(0.937,0.){\line(0.062,0.){0.062}}
\put(1.,0.){\line(-0.031,0.054){0.031}}
\put(0.968,0.054){\line(-0.031,-0.054){0.031}}
\put(0.906,0.062){\line(0.062,0.){0.062}}
\put(0.968,0.062){\line(-0.031,0.054){0.031}}
\put(0.937,0.116){\line(-0.031,-0.054){0.031}}
\put(0.812,0.125){\line(0.062,0.){0.062}}
\put(0.875,0.125){\line(-0.031,0.054){0.031}}
\put(0.843,0.179){\line(-0.031,-0.054){0.031}}
\put(0.875,0.125){\line(0.062,0.){0.062}}
\put(0.937,0.125){\line(-0.031,0.054){0.031}}
\put(0.906,0.179){\line(-0.031,-0.054){0.031}}
\put(0.843,0.187){\line(0.062,0.){0.062}}
\put(0.906,0.187){\line(-0.031,0.054){0.031}}
\put(0.875,0.241){\line(-0.031,-0.054){0.031}}
\put(0.625,0.25){\line(0.062,0.){0.062}}
\put(0.687,0.25){\line(-0.031,0.054){0.031}}
\put(0.656,0.304){\line(-0.031,-0.054){0.031}}
\put(0.687,0.25){\line(0.062,0.){0.062}}
\put(0.75,0.25){\line(-0.031,0.054){0.031}}
\put(0.718,0.304){\line(-0.031,-0.054){0.031}}
\put(0.656,0.312){\line(0.062,0.){0.062}}
\put(0.718,0.312){\line(-0.031,0.054){0.031}}
\put(0.687,0.366){\line(-0.031,-0.054){0.031}}
\put(0.75,0.25){\line(0.062,0.){0.062}}
\put(0.812,0.25){\line(-0.031,0.054){0.031}}
\put(0.781,0.304){\line(-0.031,-0.054){0.031}}
\put(0.812,0.25){\line(0.062,0.){0.062}}
\put(0.875,0.25){\line(-0.031,0.054){0.031}}
\put(0.843,0.304){\line(-0.031,-0.054){0.031}}
\put(0.781,0.312){\line(0.062,0.){0.062}}
\put(0.843,0.312){\line(-0.031,0.054){0.031}}
\put(0.812,0.366){\line(-0.031,-0.054){0.031}}
\put(0.687,0.375){\line(0.062,0.){0.062}}
\put(0.75,0.375){\line(-0.031,0.054){0.031}}
\put(0.718,0.429){\line(-0.031,-0.054){0.031}}
\put(0.75,0.375){\line(0.062,0.){0.062}}
\put(0.812,0.375){\line(-0.031,0.054){0.031}}
\put(0.781,0.429){\line(-0.031,-0.054){0.031}}
\put(0.718,0.437){\line(0.062,0.){0.062}}
\put(0.781,0.437){\line(-0.031,0.054){0.031}}
\put(0.75,0.491){\line(-0.031,-0.054){0.031}}
\put(0.25,0.5){\line(0.062,0.){0.062}}
\put(0.312,0.5){\line(-0.031,0.054){0.031}}
\put(0.281,0.554){\line(-0.031,-0.054){0.031}}
\put(0.312,0.5){\line(0.062,0.){0.062}}
\put(0.375,0.5){\line(-0.031,0.054){0.031}}
\put(0.343,0.554){\line(-0.031,-0.054){0.031}}
\put(0.281,0.562){\line(0.062,0.){0.062}}
\put(0.343,0.562){\line(-0.031,0.054){0.031}}
\put(0.312,0.616){\line(-0.031,-0.054){0.031}}
\put(0.375,0.5){\line(0.062,0.){0.062}}
\put(0.437,0.5){\line(-0.031,0.054){0.031}}
\put(0.406,0.554){\line(-0.031,-0.054){0.031}}
\put(0.437,0.5){\line(0.062,0.){0.062}}
\put(0.5,0.5){\line(-0.031,0.054){0.031}}
\put(0.468,0.554){\line(-0.031,-0.054){0.031}}
\put(0.406,0.562){\line(0.062,0.){0.062}}
\put(0.468,0.562){\line(-0.031,0.054){0.031}}
\put(0.437,0.616){\line(-0.031,-0.054){0.031}}
\put(0.312,0.625){\line(0.062,0.){0.062}}
\put(0.375,0.625){\line(-0.031,0.054){0.031}}
\put(0.343,0.679){\line(-0.031,-0.054){0.031}}
\put(0.375,0.625){\line(0.062,0.){0.062}}
\put(0.437,0.625){\line(-0.031,0.054){0.031}}
\put(0.406,0.679){\line(-0.031,-0.054){0.031}}
\put(0.343,0.687){\line(0.062,0.){0.062}}
\put(0.406,0.687){\line(-0.031,0.054){0.031}}
\put(0.375,0.741){\line(-0.031,-0.054){0.031}}
\put(0.5,0.5){\line(0.062,0.){0.062}}
\put(0.562,0.5){\line(-0.031,0.054){0.031}}
\put(0.531,0.554){\line(-0.031,-0.054){0.031}}
\put(0.562,0.5){\line(0.062,0.){0.062}}
\put(0.625,0.5){\line(-0.031,0.054){0.031}}
\put(0.593,0.554){\line(-0.031,-0.054){0.031}}
\put(0.531,0.562){\line(0.062,0.){0.062}}
\put(0.593,0.562){\line(-0.031,0.054){0.031}}
\put(0.562,0.616){\line(-0.031,-0.054){0.031}}
\put(0.625,0.5){\line(0.062,0.){0.062}}
\put(0.687,0.5){\line(-0.031,0.054){0.031}}
\put(0.656,0.554){\line(-0.031,-0.054){0.031}}
\put(0.687,0.5){\line(0.062,0.){0.062}}
\put(0.75,0.5){\line(-0.031,0.054){0.031}}
\put(0.718,0.554){\line(-0.031,-0.054){0.031}}
\put(0.656,0.562){\line(0.062,0.){0.062}}
\put(0.718,0.562){\line(-0.031,0.054){0.031}}
\put(0.687,0.616){\line(-0.031,-0.054){0.031}}
\put(0.562,0.625){\line(0.062,0.){0.062}}
\put(0.625,0.625){\line(-0.031,0.054){0.031}}
\put(0.593,0.679){\line(-0.031,-0.054){0.031}}
\put(0.625,0.625){\line(0.062,0.){0.062}}
\put(0.687,0.625){\line(-0.031,0.054){0.031}}
\put(0.656,0.679){\line(-0.031,-0.054){0.031}}
\put(0.593,0.687){\line(0.062,0.){0.062}}
\put(0.656,0.687){\line(-0.031,0.054){0.031}}
\put(0.625,0.741){\line(-0.031,-0.054){0.031}}
\put(0.375,0.75){\line(0.062,0.){0.062}}
\put(0.437,0.75){\line(-0.031,0.054){0.031}}
\put(0.406,0.804){\line(-0.031,-0.054){0.031}}
\put(0.437,0.75){\line(0.062,0.){0.062}}
\put(0.5,0.75){\line(-0.031,0.054){0.031}}
\put(0.468,0.804){\line(-0.031,-0.054){0.031}}
\put(0.406,0.812){\line(0.062,0.){0.062}}
\put(0.468,0.812){\line(-0.031,0.054){0.031}}
\put(0.437,0.866){\line(-0.031,-0.054){0.031}}
\put(0.5,0.75){\line(0.062,0.){0.062}}
\put(0.562,0.75){\line(-0.031,0.054){0.031}}
\put(0.531,0.804){\line(-0.031,-0.054){0.031}}
\put(0.562,0.75){\line(0.062,0.){0.062}}
\put(0.625,0.75){\line(-0.031,0.054){0.031}}
\put(0.593,0.804){\line(-0.031,-0.054){0.031}}
\put(0.531,0.812){\line(0.062,0.){0.062}}
\put(0.593,0.812){\line(-0.031,0.054){0.031}}
\put(0.562,0.866){\line(-0.031,-0.054){0.031}}
\put(0.437,0.875){\line(0.062,0.){0.062}}
\put(0.5,0.875){\line(-0.031,0.054){0.031}}
\put(0.468,0.929){\line(-0.031,-0.054){0.031}}
\put(0.5,0.875){\line(0.062,0.){0.062}}
\put(0.562,0.875){\line(-0.031,0.054){0.031}}
\put(0.531,0.929){\line(-0.031,-0.054){0.031}}
\put(0.468,0.937){\line(0.062,0.){0.062}}
\put(0.531,0.937){\line(-0.031,0.054){0.031}}
\put(0.5,0.991){\line(-0.031,-0.054){0.031}}
*)
