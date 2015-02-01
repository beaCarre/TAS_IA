 (*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2007  Charles Hymans
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.
  
  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

  Charles Hymans
  EADS Innovation Works - SE/CS
  12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
  email: charles.hymans@penjili.org
 *)

open UnrelState



type t = 
    Top
  | Bounds of Int32.t * Int32.t

let universe = Top

let singleton i = Bounds (i, i)

let of_bounds (l, u) =  if l <= u then Bounds (l, u) else Top

let contains x y =
  match (x, y) with
    (Top, _) -> true
  | (Bounds(i,j), Bounds (m,n)) when i<=m && j>= n -> true
  | _ -> false

let minint32 a b =
  if Int32.compare a b < 0 then a else b

let maxint32 a b =
  if Int32.compare a b > 0 then a else b

let join x y =
  match (x, y) with
    (Bounds (i,j), Bounds(m,n)) -> Bounds (minint32 i m, maxint32 j n)
  | _ -> Top

let widen x y = x

(* | Bot, _ -> y
  | _, Bot -> x
  | Itv (a, b), Itv (c, d) ->
    let e = if leq_minf a c then a else None in
    let f = if leq_pinf d b then b else None in
    mk_itv e f *)

(*let widening x y = match x, y with
  | Bot, _ -> y
  | _, Bot -> x
  | Itv (cptx, (a, b)), Itv (cpty, (c, d)) ->
    let cpt = max cptx cpty in
    if cpt < delay then
      (* Les delay premières fois, on fait un join et on le compte (+1). *)
      join (Itv (cpt + 1, (a, b))) (Itv (cpt + 1, (c, d)))
    else
      (* Ensuite on fait un widening normal. *)
      let e = if leq_minf a c then a else None in
      let f = if leq_pinf d b then b else None in
      mk_itv cpt e f*)

let neg x =
  match x with
    Bounds (i,j) when i = Int32.zero && i = j -> singleton Int32.one
  | Bounds (i,j) when (i < Int32.zero && j < Int32.zero) || (i > Int32.zero && j > Int32.zero)  -> singleton Int32.zero
  | _ -> Top


let add3264 x y =
  let xaddy32 = Int32.add x y in
  let xaddy64 = Int64.add (Int64.of_int32 x) (Int64.of_int32 y) in
  (Int64.compare (Int64.of_int32 xaddy32) xaddy64) == 0

let add x y =
  match (x, y) with
  (Bounds(a,b), Bounds(c,d)) -> 
    if (add3264 a c) && (add3264 b d) 
    then Bounds (Int32.add a c, Int32.add b d)
    else Top
  | _ -> Top

let sub3264 x y =
  let xsuby32 = Int32.sub x y in
  let xsuby64 = Int64.sub (Int64.of_int32 x) (Int64.of_int32 y) in
  (Int64.compare (Int64.of_int32 xsuby32) xsuby64) == 0

let sub x y =
  match (x, y) with
  (Bounds(a,b), Bounds(c,d)) -> 
    if (sub3264 a d) && (sub3264 b c) 
    then Bounds (Int32.sub a d, Int32.sub b c)
    else Top
  | _ -> Top
	   
let mult3264 x y =
  let xy32 = Int32.mul x y in
  let xy64 = Int64.mul (Int64.of_int32 x) (Int64.of_int32 y) in
  (Int64.compare (Int64.of_int32 xy32) xy64) == 0

let mult x y =
  match (x, y) with
  | (Bounds(a,b), Bounds(c,d)) -> 
    let ac = Int32.mul a c in
    let ad = Int32.mul a d in
    let bc = Int32.mul b c in
    let bd = Int32.mul b d in
    if (mult3264 a c) && (mult3264 a d) && (mult3264 b c) && (mult3264 b d) 
    then Bounds (List.fold_left minint32 Int32.max_int [ac;ad;bc;bd] , List.fold_left maxint32 Int32.min_int [ac;ad;bc;bd] ) 
    else Top
  | _ -> Top


let div3264 x y =
  let xy32 = Int32.div x y in
  let xy64 = Int64.div (Int64.of_int32 x) (Int64.of_int32 y) in
  (Int64.compare (Int64.of_int32 xy32) xy64) == 0

let div x y =
  match (x, y) with
  | (Bounds(a,b), Bounds(c,d)) when c > Int32.zero || d < Int32.zero -> 
     let ac = Int32.div a c in
     let ad = Int32.div a d in
     let bc = Int32.div b c in
     let bd = Int32.div b d in
     if (div3264 a c) && (div3264 a d) && (div3264 b c) && (div3264 b d) 
     then Bounds (List.fold_left minint32 Int32.max_int [ac;ad;bc;bd] , List.fold_left maxint32 Int32.min_int [ac;ad;bc;bd] ) 
     else Top
  | _ -> Top
  
let modulo3264 x y =
  let xy32 = Int32.rem x y in
  let xy34 = Int64.rem (Int64.of_int32 x) (Int64.of_int32 y) in
  (Int64.compare (Int64.of_int32 xy32) xy34) == 0

let modulo x y =
  match (x, y) with
  | (Bounds(a,b), Bounds(c,d)) ->
     if modulo3264 a c && modulo3264 a d && modulo3264 b c && modulo3264 b d 
     then Bounds(Int32.zero,max b d)
     else Top
  | _ -> Top

let is_safe_add x y =
  match (x, y) with
  | (Bounds(a,b), Bounds(c,d)) ->
     (add3264 a c) && (add3264 b d)
  | _ -> false

let is_safe_sub x y =
  match (x, y) with
  | (Bounds(a,b), Bounds(c,d)) ->
      (sub3264 a c) && (sub3264 b d)
  | _ -> false

let is_safe_mult x y =
  match (x, y) with
  | (Bounds(a,b), Bounds(c,d)) ->
      (mult3264 a c) && (mult3264 b d)
  | _ -> false

let is_safe_div x y =
  match (x, y) with
  | (Bounds(a,b), Bounds(c,d)) when c > Int32.zero || d < Int32.zero ->
      (div3264 a c) && (div3264 b d) 
  (*| (Bounds(a,b), Bounds(c,d)) ->
      (div3264 a c) && (div3264 b d)    TODO disjonction*)
  | _ -> false

let is_safe_modulo x y =
  match (x, y) with
  | (Bounds(a,b), Bounds(c,d)) ->
      (modulo3264 a c) && (modulo3264 b d)
  | _ -> false


let implies (lval, cmp, cst) = 
  match lval with 
  | Bounds(a,b) ->
     (match cmp with
     | Simple.Equals -> (Int64.compare (Int64.of_int32 cst) (Int64.of_int32 a) == 0) && (Int64.compare (Int64.of_int32 cst) (Int64.of_int32 b) == 0)
     | Simple.IsLess -> (Int64.compare (Int64.of_int32 cst) (Int64.of_int32 b) < 0))
  | Top -> false


(* Restricts the value x to make the condition 
   c op x true *)
let guard op c x =
  match (op, c, x) with
  | (LTE, Bounds(a,b), Bounds(_,y)) when Int32.compare a y > 0 -> raise Emptyset
  | (LTE, Bounds(a,b), Bounds(x,y)) when (Int32.compare a x > 0) && (Int32.compare a y <= 0) -> Bounds (a, y)

  | (GTE,Bounds(a,b), Bounds(x,y)) when Int32.compare b x < 0 -> raise Emptyset
  | (GTE,Bounds(a,b), Bounds(x,y)) when (Int32.compare b y < 0) && (Int32.compare b x >= 0)-> Bounds (x, b)

  | (GT,Bounds(a,b), Bounds(x,y)) when Int32.compare a x <= 0 -> raise Emptyset
  | (GT,Bounds(a,b), Bounds(x,y)) when (Int32.compare b y <= 0) && (Int32.compare b x >= 0)-> Bounds (x, Int32.sub b Int32.one)

  | (LT,Bounds(a,b), Bounds(x,y)) when Int32.compare a y >= 0 -> raise Emptyset
  | (LT, Bounds(a,b), Bounds(x,y)) when (Int32.compare a x >= 0) && (Int32.compare a y <= 0) -> Bounds (Int32.add a Int32.one, y)

  | (EQ,Bounds(a,b), Bounds(x,y)) when ((Int32.compare a x < 0)&&(Int32.compare b y < 0))
				       ||((Int32.compare a y > 0)&&(Int32.compare b y > 0)) -> raise Emptyset
  | (EQ,Bounds(a,b), Bounds(x,y)) when (Int32.compare a x >= 0)&&(Int32.compare a y <= 0)  
				       && (Int32.compare b y > 0) -> Bounds(a,y)
  | (EQ,Bounds(a,b), Bounds(x,y)) when (Int32.compare a x >= 0)&&(Int32.compare a y <= 0) 
				       && (Int32.compare b y <= 0) -> Bounds(a,b) 
  | (EQ,Bounds(a,b), Bounds(x,y)) when (Int32.compare b x >= 0)&&(Int32.compare b y <= 0) 	
				       && (Int32.compare a x < 0) -> Bounds(x,b) 
  | (EQ,Bounds(a,b), Bounds(x,y)) when (Int32.compare a x < 0)&&(Int32.compare b y > 0) -> Bounds(x,y)

  | (NEQ,Bounds(a,b), Bounds(x,y)) when ((Int32.compare a x > 0)&&(Int32.compare a y < 0))&&((Int32.compare b x > 0)&&(Int32.compare b y < 0)) -> raise Emptyset
  | (NEQ,Bounds(a,b), Bounds(x,y)) when (Int32.compare a x <= 0)&&(Int32.compare b y >= 0) -> raise Emptyset
  | (NEQ,Bounds(a,b), Bounds(x,y)) when ((Int32.compare a x > 0)&&(Int32.compare a y <= 0))&&(Int32.compare b y >= 0) -> Bounds(x,Int32.add a Int32.one)
  | (NEQ,Bounds(a,b), Bounds(x,y)) when ((Int32.compare b x >= 0)&&(Int32.compare b y < 0))&&(Int32.compare a x <= 0) -> Bounds(x,Int32.add a Int32.one)

  | _ -> x

let to_string v = 
  match v with
    Bounds(a,b) -> "["^(Int32.to_string a)^";"^(Int32.to_string b)^"]"
  | Top -> "?"

