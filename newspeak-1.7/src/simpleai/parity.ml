
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
  | Even | Odd

let universe = Top

let singleton i = 
  if Int32.compare (Int32.rem i 2l) Int32.zero = 0 then Even else Odd
							    

let of_bounds (l, u) =  if l = u then singleton l else Top


let contains x y =
  match (x, y) with
    Top, _ -> true
  | p1,p2 when p1 = p2 -> true
  | _ -> false

let join x y =
  match (x, y) with
    Even,Even -> Even
  | Odd,Odd -> Odd
  | _ -> Top

let neg _ = Top

let widen = join

let add x y =
  match (x, y) with
    Even,Even -> Even
  | Even,Odd -> Odd
  | Odd, Even -> Odd
  | Odd,Odd -> Even
  | _ -> Top

let sub x y =
  match (x, y) with
  | Even, Even -> Even
  | Even,Odd -> Odd
  | Odd, Even -> Odd
  | Odd,Odd -> Even
  | _ -> Top
	   

let mult x y =
  match (x, y) with
  | _ , Even -> Even
  | Even, _ -> Even
  | Odd , Odd -> Odd
  | _ -> Top


let div _ _ = Top



let modulo x y =
  match (x, y) with
  | Even , Even -> Even
  | _ -> Top


let is_safe_add _ _ = false

let is_safe_sub _ _ = false

let is_safe_mult _ _ = false

let is_safe_div _ _ = false

let is_safe_modulo _ _ = false


let implies (_,_,_) = false

(* Restricts the value x to make the condition 
   c op x true *)
let guard op c x = 
  match (op, c, x) with

    | (EQ, Even, Odd) -> raise Emptyset

    | (EQ, Odd, Even) -> raise Emptyset

    | _ -> x



let to_string v = 
  match v with
    Even -> "Even"
  | Odd -> "Odd"
  | Top -> "?"

 
