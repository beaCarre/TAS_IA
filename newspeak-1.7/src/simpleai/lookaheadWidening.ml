open UnrelState

module Make(A: Data) =
  struct

    type t = A.t * A.t
							   
    let universe = (A.universe, A.universe)

    let singleton a = (A.singleton a,A.singleton a)

    let of_bounds (l,u) = 
      (A.of_bounds(l, u),A.of_bounds(l,u))

    let neg (x,y) = (A.neg x),(A.neg y)

    let add (a,b) (c,d) = (A.add a c),(A.add b d)

    let sub (a,b) (c,d) = (A.sub a c),(A.sub b d)

    let mult (a,b) (c,d) = (A.mult a c),(A.mult b d)

    let div (a,b) (c,d) = (A.div a c),(A.div b d)

    let modulo (a,b) (c,d) = (A.modulo a c),(A.modulo b d)

    let is_safe_add (a,b) (c,d) = 
      (A.is_safe_add a c) || (A.is_safe_add b d)
			       
    let is_safe_sub (a,b) (c,d) =
      (A.is_safe_sub a c) || (A.is_safe_sub b d)

    let is_safe_mult (a,b) (c,d) = 
      (A.is_safe_mult a c) || (A.is_safe_mult b d)

    let is_safe_div (a,b) (c,d) = 
      (A.is_safe_mult a c) || (A.is_safe_mult b d)

    let is_safe_modulo (a,b) (c,d) = 
      (A.is_safe_modulo a c) || (A.is_safe_modulo b d)

    let contains (m1, p1) (m2, p2) =
      A.contains m1 m2 || (m1 = m2 && A.contains p1 p2)

    let join (m1, p1) (m2, p2) = A.join m1 m2, A.join p1 p2

    let widen (m1, p1) (m2, p2) =
      if contains (m1, p1) (m2, p2) then (m1, p1)
      else if A.contains p1 p2 then (p2, p2)
      else (A.join m1 m2, A.widen p1 p2)

    let implies ((v1,v2),cmp,c) = 
      (A.implies (v1,cmp,c)) && (A.implies (v2,cmp,c))

    let guard op (c1,c2) (x1,x2) =
      (A.guard op c1 x1),(A.guard op c2 x2)

    let to_string (a,b) = 
      (A.to_string a)^" | "^(A.to_string b)
  end
