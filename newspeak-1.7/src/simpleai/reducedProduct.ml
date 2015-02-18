
module type Product =
  sig 
    module A : UnrelState.Data
    module B : UnrelState.Data
    val reduce : A.t * B.t -> A.t * B.t
  end


module Make (P:Product) =
  struct

    type t = P.A.t * P.B.t

    let universe = (P.A.universe, P.B.universe)

    let singleton s = (P.A.singleton s, P.B.singleton s)

    let of_bounds b = (P.A.of_bounds b, P.B.of_bounds b)

    let contains (a1, b1) (a2, b2) =
      P.A.contains a1 a2 && P.B.contains b1 b2

    let join (a1, b1) (a2, b2) = P.reduce (P.A.join a1 a2, P.B.join b1 b2)
    let widen (a1, b1) (a2, b2) = P.reduce (P.A.widen a1 a2, P.B.widen b1 b2)

    let neg (a, b) = P.reduce (P.A.neg a, P.B.neg b)

    let add (a1, b1) (a2, b2) =
      P.reduce (P.A.add a1 a2, P.B.add b1 b2)
    let sub (a1, b1) (a2, b2) = 
      P.reduce (P.A.sub a1 a2, P.B.sub b1 b2)
    let mult (a1, b1) (a2, b2) = 
      P.reduce (P.A.mult a1 a2, P.B.mult b1 b2)
    let div (a1, b1) (a2, b2) = P.reduce (P.A.div a1 a2, P.B.div b1 b2)
    let modulo (a1, b1) (a2, b2) = P.reduce (P.A.modulo a1 a2, P.B.modulo b1 b2)


    let is_safe_add (a1, b1) (a2, b2) = 
      P.A.is_safe_add a1 a2 &&  P.B.is_safe_add b1 b2
    let is_safe_sub (a1, b1) (a2, b2) = 
      P.A.is_safe_sub a1 a2 && P.B.is_safe_sub b1 b2
    let is_safe_mult (a1, b1) (a2, b2) = 
      P.A.is_safe_mult a1 a2 && P.B.is_safe_mult b1 b2
    let is_safe_div (a1, b1) (a2, b2) = 
      P.A.is_safe_div a1 a2 && P.B.is_safe_div b1 b2
    let is_safe_modulo (a1, b1) (a2, b2) = 
      P.A.is_safe_modulo a1 a2 &&  P.B.is_safe_modulo b1 b2

    let implies ((a, b), cmp, cst) =
      P.A.implies (a,cmp,cst) && P.B.implies (b,cmp,cst)

    let guard op (a1, b1) (a2, b2) = 
      P.reduce (P.A.guard op a1 a2, P.B.guard op b1 b2)

    let to_string (a, b) = "("^(P.A.to_string a)^", "^(P.B.to_string b)^")"
  end
