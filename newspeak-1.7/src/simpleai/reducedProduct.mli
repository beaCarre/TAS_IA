

module type Product =
  sig 
    module A : UnrelState.Data
    module B : UnrelState.Data
    val reduce : A.t * B.t -> A.t * B.t
  end

module Make (P : Product) : UnrelState.Data

