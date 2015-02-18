
type t = Top | Bounds of Int32.t * Int32.t

    val universe: t
    val singleton: Int32.t -> t
    val of_bounds: (Int32.t * Int32.t) -> t
    val join: t -> t -> t
    val contains: t -> t -> bool
    val implies: (t * Simple.cmp * Int32.t) -> bool
    val widen: t -> t -> t

    val neg: t -> t
    val add: t -> t -> t
    val sub: t -> t -> t
    val mult: t -> t -> t
    val div: t -> t -> t
    val modulo : t -> t -> t

    val is_safe_add: t -> t -> bool
    val is_safe_sub: t -> t -> bool
    val is_safe_mult: t -> t -> bool
    val is_safe_div: t -> t -> bool
    val is_safe_modulo: t -> t -> bool

    val guard: UnrelState.bop -> t -> t -> t
    val to_string: t -> string
