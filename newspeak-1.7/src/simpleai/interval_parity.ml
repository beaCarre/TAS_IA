module A = Interval
module B = Parity


let reduce (i, p) =
  match (i,p) with
    (A.Bounds (a,b), B.Top) ->
    if a = b 
    then (i,B.singleton a)
    else (i,p)
  | (A.Bounds(a,b), _) ->
     let a = if B.singleton a <> p 
	     then Int32.add a Int32.one 
	     else a in
     let b = if B.singleton b <> p 
	     then Int32.sub b Int32.one 
	     else b in
     (A.Bounds((if B.singleton a <> p then Int32.add a Int32.one else a), 
	      (if B.singleton b <> p then Int32.sub b Int32.one else b)), 
      p)
  | _ -> (i,p)
