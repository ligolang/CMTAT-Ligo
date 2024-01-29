let nth_exn (type a) (i: int) (a: a list) : a =
  let rec aux (remaining: a list) (cur: int) : a =
    match remaining with 
    [] -> 
    failwith "Not found in list"
    | hd :: tl -> 
      if cur = i then 
      hd 
      else aux tl (cur + 1)
  in
  aux a 0  

let contains (type a) (value: a) (lst: a list) : bool =
  let retrieve (acc, elt: (a * bool) * a) = value, (acc.1 || (Test.equal elt acc.0)) in  
  let _v, result = List.fold retrieve lst (value, false) in
  result