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

let reverse (type a) (xs : a list) : a list =
    let f (ys,x : (a list * a)) : a list = x :: ys in
    List.fold_left f ([] : a list) xs


let rec zip (type a b) (x, y : a list * b list) : (a * b) list =
    match (x, y) with
    | hd1::tl1, hd2::tl2 -> (hd1, hd2) :: (zip (tl1, tl2))  
    | _ -> []

let rec zipWith (type a b c) (f: ((a * b) -> c)) (lst1 : a list) (lst2: b list) : c list =
    match (lst1, lst2) with
    | ([], _) -> []
    | (_, []) -> []
    | (x::xs, y::ys) -> (f (x, y)) :: zipWith f xs ys