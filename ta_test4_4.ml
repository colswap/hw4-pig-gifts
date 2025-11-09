(*Header*)

let rec eval cond (shops : (id * gift list) list) : gift list =
  match cond with
  | Items gifts -> gifts
  | Same id -> List.assoc id shops
  | Common (c1, c2) ->
    let gifts1 = eval c1 shops in
    let gifts2 = eval c2 shops in
    List.filter (fun gift -> List.mem gift gifts2) gifts1
  | Except (c1, c2) ->
    let gifts1 = eval c1 shops in
    let gifts2 = eval c2 shops in
    List.filter (fun gift -> not (List.mem gift gifts2)) gifts1

let check_satisfy (reqs : require list) (shops : (id * gift list) list) : bool =
  let required_gifts = List.map (fun (id, conds) ->
    (id, (List.concat_map (fun cond -> eval cond shops) conds
  |> List.sort_uniq compare))
  ) reqs in
  let happy : bool list = List.map (fun (id, gifts) ->
    let shop_gifts = List.assoc id shops in
    List.for_all (fun gift -> List.mem gift shop_gifts) gifts
  ) required_gifts in
  List.for_all (fun x -> x) happy

let check_size reqs =
  let shops = shoppingList reqs in
  let shops_size =
    shops
    |> List.map (fun (_, gifts) -> List.length gifts)
    |> List.fold_left (+) 0
  in
  if check_satisfy reqs shops then shops_size else -1

(*Test*)
(* 1 *)
shoppingList [(A, [Items[1]]); (B, [Items [2]]); (C, []); (D, []); (E, [])]

(*Value*)
[(A,[1]); (B,[2]); (C,[]); (D,[]); (E,[])]


(*Test*)
(* 2 *)
shoppingList [(A, [Items [1;2]]); (B, [Items [2;3]]); (C, [Common(Same A, Same B)]); (D, []); (E, [])]

(*Value*)
[(A,[1;2]); (B,[2;3]); (C,[2]); (D,[]); (E,[])]


(*Test*)
(* 3 *)
shoppingList [(A, [Items[1;2]]); (B, [Except(Same A, Items[1])]); (C, []); (D, []); (E, [])]

(*Value*)
[(A,[1;2]); (B,[2]); (C,[]); (D,[]); (E,[])]


(*Test*)
(* 4 *)
shoppingList [(A, [Same B]); (B, [Same A]); (C, []); (D, []); (E, [])]

(*Value*)
[(A,[]); (B,[]); (C,[]); (D,[]); (E,[])]


(*Test*)
(* 5 *)
shoppingList [(A, [Items [1]; Same B]); (B, [Items [2]; Same C]); (C, [Items [3]; Except(Same A, Items[1;2])]); (D,[]); (E, [])]

(*Value*)
[(A, [1;2;3]); (B, [2;3]); (C, [3]); (D, []); (E, [])]


(*Test*)
(* 6 *)
shoppingList [(A, [Items [1]; Same B]); (B, [Items [2]; Same C]); (C, [Common(Items [2], Same A)]); (D, []); (E, [])]

(*Value*)
[(A, [1;2]); (B, [2]); (C, [2]); (D, []); (E, [])]


(*Test*)
(* 7 *)
shoppingList [(A, [Items [1]; Same B]); (B, [Items[2];Same C]); (C, [Items [3];Same B]); (D, [Same A; Common (Same B, Same C)]); (E, [Same A; Same D]) ]

(*Value*)
[(A, [1;2;3]); (B, [2;3]); (C, [2;3]); (D, [1;2;3]); (E, [1;2;3])]


(*Test*)
(* 8 *)
shoppingList [(A, [Items [1;2;3]; Common(Same B, Same C)]); (B, [Common (Same C, Items [2;3])]); (C, [Items[1;3]; Except (Same A, Items[3])]); (D, [Common (Same A, Same B)]); (E, [Common (Same A, Same C)])]

(*Value*)
[(A, [1;2;3]); (B, [2;3]); (C, [1;2;3]); (D, [2;3]); (E, [1;2;3])]


(*Test*)
(* 9 *)
shoppingList [(A, [Items[1]; Except (Same B, Items[1])]); (B, [Items[2]; Except (Same C, Items[2])]); (C, [Items[3]; Except (Same D, Items[3])]); (D, [Items[4]; Except (Same E, Items[4])]); (E, [Items[5]; Except (Same A, Items[5])])]
 
(*Value*)
[(A,[1;2;3;4;5]); (B,[1;2;3;4;5]); (C,[1;2;3;4;5]); (D,[1;2;3;4;5]); (E,[1;2;3;4;5])]


(*Test*)
(* 10 *)
shoppingList [(A, [Items[1]; Common(Items[2;3;4], Except (Same B, Items[1;2]))]); (B, [Items[2]; Common(Items[4], Except (Same C, Items[2;3]))]); (C, [Items[3]; Common(Items[4;5], Except (Same D, Items[3]))]); (D, [Items[4]; Common(Items[5], Except (Same E, Items[4]))]); (E, [Items[5]; Except (Same A, Items[5])])]
 
(*Value*)
[(A,[1;4]); (B,[2;4]); (C,[3;4;5]); (D,[4;5]); (E,[1;4;5])]

(*Test*)
(* 11 *)
check_size [(A, [Except (Items[1], Same B)]); (B, [Except (Items[1], Same A)]); (C, [Except (Items[1], Same E)]); (D, [Except (Items[1], Same E)]); (E, [])]

(*Value*)
2

(*Test*)
(* 12 *)
check_size [(A, [Except (Items[1], Same B)]); (B, [Except (Items[2], Same A)]); (C, [Except (Items[1;2], Same D)]); (D, []); (E, [Common (Same A, Same D)])]

(*Value*)
4

(*Test*)
(* 13 *)
check_size [(A, [Except (Items[1;3], Same B); Except (Items[2;4], Same C)]); (B, [Except (Items[1;2], Same A); Except (Items[3;4], Same C)]); (C, [Except (Items[2;3], Same A); Except (Items[1;4], Same B)]); (D, []); (E, [])]

(*Value*)
5

(*Test*)
(* 14 *)
check_size [(A, [Except (Items[5], Common (Same D, Same C))]); (B, []); (C, [Except (Items[5], Same D)]); (D, [Except (Items[5], Same E)]); (E, [Except (Items[5], Same C)])]

(*Value*)
2

(*Test*)
(* 15 *)
check_size [(A, [Except (Items[1;2], Same B)]); (B, [Except (Items[3;4], Same A)]); (C, [Same A]); (D, []); (E, [])]

(*Value*)
4