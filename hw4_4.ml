type id = A | B | C | D | E
type gift = int

type cond =
  | Items of gift list
  | Same of id
  | Common of cond * cond
  | Except of cond * gift list

type require = id * (cond list)

exception No_minimum

(* 중복 제거 + 정렬 *)
let sort_unique lst = List.sort_uniq compare lst

(* 조건 해석기: 조교 테스트에 맞춰 인자 순서 변경 *)
let rec eval (cond : cond) (env : (id * gift list) list) : gift list =
  match cond with
  | Items gifts -> sort_unique gifts
  | Same who -> List.assoc who env
  | Common (c1, c2) ->
      let g1 = eval c1 env in
      let g2 = eval c2 env in
      List.filter (fun x -> List.mem x g2) g1
  | Except (c, gl) ->
      let g = eval c env in
      List.filter (fun x -> not (List.mem x gl)) g

(* 조건 리스트를 모두 만족하는 최소 선물 리스트 *)
let satisfy_cond (env : (id * gift list) list) (conds : cond list) : gift list =
  conds |> List.map (fun cond -> eval cond env) |> List.flatten |> sort_unique

(* 모든 돼지에 대한 반복 계산 *)
let rec update_env (env : (id * gift list) list) (reqs : require list)
    : (id * gift list) list =
  let updated = List.map (fun (p, cs) -> (p, satisfy_cond env cs)) reqs in
  if updated = env then env else update_env updated reqs

(* 결과를 정렬된 ID 순서로 출력 *)
let ensure_all_ids (present : (id * 'a list) list) : (id * 'a list) list =
  let all_ids = [ A; B; C; D; E ] in
  List.map (fun id ->
    match List.assoc_opt id present with
    | Some v -> (id, sort_unique v)
    | None -> (id, [])) all_ids

(* 최종 함수 *)
let shoppingList (reqs : require list) : (id * gift list) list =
  let init = [ (A, []); (B, []); (C, []); (D, []); (E, []) ] in
  let final = update_env init reqs in
  ensure_all_ids final
