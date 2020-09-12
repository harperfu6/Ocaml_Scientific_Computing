#require "owl"
open Owl

type dr = {
  mutable p: float;
  mutable a: float ref;
  mutable adj_fun : float ref -> (float * dr) list -> (float * dr) list
}

let primal dr = dr.p
let adjoint dr = dr.a
let adj_fun dr = dr.adj_fun

(*バックプロパゲーションでは連鎖律の過程で後ろの層の偏微分が必要になる*)
  (*caがadjoint value: yに対する微分値*)
  (*よってcaは後ろから伝搬されてくるもので具体的な値が入らないとわからないの
    参照値のプレースホルダーにする*)
  (*オリジナルのdrとこの関数でプロパゲートされる値の組みをtにスタックする*)
let sin_ad dr =
  let p = primal dr in
  let p' = Owl_maths.sin p in
  let adjfun' ca t =
    let r = !ca *. (Owl_maths.cos p) in
    (r, dr) :: t
  in
  {p=p'; a=ref 0.; adj_fun=adjfun'}

(*mulは２つの入力へと伝搬する*)
let mul_ad dr1 dr2 =
  let p1 = primal dr1 in
  let p2 = primal dr2 in

  let p' = Owl_maths.mul p1 p2 in
  (*mulは入力に対し逆転する*)
  let adjfun' ca t =
    let r1 = !ca *. p2 in
    let r2 = !ca *. p1 in
    (r1, dr1) :: (r2, dr2) :: t
  in
  {p=p'; a = ref 0.; adj_fun=adjfun'}

let exp_ad dr =
  let p = primal dr in
  let p' = Owl_maths.exp p in
  let adjfun' ca t =
    let r = !ca *. (Owl_maths.exp p) in
    (r, dr) :: t
    in
    {p=p'; a=ref 0.; adj_fun=adjfun'}

let add_ad dr1 dr2 =
  let p1 = primal dr1 in
  let p2 = primal dr2 in
  let p' = Owl_maths.add p1 p2 in
  let adjfun' ca t =
    let r1 = !ca in
    let r2 = !ca in
    (r1, dr1) :: (r2, dr2) :: t
    in
    {p=p'; a=ref 0.; adj_fun=adjfun'}

let div_ad dr1 dr2 =
  let p1 = primal dr1 in
  let p2 = primal dr2 in

  let p' = Owl_maths.div p1 p2 in
  let adjfun' ca t =
    let r1 = !ca /. p2 in
    let r2 = !ca *. (-.p1) /. (p2 *. p2) in
    (r1, dr1) :: (r2, dr2) :: t
    in
    {p=p'; a=ref 0.; adj_fun=adjfun'}


let make_reverse v =
  let a = ref 0. in
  let adj_fun _a t = t in
  {p=v; a; adj_fun}
(*
let x = make_reverse 1.;;
val x : dr = {p = 1.; a = {contents = 0.}; adj_fun = }
let y = make_reverse 2.;;
val y : dr = {p = 2.; a = {contents = 0.}; adj_fun = }
let v = mul_ad (sin_ad x) y;;
val v : dr = {p = 1.68294196961579301; a = {contents = 0.}; adj_fun = }
*)

let rec reverse_push xs =
  match xs with
  | [] -> ()
  | (r, dr) :: t -> (*stackの先頭から取り出す*)
      let aa = adjoint dr in (*計算済みのadjoint valueの取得*)
      let adjfun = adj_fun dr in
      aa := !aa +. r;
      let stack = adjfun aa t in
      reverse_push stack


let diff f =
  let f' x =
    (*forward pass*)
    let r = f x in
    (*backward pass*)
    reverse_push [(1., r)];
    (*get result values*)
    let x0, x1 = x in
    primal x0, !(adjoint x0), primal x1, !(adjoint x1)
  in
  f'

let x0 = make_reverse 1.;;
let x1 = make_reverse 1.;;

let f x =
  let x0, x1 = x in
  let v2 = sin_ad x0 in
  let v3 = mul_ad x0 x1 in
  let v4 = add_ad v2 v3 in
  let v5 = make_reverse 1. in
  let v6 = exp_ad v4 in
  let v7 = make_reverse 1. in
  let v8 = add_ad v5 v6 in
  let v9 = div_ad v7 v8 in
  v9
;;

let pri_x0, adj_x0, pri_x1, adj_x1 = diff f (x0, x1);;
