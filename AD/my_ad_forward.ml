#require "owl"
open Owl

type df = {
  mutable p: float;
  mutable t: float
}

let primal df = df.p
let tangent df = df.t

(*each ad*)
let sin_ad x =
  (*get data from df*)
  let p = primal x in
  let t = tangent x in
  (*ad value*)
  let p' = Owl_maths.sin p in
  let t' = (Owl_maths.cos p) *. t in
  {p=p'; t=t'}

let exp_ad x =
  let p = primal x in
  let t = tangent x in
  let p' = Owl_maths.exp p in
  let t' = p' *. t in
  {p=p'; t=t'}

let mul_ad a b =
  let pa = primal a in
  let ta = tangent a in
  let pb = primal b in
  let tb = tangent b in
  let p' = pa *. pb in
  let t' = pa *. tb +. ta *. pb in
  {p=p'; t=t'}

let add_ad a b =
  let pa = primal a in
  let ta = tangent a in
  let pb = primal b in
  let tb = tangent b in
  let p' = pa +. pb in
  let t' = ta +. tb in
  {p=p'; t=t'}

let div_ad a b =
  let pa = primal a in
  let ta = tangent a in
  let pb = primal b in
  let tb = tangent b in
  let p' = pa /. pb in
  let t' = (ta *. pb -. tb *. pa) /. (pb *. pb) in
  {p=p'; t=t'}

(*define diff based on above ad function*)
let diff f =
  let f' x y =
    let r = f x y in
    primal r, tangent r
  in
  f'


(*differantiation example*)
let x0 = {p=1.; t=1.}
let x1 = {p=1.; t=0.}

let f x0 x1 =
  let v2 = sin_ad x0 in
  let v3 = mul_ad x0 x1 in
  let v4 = add_ad v2 v3 in
  let v5 = {p=1.; t=0.} in
  let v6 = exp_ad v4 in
  let v7 = {p=1.; t=0.} in
  let v8 = add_ad v5 v6 in
  let v9 = div_ad v7 v8 in
  v9
;;

(*pri: f(x0), tan: f(x0)'*)
let pri, tan = diff f x0 x1;;
