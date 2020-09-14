#require "owl"
open Owl

type t =
  | DF of float * float
  | DR of float * float ref * adjoint

and adjoint = float ref -> (float * t) list -> (float * t) list

let primal = function
  | DF (p, _) -> p
  | DR (p, _, _) -> p

let tangent = function
  | DF (_, t) -> t
  | DR (_, _, _) -> failwith "error: no tangent for DR"

let adjoint = function
  | DF (_, _) -> failwith "error: no adjoint for DF"
  | DR (_, a, _) -> a

let make_forward p a = DF (p, a)

let make_reverse p =
  let a = ref 0. in
  let adj_fun _ t = t in
  DR (p, a, adj_fun)

let rec reverse_push xs =
  match xs with
  | [] -> ()
  | (v, x) :: t ->
      (match x with
      | DR (_, a, adjfun) ->
          a := !a +. v;
          let stack = adjfun a t in
          reverse_push stack
      | _ -> failwith "error: unsupported type")


module type Unary = sig
  val ff : float -> float
  val df : float -> float -> float
  val dr : float -> float ref -> float
end

module type Binary = sig
  val ff : float -> float -> float
  val df : float -> float -> float -> float -> float
  val dr : float -> float -> float ref -> float * float
end


module Sin = struct
  let ff = Owl_maths.sin
  let df p t = (Owl_maths.cos p) *. t
  let dr p a = !a *. (Owl_maths.cos p)
end

module Exp = struct
  let ff = Owl_maths.exp
  let df p t = (Owl_maths.exp p) *. t
  let dr p a = !a *. (Owl_maths.exp p)
end

module Mul = struct
  let ff = Owl_maths.mul
  let df pa pb ta tb = pa *. tb +. ta *. pb
  let dr pa pb a = !a *. pb, !a *. pa
end

module Add = struct
  let ff = Owl_maths.add
  let df _ _ ta tb = ta +. tb
  let dr _ _ a = !a, !a
end

module Div = struct
  let ff = Owl_maths.div
  let df pa pb ta tb = (ta *. pb -. tb *. pa) /. (pb *. pb)
  let dr pa pb a = !a /. pb, !a *. (-.pa) /. (pb *. pb)
end


let unary_op (module U: Unary) = fun x ->
  match x with
    | DF (p, t) ->
        let p' = U.ff p in
        let t' = U.df p t in
        DF (p', t')
    | DR (p, _, _) ->
        let p' = U.ff p in
        let adjfun' a t =
          let r = U.dr p a in
          (r, x) :: t
        in
        DR (p', ref 0., adjfun')

let binary_op (module B: Binary) = fun xa xb ->
  match xa, xb with
  | DF (pa, ta), DF (pb, tb) ->
      let p' = B.ff pa pb in
      let t' = B.df pa pb ta tb in
      DF (p', t')
  | DR (pa, _, _), DR (pb, _, _) ->
      let p' = B.ff pa pb in
      let adjfun' a t =
        let ra, rb = B.dr pa pb a in
        (ra, xa) :: (rb, xb) :: t
      in
      DR (p', ref 0., adjfun')
  | _, _ -> failwith "unsupported op"


let sin_ad = unary_op (module Sin : Unary)
let exp_ad = unary_op (module Exp : Unary)
let mul_ad = binary_op (module Mul : Binary)
let add_ad = binary_op (module Add: Binary)
let div_ad = binary_op (module Div : Binary)

let diff f =
  let f' x =
    let x0, x1 = x in
    match x0, x1 with
    | DF (_, _), DF(_, _) -> f x |> tangent
    | DR (_, _, _), DR (_, _, _) ->
        let r = f x in
        reverse_push [(1., r)];
        !(adjoint x0)
    | _, _ -> failwith "error: unsupported operator"
  in
  f'

(*forward mode*)
let x0 = make_forward 1. 1.;;
let x1 = make_forward 1. 0.;;
let f_forward x =
  let x0, x1 = x in
  let v2 = sin_ad x0 in
  let v3 = mul_ad x0 x1 in
  let v4 = add_ad v2 v3 in
  let v5 = make_forward 1. 0. in
  let v6 = exp_ad v4 in
  let v7 = make_forward 1. 0. in
  let v8 = add_ad v5 v6 in
  let v9 = div_ad v7 v8 in
  v9
;;

diff f_forward (x0, x1);;


(*reverse mode*)
let x0 = make_reverse 1.;;
let x1 = make_reverse 1.;;
let f_reverse x =
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

diff f_reverse (x0, x1);;
