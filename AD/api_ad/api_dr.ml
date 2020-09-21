#require "owl"
open Owl
open Algodiff.D

module AD = Algodiff.D


let f x =
  let x0 = AD.Mat.get x 0 0 in
  let x1 = AD.Mat.get x 0 1 in
  AD.Maths.((F 1.) / (F 1. + exp (sin x0 + x0 * x1)))

let x = Mat.ones 1 2;;
let x' = make_reverse x (tag ());;
let y = f x';;
let _ = reverse_prop (F 1.) y;;
let y' = adjval x';;
Mat.print (y');;
