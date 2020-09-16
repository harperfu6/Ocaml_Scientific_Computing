#require "owl"
open Owl

module AD = Algodiff.D

let f x =
  let x0 = AD.Mat.get x 0 0 in
  let x1 = AD.Mat.get x 0 1 in
  AD.Maths.((F 1.) / (F 1. + exp (sin x0 + x0 * x1))) (*F wrap float number for AD*)

let input = Arr (Dense.Matrix.D.ones 1 2)
let result = f input |> unpack_flt (*unpack_flt unwrap AD container*)
