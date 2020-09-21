#require "owl"
open Owl
open Algodiff.D

module AD = Algodiff.D

let f y =
  let y0 = Mat.get y 0 0 in
  let y1 = Mat.get y 0 1 in
  let y2 = Mat.get y 0 2 in
  let y3 = Mat.get y 0 3 in

  let r = Maths.(sqrt ((sqr y0) + (sqr y1))) in
  let y0' = y2 in
  let y1' = y3 in
  let y2' = Maths.(neg y0 / pow r (F 3.)) in
  let y3' = Maths.(neg y1 / pow r (F 3.)) in

  let y' = Mat.ones 1 4 in
  let y' = Mat.set y' 0 0 y0' in
  let y' = Mat.set y' 0 1 y1' in
  let y' = Mat.set y' 0 2 y2' in
  let y' = Mat.set y' 0 3 y3' in
  y'


let y = Mat.ones 1 4;;
(*Algodiff.D.jacobian*)
let result = jacobian f y;;
Mat.print(result);;

let j = unpack_arr result;;
let eig = Owl_linalg.D.eigvals j;;
