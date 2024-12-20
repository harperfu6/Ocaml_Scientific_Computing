#require "owl"
open Owl
open Algodiff.S

(*arr data*)
(*let x, _, y = Dataset.load_mnist_train_data_arr ()*)
(*let x_shape, y_shape = Dense.Ndarray.S.shape x, Dense.Ndarray.S.shape y*)
(*val x_shape : int array = [|60000; 28; 28; 1|]*)
(*val y_shape : int array = [|60000; 10|]*)


(*Feed Forward Network*)
(*Layers*)
type layer = {
  mutable w : t;
  mutable b : t;
  mutable a : t -> t;
}

type network = {layers : layer array}

let run_layer x l = Maths.((x *@ l.w) + l.b) |> l.a
let run_network x nn = Array.fold_left run_layer x nn.layers

(*Initialisation*)
let l0 = {
  w = Maths.(Mat.uniform 784 40 * F 0.15 - F 0.075);
  b = Mat.zeros 1 40;
  a = Maths.tanh;
}

let l1 = {
  w = Maths.(Mat.uniform 40 10 * F 0.15 - F 0.075);
  b = Mat.zeros 1 10;
  a = Maths.softmax ~axis:1;
}

let nn = {layers = [|l0;l1|]}

(*Training*)
(*cross_entropy y x: -ylog(x)*)
(*Forward*)
let loss_fun nn x y =
  let t = tag () in
  Array.iter (fun l ->
    l.w <- make_reverse l.w t;
    l.b <- make_reverse l.b t;
  ) nn.layers;
  Maths.(cross_entropy y (run_network x nn) / (F (Mat.row_num y |> float_of_int)))

(*Backward*)
let backprop nn eta x y =
  let loss = loss_fun nn x y in
  reverse_prop (F 1.) loss;
  Array.iter (fun l ->
    l.w <- Maths.((primal l.w) - (eta * (adjval l.w))) |> primal;
    l.b <- Maths.((primal l.b) - (eta * (adjval l.b))) |> primal;
  ) nn.layers;
  loss |> unpack_flt


(*Test*)
let test nn x y =
  Dense.Matrix.S.iter2_rows (fun u v ->
    let p = run_network (Arr u) nn |> unpack_arr in
    Dense.Matrix.Generic.print p;
    Printf.printf "prediction: %i\n" (let _, i = Dense.Matrix.Generic.max_i p in i.(1))
  ) (unpack_arr x) (unpack_arr y)


(*main*)
let main () =
  let x, _, y = Dataset.load_mnist_train_data () in
  for i = 1 to 999 do
    let x', y' = Dataset.draw_samples x y 100 in
    backprop nn (F 0.01) (Arr x') (Arr y')
    |> Owl_log.info "#%03i : loss = %g" i
  done;
  let x, y, _ = Dataset.load_mnist_test_data () in
  let x, y = Dataset.draw_samples x y 10 in
  test nn (Arr x) (Arr y)

