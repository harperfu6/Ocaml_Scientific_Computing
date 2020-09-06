#require "owl"
#require "owl-plplot"
open Owl
open Owl_plplot;;

let generate_data () =
  let b = Mat.ones 100 1 in
  let x = Mat.uniform 100 1 in
  let b_x = Mat.concat_horizontal b x in
  let p = Mat.of_array [|1.;2.|] 2 1 in
  let _y = Mat.(b_x *@ p) in
  let e = Mat.uniform 100 1 in
  let e_small = Mat.map (fun n -> n *. 0.3) e in
  let y = Mat.add _y e_small in
  x, p, y

let plot_data x y =
  let h = Plot.create "regdata.png" in
  Plot.scatter ~h ~spec:[MarkerSize 6.] x y;
  Plot.set_xlabel h "x";
  Plot.set_ylabel h "y";
  Plot.output h

let plot_data_reg x y t0 t1 =
  let h = Plot.create "reg.png" in
  Plot.scatter ~h ~spec:[MarkerSize 6.] x y;
  let _x = Mat.linspace 0. 1. 100 in
  let _y = Mat.(_x *$ t1 +$ t0) in
  Plot.plot ~h ~spec:[RGB(0,0,255); LineStyle 1] _x _y;
  Plot.set_xlabel h "x";
  Plot.set_ylabel h "y";
  Plot.output h

let gradient_desc x y =
  let alpha = 0.001 in
  let theta0 = ref 1. in
  let theta1 = ref 1. in

  for i = 0 to 10000 do
    let f x = x *. !theta1 +. !theta0 in
    theta0 := !theta0 -. Mat.(map f x - y |> mean') *. alpha;
    theta1 := !theta1 -. Mat.((map f x - y) * x |> mean') *. alpha;
  done;
  theta0, theta1

let _ =
  let x, p, y = generate_data () in
  (*plot_data x y;;*)
  let t0, t1 = gradient_desc x y in
  print_float !t0;
  print_newline ();
  print_float !t1;
  plot_data_reg x y !t0 !t1

