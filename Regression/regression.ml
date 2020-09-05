#require "owl"
#require "owl-plplot"
open Owl
open Owl_plplot;;

let generate_data () =
  let b = Mat.ones 100 1 in
  let x = Mat.uniform 100 1 in
  let b_x = Mat.concat_horizontal b x in
  let p = Mat.of_array [|1.;2.|] 2 1 in
  let y = Mat.(b_x *@ p) in
  let e = Mat.uniform 100 1 in
  let e_small = Mat.map (fun n -> n *. 0.3) e in
  let y_e = Mat.add y e_small in
  x, p, y_e

let plot_data x y =
  let h = Plot.create "regdata.png" in
  Plot.scatter ~h ~spec:[MarkerSize 6.] x y;
  Plot.set_xlabel h "x";
  Plot.set_ylabel h "y";
  Plot.output h

let _ =
  let x, p, y = generate_data () in
  plot_data x y;;
