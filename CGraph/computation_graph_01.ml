#require "owl"
open Owl

module G = Owl_computation_cpu_engine.Make (Owl_algodiff_primal_ops.D)
include Owl_algodiff_generic.Make (G)


let f x y = Maths.((x * sin (x + x) + ((pack_flt 1.) * sqrt x) / (pack_flt 7.)) * (relu y) |> sum')

let dotdir = "dot/"
let pdfdir = "pdf/"


let output s dotfile pdffile =
  let dotfile = dotdir ^ dotfile in
  let pdffile = pdfdir ^ pdffile in
  Owl_io.write_file dotfile s;
  Sys.command ("dot -Tpdf " ^ dotfile ^ " -o " ^ pdffile) |> ignore

let visualise_01 () =
  let x = G.var_elt "x" |> pack_elt in
  let y = G.var_elt "y" |> pack_elt in
  let z = f x y in
  let inputs = [| unpack_elt x |> G.elt_to_node; unpack_elt y |> G.elt_to_node |] in
  let outputs = [| unpack_elt z |> G.elt_to_node |] in
  let graph = G.make_graph inputs outputs "graph" in
  let s = G.graph_to_dot graph in
  output s "cgraph_01.dot" "cgraph_01.pdf"


let visualise_02 () =
  let x = G.var_elt "x" |> pack_elt in
  let y = G.var_elt "y" |> pack_elt in
  let z = (grad (f x)) y in
  let inputs = [| unpack_elt x |> G.elt_to_node; unpack_elt y |> G.elt_to_node |] in
  let outputs = [| unpack_elt z |> G.elt_to_node |] in
  let s = G.make_graph inputs outputs "graph" |> G.graph_to_dot in
  output s "cgraph_02.dot" "cgraph_02.pdf"

let visualise_03 () =
  let t = tag () in
  let x = make_reverse (G.var_arr "x" ~shape:[|3;4|] |> pack_arr) t in
  let y = make_reverse (G.var_arr "y" ~shape:[|1;4|] |> pack_arr) t in
  let z = f x y in
  let i0 = [| unpack_arr x |> G.arr_to_node; unpack_arr y |> G.arr_to_node |] in
  let o0 = [| primal z |> unpack_elt |> G.elt_to_node |] in
  let s0 = G.make_graph i0 o0 "graph" |> G.graph_to_dot in
  output s0 "cgraph_03_forward.dot" "cgraph_03_forward.pdf";

  reverse_prop (pack_flt 1.) z;
  let x' = adjval x |> unpack_arr |> G.arr_to_node in
  let y' = adjval y |> unpack_arr |> G.arr_to_node in
  let i1 = [| unpack_arr x |> G.arr_to_node |] in
  let s1 = G.make_graph i1 [| x' |] "graph" |> G.graph_to_dot in
  let i2 = [| unpack_arr y |> G.arr_to_node |] in
  let s2 = G.make_graph i2 [| y' |] "graph" |> G.graph_to_dot in
  let s3 = G.make_graph i0 [| x'; y' |] "graph" |> G.graph_to_dot in

  output s1 "cgraph_03_backward_x.dot" "cgraph_03_backward_x.pdf";
  output s2 "cgraph_03_backward_y.dot" "cgraph_03_backward_y.pdf";
  output s3 "cgraph_03_backward_xy.dot" "cgraph_03_backward_xy.pdf"


let _ =
  visualise_01 ();
  visualise_02 ();
  visualise_03 ()
