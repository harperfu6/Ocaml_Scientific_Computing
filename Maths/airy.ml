#require "owl"
#require "owl-plplot"
open Owl;;
open Owl_plplot;;

let x = Mat.linspace (-15.) 5. 200

let y0 = Mat.map (fun x ->
  let ai, _, _, _ = Maths.airy x in ai
) x


let y1 = Mat.map (fun x ->
  let _, _, bi, _ = Maths.airy x in bi
) x

let _ =
  let h = Plot.create "special_airy.png" in
  Plot.(plot ~h ~spec:[ RGB (66, 133, 244); LineStyle 1; LineWidth 2. ] x y0);
  Plot.(plot ~h ~spec:[ RGB (219, 68,  55); LineStyle 2; LineWidth 2. ] x y1);
  Plot.(set_yrange h (-0.5) 1.);
  Plot.(legend_on h ~position:SouthEast [|"Ai"; "Bi"|]);
  Plot.output h
