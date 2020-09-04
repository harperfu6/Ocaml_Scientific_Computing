#require "owl";;
#require "owl-plplot";;
open Owl
open Owl_plplot

(* N(0,1) sample *)
let y = Mat.gaussian 5000 2 ;;

(* Initiates the plot *)
let h = Plot.create "bivariate.png" ;;

(* Side effects *)
Plot.set_background_color h 255 255 255;

(* More side effects *)
Plot.set_title h "Bivariate model";

(* Simple scatter *)
Plot.scatter ~h (Mat.col y 0) (Mat.col y 1);
Plot.output h;;
