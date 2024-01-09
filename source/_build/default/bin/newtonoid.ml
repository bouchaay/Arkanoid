(* ouvre la bibliotheque de modules definis dans lib/ *)
open Libnewtonoid
open Iterator

(* exemple d'ouvertue d'un tel module de la bibliotheque : *)
open Game

module Init = struct
  let dt = 1000. /. 60. (* 60 Hz *)
end

module Box = struct
  let marge = 10.
  let infx = 10.
  let infy = 10.
  let supx = 790.
  let supy = 590.
end

let graphic_format =
  Format.sprintf
    " %dx%d+50+50"
    (int_of_float ((2. *. Box.marge) +. Box.supx -. Box.infx))
    (int_of_float ((2. *. Box.marge) +. Box.supy -. Box.infy))

type game_state = {
      paddle_position : float * float;
      paddle_size : float * float;
      paddle_color : Graphics.color;
      (* Autres champs de l'état *)
    }

let draw_state (state : game_state) =
  let (x, y) = state.paddle_position in
  let (width, height) = state.paddle_size in
  let color = state.paddle_color in
  Paddle.draw (x, y) (width, height) color


(* extrait le score courant d'un etat : *)
(*let score etat : int = failwith "A DEFINIR"*)

let draw flux_etat =
  let rec loop flux_etat =
    match Flux.uncons flux_etat with
    | None -> ()
    | Some (etat, flux_etat') ->
      Graphics.clear_graph ();
      (* DESSIN ETAT *)
      draw_state etat;
      (* FIN DESSIN ETAT *)
      Graphics.synchronize ();
      Unix.sleepf Init.dt;
      loop flux_etat'
  in
  Graphics.open_graph graphic_format;
  Graphics.auto_synchronize false;
  
  loop flux_etat;
  Graphics.close_graph ()

let initial_state : game_state =
    {
      paddle_position = (100., 100.);  (* Position initiale de la raquette *)
      paddle_size = (50., 10.);        (* Taille initiale de la raquette *)
      paddle_color = Graphics.blue;    (* Couleur initiale de la raquette *)
  
    }
  
  let () = draw (Flux.constant initial_state)
