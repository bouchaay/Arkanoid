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
      let bricks, padle, ball = state.bricks in
      let get_click_position () =
      let event = wait_next_event [Button_down] in
        (event.button,(float_of_int event.mouse_x, float_of_int event.mouse_y)) in
        
      let _, posM = get_click_position () in
      let new_brick = Brick.update_brick_lines posM bricks in
      Brick.draw_brick_lines new_brick;
      Paddle.draw padle;
      Ball.draw ball
    


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

  let () =
    let posd = (50., 300.) in
    let posM = (Box.supx , Box.supy ) in
   
    
    let coleR = Graphics.blue in
    let coloRpadle = Graphics.black in
    let coloRball = Graphics.red in
    let paddleInit = Paddle.create (300.,60.) (90.,20.) coloRpadle in
    let ballInit = Ball.create (300.,150.) 10. coloRball Normal 4. in
    
    let initial_state : game_state = { bricks = (Brick.generate_brick_lines posd posM coleR, paddleInit, ballInit) } in
  
    let flux_etat = Flux.constant initial_state in
    Graphics.open_graph graphic_format;
    Graphics.auto_synchronize false;
    loop flux_etat;
    Graphics.close_graph ()
  
