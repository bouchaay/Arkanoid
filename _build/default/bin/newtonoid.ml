(* ouvre la bibliotheque de modules definis dans lib/ *)
open Libnewtonoid
open Iterator
open Game
open Ball
open Paddle
open Brick

(* Définition de la fenêtre graphique *)
module Box = struct
  let marge = 10.
  let infx = 10.
  let infy = 10.
  let supx = 790.
  let supy = 590.
end

(* Initialisatioon du game*)
module Init = struct
  let dt = 1000. /. 60. (* 60 Hz *)
  let posd = (50., 330.) 
  let posM = (Box.supx , Box.supy -. 60. )

  
  let coleR = Graphics.yellow
  let coloRpadle = Graphics.black 
  let coloRball = Graphics.red 
  let paddleInit = Paddle.create (300.,60.) (90.,20.) coloRpadle 
  let ballInit = Ball.create (300.,150.) 6. coloRball Normal (0.5,0.6) 
  let bricksInt = Brick.generate_brick_lines posd posM coleR
  
  let initial_state : game_state = { bricks = bricksInt;
  paddle = paddleInit;
  ball = ballInit;
  score = 0;
  vie = 3 } 
end

let graphic_format =
  Format.sprintf
    " %dx%d+50+50"
    (int_of_float ((2. *. Box.marge) +. Box.supx -. Box.infx))
    (int_of_float ((2. *. Box.marge) +. Box.supy -. Box.infy))

let update_game (game : game_state)  =
    let ball, paddle, bricks, vies =  game.ball, game.paddle, game.bricks, game.vie in
    let (xb,yb) = Ball.get_position ball in
    let new_bricks = Brick.update_brick_lines (xb,yb) bricks in
    let new_ball = Ball.updateBall ball Init.dt in      
    let (x,_) = Graphics.mouse_pos () in
    let new_paddle = Paddle.updatePadle paddle (float_of_int(x),true) in
    let new_score =  Brick.nbBricksWithoutPower new_bricks in
          {  ball = new_ball ; paddle = new_paddle; bricks = new_bricks; score = new_score; vie = vies}

  let draw_state (state : Game.game_state) =
    let bricks, paddle, ball, score, vies = state.bricks, state.paddle, state.ball, state.score, state.vie in
    Graphics.moveto 10 10;  (* Déplace le pointeur en bas à gauche *)
    Graphics.draw_string ("Score: " ^ (string_of_int score));
    Graphics.moveto 650 20;
    Graphics.draw_string ("Vies : " ^ (string_of_int vies));
    Brick.draw_brick_lines bricks;
    Paddle.draw paddle;
    Ball.draw ball

let rec loop game flux_etat =
  Graphics.clear_graph ();
  draw_state game;
  Graphics.synchronize ();
  Unix.sleepf 0.04;
  let game' = update_game game in
  let ball = game'.ball in
  let paddle = game'.paddle in
  let pos, size = Paddle.get_position paddle, Paddle.get_size paddle in
  let height = snd size in
  let ypos = snd pos in
  
  match Flux.uncons flux_etat with
  | None -> ()
  | Some (_, flux_etat') ->
    let paddle = game'.paddle in
    let pos, size, brickLines = Paddle.get_position paddle, Paddle.get_size paddle, game'.bricks in
    let newGame = {game' with ball = Ball.reflectGeneral ball brickLines pos size } in
    (if Ball.isFinished ball ypos height then
      let vieRestante = game'.vie - 1 in
      if game'.vie > 1 then
        let newGameVie = {game' with vie = vieRestante; ball = Ball.create (300.,150.) 6. Graphics.red Normal (0.5,0.6)} in
        loop newGameVie flux_etat'
      else
        begin
          Graphics.clear_graph ();
          let newGameEnd = {newGame with vie = vieRestante} in
          draw_state newGameEnd;
          Graphics.moveto 350 300;
          Graphics.draw_string "Game Over";
          Graphics.moveto 650 20;
          Graphics.synchronize ();
          Unix.sleepf 3.;
          Graphics.close_graph ()
      end
    else
      loop newGame flux_etat')

let draw game flux_etat =
  Graphics.open_graph graphic_format;
  Graphics.auto_synchronize true;
  loop game flux_etat;
  Graphics.close_graph ()

let () = 
  let initial_state = Init.initial_state in
  let flux_etat = Flux.unfold (fun state -> Some (state, update_game state)) initial_state in
  draw initial_state flux_etat;
