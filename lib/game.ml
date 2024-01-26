open Brick
open Paddle
open Ball

(* Définition du type game *)
type game_state = {
  bricks : Brick.brick list list;
  paddle : Paddle.paddle;
  ball : Ball.ball;
  score : int;
  vie : int;
}