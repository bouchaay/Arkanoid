open Graphics
open Ball
open Paddle
open Brick
open Screen

module type Collision = sig
  type ball
  type paddle
  type brick
  val ball_paddle_collision : ball -> paddle -> bool
  val ball_brick_collision : ball -> brick -> bool
  val ball_wall_collision : ball -> bool

end

module Collision : Collision = struct
  type ball = Ball.ball
  type paddle = Paddle.paddle
  type brick = Brick.brick

  let ball_paddle_collision ball paddle =
    let ball_r = Ball.get_radius ball in
    let ball_x = Ball.get_x ball in (* x du centre de la balle *)
    let ball_y = Ball.get_y ball in (* y du centre de la balle *)
    let paddle_x = Paddle.get_x paddle in (* x du coin inférieur gauche *)
    let paddle_y = Paddle.get_y paddle in (* y du coin inférieur gauche *)
    let paddle_width = Paddle.get_width paddle in
    let paddle_height = Paddle.get_height paddle in
    let paddle_left = paddle_x in
    let paddle_right = paddle_x +. paddle_width in
    let paddle_top = paddle_y in
    let paddle_bottom = paddle_y +. paddle_height in
    let ball_left = ball_x -. ball_r in
    let ball_right = ball_x +. ball_r in
    let ball_top = ball_y -. ball_r in
    let ball_bottom = ball_y +. ball_r in
    let paddle_left_collision = ball_right >= paddle_left in
    let paddle_right_collision = ball_left <= paddle_right in
    let paddle_top_collision = ball_bottom >= paddle_top in
    let paddle_bottom_collision = ball_top <= paddle_bottom in
    paddle_left_collision && paddle_right_collision && paddle_top_collision && paddle_bottom_collision

  let ball_brick_collision ball brick =
    let ball_r = Ball.get_radius ball in
    let ball_x = (fst (Ball.get_position ball)) in (* x du centre de la balle *)
    let ball_y = (snd (Ball.get_position ball)) in (* y du centre de la balle *)
    let brick_x = (fst (Brick.get_position brick)) in (* x du coin inférieur gauche *)
    let brick_y = (snd (Brick.get_position brick)) in (* y du coin inférieur gauche *)
    let brick_width = fst (Brick.get_size brick) in
    let brick_height = snd (Brick.get_size brick) in
    let brick_left = brick_x in
    let brick_right = brick_x + brick_width in
    let brick_top = brick_y in
    let brick_bottom = brick_y + brick_height in
    let ball_left = ball_x - ball_r in
    let ball_right = ball_x + ball_r in
    let ball_top = ball_y - ball_r in
    let ball_bottom = ball_y + ball_r in
    let brick_left_collision = ball_right >= brick_left in
    let brick_right_collision = ball_left <= brick_right in
    let brick_top_collision = ball_bottom >= brick_top in
    let brick_bottom_collision = ball_top <= brick_bottom in
    brick_left_collision && brick_right_collision && brick_top_collision && brick_bottom_collision

  let ball_wall_collision ball = failwith "not implemented"

  end


