open Graphics
open Brick
open Paddle



type state = Normal | Perdu

module type Ball = sig
  type position
  type size (* Taille du ball *)
  type color (* Couleur du ball *)
  type speed (* Vitesse du ball *)
  type ball (* Type du ball *)
  (* Renvoie un ball *)
  val create : position -> size -> color -> state -> speed -> ball
  (* Renvoie la position de la balle *)
  val get_position : ball -> position
  (* Renvoie la taille de la balle *)
  val get_size : ball -> size
  (* Renvoie la couleur de la balle *)
  val get_color : ball -> color
  (* Renvoie l'état de la balle *)
  val get_state : ball -> state
  (* Renvoie la vitesse de la balle *)
  val get_speed : ball -> speed
  (* Renvoie la balle après le changement de position *)
  val set_position : ball -> position -> ball
  (* Renvoie la balle après le changement de taille *)
  val set_size : ball -> size -> ball
  (* Renvoie la balle après le changement de couleur *)
  val set_color : ball -> color -> ball
  (* Renvoie la balle après le changement d'état *)
  val set_state : ball -> state -> ball
  (* Renvoie la balle après le changement de vitesse *)
  val set_speed : ball -> speed -> ball
  (* Dessine la balle *)
  val draw : t -> unit
  (* Renvoie true si la balle touche brick *)
  val brick_touched : ball -> brick -> bool
  (* Renvoie true si la balle touche paddle *)
  val paddle_touched : ball -> paddle -> bool
  (* Renvoie la liste des briques touchées par la balle *)
  val list_brick_touched : ball -> brick list -> brick list
  (* Renvoie le nombre des briques touchées par la balle pour l'ajouter au score *)
  val nombre_brick_touched : ball -> brick list -> int
end 


module Ball : Ball = struct
  type position = float * float
  type size = float * float
  type color = Graphics.color
  type speed = float * float
  type state = Normal | Perdu
  type ball = position * size * color * state * speed
  
  let create position size color state speed = (position, size, color, state, speed)
  
  let get_position (position,_,_,_,_) = position 
  let get_size  (_,size,_,_,_) = size
  let get_color (_,_,color,_,_) = color
  let get_state (_,_,_,state,_) = state
  let get_speed  (_,_,_,_,speed) = speed
  
  let set_position (position, size, color, state, speed) newPosition = (newPosition, size, color, state, speed)
  let set_size (position, size, color, state, speed) newSize = (position, newSize, color, state, speed)
  let set_color (position, size, color, state, speed) newColor = (position, size, newColor, state, speed)
  let set_state (position, size, color, state, speed) newState = (position, size, color, newState, speed)
  let set_speed (position, size, color, state, speed) newSpeed = (position, size, color, state, newSpeed)
  
  
  let draw ball =(
    let (x, y) = get_position ball in
    let (w, h) = get_size ball in
    let color = get_color ball in
    set_color color;
    fill_circle x y w h)
  
  
  let brick_touched ball brick =(
    let (x, y) = get_position ball in
    let (w, h) = get_size ball in
    let (x_brick, y_brick) = Brick.get_position brick in
    let (w_brick, h_brick) = Brick.get_size brick in
    let (x_brick2, y_brick2) = (x_brick + w_brick, y_brick + h_brick) in
    let (x_ball2, y_ball2) = (x + w, y + h) in
    if (x_brick <= x && x <= x_brick2 && y_brick <= y && y <= y_brick2) then true
    else if (x_brick <= x_ball2 && x_ball2 <= x_brick2 && y_brick <= y && y <= y_brick2) then true
    else if (x_brick <= x && x <= x_brick2 && y_brick <= y_ball2 && y_ball2 <= y_brick2) then true
    else if (x_brick <= x_ball2 && x_ball2 <= x_brick2 && y_brick <= y_ball2 && y_ball2 <= y_brick2) then true
    else false )
  

  let paddle_touched ball paddle =(
    let (x, y) = get_position ball in
    let (w, h) = get_size ball in
    let (x_paddle, y_paddle) = Paddle.get_position paddle in
    let (w_paddle, h_paddle) = Paddle.get_size paddle in
    let (x_paddle2, y_paddle2) = (x_paddle + w_paddle, y_paddle + h_paddle) in
    let (x_ball2, y_ball2) = (x + w, y + h) in
    if (x_paddle <= x && x <= x_paddle2 && y_paddle <= y && y <= y_paddle2) then true
    else if (x_paddle <= x_ball2 && x_ball2 <= x_paddle2 && y_paddle <= y && y <= y_paddle2) then true
    else if (x_paddle <= x && x <= x_paddle2 && y_paddle <= y_ball2 && y_ball2 <= y_paddle2) then true
    else if (x_paddle <= x_ball2 && x_ball2 <= x_paddle2 && y_paddle <= y_ball2 && y_ball2 <= y_paddle2) then true
    else false )

  let rec list_brick_touched ball list_brick =(
    match list_brick with
    | [] -> []
    | h::t -> if (brick_touched ball h) then h::(list_brick_touched ball t) else list_brick_touched ball t)


  let nombre_brick_touched ball list_brick =(
    let list_brick_touched = list_brick_touched ball list_brick in
    List.length list_brick_touched)

end


