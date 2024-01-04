open Graphics

(* Type de l'état de la balle *)
type state = Normal | Lost

module type Ball = sig
  type position (* Position du centre de la balle *)
  type radius (* Rayon de la balle *)
  type color (* Couleur de la balle *)
  type speed (* Vitesse de la balle *)
  type ball (* Type de la balle *)

  (* Renvoie un ball *)
  val create : position -> radius -> color -> state -> speed -> ball
  (* Renvoie la position de la balle *)
  val get_position : ball -> position
  (* Renvoie la taille de la balle *)
  val get_radius : ball -> radius
  (* Renvoie la couleur de la balle *)
  val get_color : ball -> color
  (* Renvoie l'état de la balle *)
  val get_state : ball -> state
  (* Renvoie la vitesse de la balle *)
  val get_speed : ball -> speed
  (* Renvoie la balle après le changement de position *)
  val set_position : ball -> position -> ball
  (* Renvoie la balle après le changement de taille *)
  val set_radius : ball -> radius -> ball
  (* Renvoie la balle après le changement de couleur *)
  val set_ball_color : ball -> color -> ball
  (* Renvoie la balle après le changement d'état *)
  val set_state : ball -> state -> ball
  (* Renvoie la balle après le changement de vitesse *)
  val set_speed : ball -> speed -> ball
  (* Dessine la balle *)
  val draw : ball -> unit
end 


module Ball : Ball = struct
  type position = float * float
  type radius = float
  type color = Graphics.color
  type speed = float * float
  type ball = position * radius * color * state * speed
  
  let create position radius color state speed = (position, radius, color, state, speed)
  
  let get_position (position,_,_,_,_) = position 
  let get_radius  (_,radius,_,_,_) = radius
  let get_color (_,_,color,_,_) = color
  let get_state (_,_,_,state,_) = state
  let get_speed  (_,_,_,_,speed) = speed
  
  let set_position (_, radius, color, state, speed) newPosition = (newPosition, radius, color, state, speed)
  let set_radius (position, _, color, state, speed) newradius = (position, newradius, color, state, speed)
  let set_ball_color (position, radius, _, state, speed) newColor = (position, radius, newColor, state, speed)
  let set_state (position, radius, color, _, speed) newState = (position, radius, color, newState, speed)
  let set_speed (position, radius, color, state, _) newSpeed = (position, radius, color, state, newSpeed)
  
  
  let draw ball =(
    let (x, y) = get_position ball in
    let r = get_radius ball in
    let color = get_color ball in
    set_color color;
    fill_circle (int_of_float x) (int_of_float y) (int_of_float r) )
end


