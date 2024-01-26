open Graphics


module type Brick =
sig
  type t
  type power
  type color = Graphics.color
  type brick

  (* Créer une brique à partir d'une position, d'une taille, d'une puissance et d'une couleur *)
  val create : (t * t) -> (t * t) -> int -> color -> brick
  (* Renvoie vrai si la position est dans la brique *)
  val is_position_inside_brick : (t * t) -> brick -> bool
  (* Renvoie la puissance de la brique *)
  val get_power : brick -> power
  (* Renvoie la position de la brique *)
  val get_position : brick -> (t * t)
  (* Renvoie la taille de la brique *)
  val get_size : brick -> (t * t)
  (* Renvoie vrai si la brique a une puissance *)
  val brickWithPower : brick -> bool
  (* Dessine une brique *)
  val draw_brick : brick -> unit
  (* Génère une ligne de briques *)
  val generate_brick_line : (t * t) -> t -> color -> brick list
  (* Génère des lignes de briques *)
  val generate_brick_lines : (t * t) -> (t * t)  -> color -> brick list list
  (* Dessine une ligne de briques *)
  val draw_brick_line : brick list -> unit
  (* Dessine des lignes de briques *)
  val draw_brick_lines : brick list list -> unit
  (* Transforme une brique en vide *)
  val brickTocheese : (t * t) -> brick -> brick
  (* Transforme une ligne de briques en ligne vide *)
  val update_brick_line : (t * t) -> brick list -> brick list
  (* Transforme des lignes de briques en lignes vides *)
  val update_brick_lines : (t * t) -> brick list list -> brick list list
  (* Renvoie le nombre de briques *)
  val nbBricks : brick list list -> int
  (* Renvoie le nombre de briques sans puissance *)
  val nbBricksWithoutPower : brick list list -> int
  (* Transforme une brique en vide *)
  val update_brick : (t * t) -> brick list  -> brick list 
  (* Transforme des lignes de briques en lignes vides *)
  val update_bricks : (t * t) -> brick list list -> brick list list
end

module Brick : Brick with type t = float = struct
  type t = float
  type power = int
  type color = Graphics.color
  type brick = (t * t) * (t * t) * power * color

  let create position size power color = (position, size, power, color)

  let get_power (_, _, power, _) = power

  let get_position ((x, y), _, _, _) = (x, y)

  let get_size (_, (w, h), _, _) = (w, h)

  let brickWithPower (_, _, power, _) = power <> 0

  let is_position_inside_brick (xb, yb) ((brick_x, brick_y), (brick_width, brick_height), _, _) =
    xb >= brick_x && xb <= brick_x +. brick_width &&
    yb <= brick_y && yb >= brick_y -. brick_height

  let generate_brick_line position_depart xmax color =
    let space = 35. in
    let width = 25.  in
    let height = 10. in
    
    let rec aux position  =
      if  (fst position) +. width >= xmax -. 40. then []
      else
        let brick = create position (width, height) (Random.int 3 + 1) color in
        brick :: aux (fst position +.  space, snd position) 
      in
    aux position_depart 

  let generate_brick_lines position_depart posMax color =
    let space = 30. in
    let xmax, ymax = posMax in
    
  let rec aux position  =
    if  (snd( position) >= ymax) then []
    else
      let brick_line = generate_brick_line position xmax  color in
      brick_line :: aux (fst position, snd position +. space) 
    in
    aux position_depart 

  let draw_brick ((x, y), (width, height), _, color) =
    set_color color;
    Graphics.fill_rect (int_of_float x) (int_of_float y) (int_of_float width) (int_of_float height)

  let draw_brick_line brick_line =
    List.iter draw_brick brick_line
    
  let draw_brick_lines brick_lines =
    List.iter draw_brick_line brick_lines

  let brickTocheese (x,y) (pos,size,power,color) =
    if is_position_inside_brick (x,y) (pos,size,power,color) then
      (pos,size,0,Graphics.white)
    else
      (pos,size,power,color)
    
  let update_brick_line (x,y) brick_line =
    List.map (brickTocheese (x,y)) brick_line
    
  let update_brick_lines (x,y) brick_lines =
    List.map (update_brick_line (x,y)) brick_lines

  let nbBricks brick_lines =
    List.fold_left (fun acc brick_line -> acc + List.length brick_line) 0 brick_lines

  let nbBricksWithoutPower brick_lines =
    List.fold_left (fun acc brick_line -> acc + List.length (List.filter (fun (_,_,power,_) -> power = 0) brick_line)) 0 brick_lines

  let rec update_brick (x,y) brick_line =
    match brick_line with
      | [] -> []
      | b :: q -> 
        ( if is_position_inside_brick (x,y) b then
          update_brick (x,y) q
        else
          b :: update_brick (x,y) q)

  let update_bricks (x,y) brick_lines =
    List.map (update_brick (x,y)) brick_lines
end
