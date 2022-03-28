

(* -------------------------- *)
(* -------------------------- *)
(*    fonctions utilitaires   *)
(* -------------------------- *)
(* -------------------------- *)


let mywait(x : float) : unit =
  let y : float ref = ref (Sys.time()) in
  while (Sys.time() -. !y) < x
  do ()
  done
;;


let valid_matrix_point(p, param : t_point * t_param) : bool =
  (
    (p.x >= 0) && (p.y >= 0) && (p.x <= param.mat_szx - 1) && (p.y <= param.mat_szy- 1)
  )
;;


let rec is_free_move(p, shape, mymat, param : t_point * t_point list * t_color matrix * t_param) : bool =
  if valid_matrix_point(p, param)
  then
    (
      if isempty(shape)
      then true
      else
        let sx : int = (fst(shape)).x + p.x in
        let sy : int = (fst(shape)).y + p.y in
        if (mymat.(p.x).(p.y) <> white) || (mymat.(sx).(sy) <> white)
        then false
        else is_free_move(p, rem_fst(shape), mymat, param)
    )
  else false
;;

  
(* --------------------------------- *)
(* --------------------------------- *)
(*   Types et fonctions graphique    *)
(* --------------------------------- *)
(* --------------------------------- *)

type t_point = {x : int ; y : int} ;;


let draw_absolute_pt(p, base_draw, dilat, col : t_point * t_point * int * t_color) : unit =
  let (p_dilat_x, p_dilat_y) : int * int = ((p.x * dilat) + base_draw.x, (p.y * dilat) + base_draw.y) in
  (
  set_color(col) ;
  draw_rect(p_dilat_x, p_dilat_y, dilat - 1, dilat - 1) ;
  )
;;


let fill_absolute_pt(p, base_draw, dilat, col : t_point * t_point * int * t_color) : unit =
  let (p_dilat_x, p_dilat_y) : int * int = ((p.x * dilat) + base_draw.x, (p.y * dilat) + base_draw.y) in
  (
  set_color(col) ;
  fill_rect(p_dilat_x, p_dilat_y, dilat - 1, dilat - 1) ;
  )
;;

let drawfill_absolute_pt(p,  base_draw, dilat, col : t_point * t_point * int * t_color) : unit =
  (
  draw_absolute_pt(p, base_draw, dilat, black) ;
  fill_absolute_pt(p, base_draw, dilat, col)
  )
;;


let draw_relative_pt(p, base_point, base_draw, dilat, col : t_point * t_point * t_point * int * t_color) : unit =
  let (p_newx, p_newy) : int * int = (p.x + base_point.x, p.y + base_point.y) in
  draw_absolute_pt({x = p_newx ; y = p_newy}, base_draw, dilat, col)
;;


let fill_relative_pt(p, base_point, base_draw, dilat, col : t_point * t_point * t_point * int * t_color) : unit =
  let (p_newx, p_newy) : int * int = (p.x + base_point.x, p.y + base_point.y) in
  fill_absolute_pt({x = p_newx ; y = p_newy}, base_draw, dilat, col) 
;;


let drawfill_relative_pt(p, base_point, base_draw, dilat, col : t_point * t_point * t_point * int * t_color) : unit =
  let (p_newx, p_newy) : int * int = (p.x + base_point.x, p.y + base_point.y) in
  drawfill_absolute_pt({x = p_newx ; y = p_newy}, base_draw, dilat , col)
;;


let rec draw_pt_list(l, base_pt, base_draw, dilat, col : t_point list * t_point * t_point * int * t_color) : unit =
  if isempty(l)
  then ()
  else
    (
    draw_relative_pt(fst(l), base_pt, base_draw, dilat, col) ;
    draw_pt_list(rem_fst(l), base_pt, base_draw, dilat, col)
    )
;;


let rec fill_pt_list(l, base_pt, base_draw, dilat, col : t_point list * t_point * t_point * int * t_color) : unit =
  if isempty(l)
  then ()
  else
    (
    fill_relative_pt(fst(l), base_pt, base_draw, dilat, col) ;
    fill_pt_list(rem_fst(l), base_pt, base_draw, dilat, col)
    )
;;

let drawfill_pt_list(l, base_pt, base_draw, dilat, col : t_point list * t_point * t_point * int * t_color) : unit =
  (
  fill_pt_list(l, base_pt, base_draw, dilat, col) ;
  draw_pt_list(l, base_pt, base_draw, dilat, black) ;
  )
;;

                           
let draw_frame(base_draw, size_x, size_y, dilat : t_point * int * int * int) : unit =
  (
    for i =  -1 downto  -2
    do
      for a = -2 to size_y - 1
      do
        drawfill_absolute_pt({x = i ; y = a}, {x = base_draw.x ; y = base_draw.y}, dilat, black)
      done
    done
  ) ;
  (
    for a = -1 downto -2
    do
      for i = -2 to size_x - 1
      do
        drawfill_absolute_pt({x = i ; y = a}, {x = base_draw.x ; y = base_draw.y}, dilat, black)
      done
    done
  ) ;
  (
    for i = size_x + 1 downto size_x
    do
      for a = -2 to size_y - 1
      do
        drawfill_absolute_pt({x = i ; y = a}, {x = base_draw.x ; y = base_draw.y}, dilat, black)
      done
    done
  )
;;


let move_left(pl : t_play) : unit =
  let p : t_point = {x = !(pl.cur_shape.base).x - 1 ; y = !(pl.cur_shape.base).y} in
  let shape : t_point list = pl.par.shapes.value.(!(pl.cur_shape.shape)).shape in
  let mymat : t_color matrix = pl.mat in
  let param : t_param = pl.par in
  (
    if not(is_free_move(p, shape, mymat, param))
    then ()
    else
      (
        let p_white : t_point = {x = !(pl.cur_shape.base).x ; y = !(pl.cur_shape.base).y} in
        let base_draw : t_point = param.graphics.base in
        let dilat : int = param.graphics.dilat in
        let col : t_color = !(pl.cur_shape.color) in
        fill_pt_list(shape, p_white, base_draw, dilat, white) ;
        drawfill_pt_list(shape, p, base_draw, dilat, col) ;
      )
  )
;;


let move_right(pl : t_play) : unit =
  let p : t_point = {x = !(pl.cur_shape.base).x + 1 ; y = !(pl.cur_shape.base).y} in
  let shape : t_point list = pl.par.shapes.value.(!(pl.cur_shape.shape)).shape in
  let mymat : t_color matrix = pl.mat in
  let param : t_param = pl.par in
  (
    if not(is_free_move(p, shape, mymat, param))
    then ()
    else
      (
        let p_white : t_point = {x = !(pl.cur_shape.base).x ; y = !(pl.cur_shape.base).y} in
        let base_draw : t_point = param.graphics.base in
        let dilat : int = param.graphics.dilat in
        let col : t_color = !(pl.cur_shape.color) in
        fill_pt_list(shape, p_white, base_draw, dilat, white) ;
        drawfill_pt_list(shape, p, base_draw, dilat, col) ;
      )
  )
;;


let move_down(pl : t_play) : unit =
  let p : t_point = {x = !(pl.cur_shape.base).x ; y = !(pl.cur_shape.base).y - 1} in
  let shape : t_point list = pl.par.shapes.value.(!(pl.cur_shape.shape)).shape in
  let mymat : t_color matrix = pl.mat in
  let param : t_param = pl.par in
  (
    if not(is_free_move(p, shape, mymat, param))
    then ()
    else
      (
        let p_white : t_point = {x = !(pl.cur_shape.base).x ; y = !(pl.cur_shape.base).y} in
        let base_draw : t_point = param.graphics.base in
        let dilat : int = param.graphics.dilat in
        let col : t_color = !(pl.cur_shape.color) in
        fill_pt_list(shape, p_white, base_draw, dilat, white) ;
        drawfill_pt_list(shape, p, base_draw, dilat, col) ;
      )
  )
;;


let rotate_right(pl : t_play) : unit =
  let rot_rx : int = pl.par.shapes.value.(!(pl.cur_shape.shape)).rot_rgt_base.x in
  let rot_ry : int = pl.par.shapes.value.(!(pl.cur_shape.shape)).rot_rgt_base.y in
  let p : t_point = {x = !(pl.cur_shape.base).x + rot_rx ; y = !(pl.cur_shape.base).y + rot_ry}  in
  let new_shape_pos : int = pl.par.shapes.value.(!(pl.cur_shape.shape)).rot_rgt_shape in
  let shape : t_point list = pl.par.shapes.value.(new_shape_pos).shape in
  let mymat : t_color matrix = pl.mat in
  let param : t_param = pl.par in
  (
    if not(is_free_move(p, shape, mymat, param))
    then ()
    else
      (
        let old_shape : t_point list = pl.par.shapes.value.(!(pl.cur_shape.shape)).shape in
        let p_white : t_point = {x = !(pl.cur_shape.base).x ; y = !(pl.cur_shape.base).y} in
        let base_draw : t_point = param.graphics.base in
        let dilat : int = param.graphics.dilat in
        let col : t_color = !(pl.cur_shape.color) in
        fill_pt_list(old_shape, p_white, base_draw, dilat, white) ;
        drawfill_pt_list(shape, p, base_draw, dilat, col) ;
      )
  )
;;


let rotate_left(pl : t_play) : unit =
  let rot_lx : int = pl.par.shapes.value.(!(pl.cur_shape.shape)).rot_lft_base.x in
  let rot_ly : int = pl.par.shapes.value.(!(pl.cur_shape.shape)).rot_lft_base.y in
  let p : t_point = {x = !(pl.cur_shape.base).x + rot_lx ; y = !(pl.cur_shape.base).y + rot_ly}  in
  let new_shape_pos : int = pl.par.shapes.value.(!(pl.cur_shape.shape)).rot_lft_shape in
  let shape : t_point list = pl.par.shapes.value.(new_shape_pos).shape in
  let mymat : t_color matrix = pl.mat in
  let param : t_param = pl.par in
  (
    if not(is_free_move(p, shape, mymat, param))
    then ()
    else
      (
        let old_shape : t_point list = pl.par.shapes.value.(!(pl.cur_shape.shape)).shape in
        let p_white : t_point = {x = !(pl.cur_shape.base).x ; y = !(pl.cur_shape.base).y} in
        let base_draw : t_point = param.graphics.base in
        let dilat : int = param.graphics.dilat in
        let col : t_color = !(pl.cur_shape.color) in
        fill_pt_list(old_shape, p_white, base_draw, dilat, white) ;
        drawfill_pt_list(shape, p, base_draw, dilat, col) ;
      )
  )
;;


let move_at_bottom(pl : t_play) : unit =
  let p : t_point ref = pl.cur_shape.base in
  let shape : t_point list = pl.par.shapes.value.(!(pl.cur_shape.shape)).shape in
  let mymat : t_color matrix = pl.mat in
  let param : t_param = pl.par in
  while !(p).y > 0 &&  is_free_move(!p, shape, mymat, param)  
  do
    move_down(pl) ;
    p := {x = !(p).x ; y = !(p).y - 1}
  done ;
  pl.mat.((!p).x).((!p).y) <- !(pl.cur_shape.color)
;;

  
(* ------------------------------------------------- *)
(* ------------------------------------------------- *)
(*    Types, formes, parametrage et initialisation   *)
(* ------------------------------------------------- *)
(* ------------------------------------------------- *)


(* Types *)

type 'a t_array = {len : int ; value : 'a array} ;;

type t_shape = {shape : t_point list ; x_len : int ; y_len : int ; 
                rot_rgt_base : t_point ; rot_rgt_shape : int ; 
                rot_lft_base : t_point ; rot_lft_shape : int} ;; 

type t_cur_shape = {base : t_point ref ; shape : int ref ; color : t_color ref} ;;


type t_param_time = {init : float ; extent : float ; ratio : float} ;;

type t_param_graphics = 
    {base : t_point ; dilat : int ; color_arr : t_color t_array} ;;

type t_param = 
  {time : t_param_time ; 
   mat_szx : int ; mat_szy : int ;
   graphics : t_param_graphics ; 
   shapes : t_shape t_array
} ;;

type t_play = {par : t_param ; cur_shape : t_cur_shape ; mat : t_color matrix} ;;


(* Initialisation de quelques formes et des parametres *)

let init_sh011() : t_shape = 
  {shape = [{x = 0 ; y = 0} ; {x = 1 ; y = 0} ; {x = 2 ; y = 0} ; {x = 3 ; y = 0}]
  ; 
  x_len = 4 ; y_len = 1 ; 
  rot_rgt_base = {x = 1 ;  y = 1} ; rot_rgt_shape = 1 ; 
  rot_lft_base = {x = 2 ; y = 1} ; rot_lft_shape = 1} 
;;
let init_sh112() : t_shape = 
  {shape = [{x = 0 ; y = 0} ; {x = 0 ; y = -1} ; {x = 0 ; y = -2} ; {x = 0 ; y = -3}] ; 
  x_len = 1 ; y_len = 4 ; 
  rot_rgt_base = {x = -2 ;  y = -1} ; rot_rgt_shape = 0 ; 
  rot_lft_base = {x = -1 ; y = -1} ; rot_lft_shape = 0} 
;;
let init_sh211() : t_shape = 
  {shape = [{x = 0 ; y = 0} ; {x = 0 ; y = -1} ; {x = 1 ; y = 0} ; {x = 1 ; y = -1}] ; 
  x_len = 2 ; y_len = 2 ; 
  rot_rgt_base = {x = 0 ;  y = 0} ; rot_rgt_shape = 2 ; 
  rot_lft_base = {x = 0 ;  y = 0} ; rot_lft_shape = 2} 
;;

let init_shapes() : t_shape t_array = 
  {len = 3 ; value = [| init_sh011() ; init_sh112() ; init_sh211() |]} 
;;


let init_color() : t_color t_array = 
  {len = 7 ; value = [|blue ; red ; green ; yellow ; cyan ; magenta ; grey|]}
;;


let init_param() : t_param = 
    {
    time = {init = 1.0 ; extent = 10.0 ; ratio = 0.8} ; 
    mat_szx = 15; mat_szy = 28 ;
    graphics = {base = {x = 50 ; y = 50} ; dilat = 20 ; color_arr = init_color()} ; 
    shapes = init_shapes()
    }
;;


let time_get(p : t_param) : t_param_time =
  p.time
;;


let mat_get(p : t_param) : (int * int) =
  (p.mat_szx, p.mat_szy)
;;


let graphic_get(p : t_param) : t_param_graphics =
  p.graphics
;;


let shape_get(p :t_param) : t_shape t_array =
  p.shapes
;;


let play_change(p, cs, ma : t_param * t_cur_shape * t_color matrix) : t_play =
  {par = p ; cur_shape = t_cur_shape ; mat = t_color matrix}
;;


let color_choice(t : t_color t_array) : t_color =
  let pos : int = rand_int(0, (t.len - 1)) in
  t.value.(pos)
;;


let cur_shape_choice(shapes, mat_szx, mat_szy, color_arr : t_shape t_array * int * int * t_color t_array) : t_cur_shape =
  let shape_rand : int ref = ref(rand_int(0, shapes.len - 1)) in
  let x_pos_max : int = (mat_szx - 1) - shapes.value.(!(shape_rand)).x_len in
  let x_rand : int = rand_int(0, x_pos_max) in
  let color_rand : t_color ref = ref(color_choice(color_arr)) in
  {base = ref({x = x_rand ; y = mat_szy - 1}) ; shape = shape_rand ; color = color_rand}
;;


let rec insert(cur, shape, param, mymat : t_cur_shape * t_point list * t_param * t_color matrix) : bool =
  if isempty(shape)
  then
    (
      let shape_bis : t_point list = param.shapes.value.(!(cur.shape)).shape in
      let base_pt : t_point = !(cur.base) in
      let base_draw : t_point = param.graphics.base in
      let dilat : int = param.graphics.dilat in
      let col : t_color = !(cur.color) in
      (
        drawfill_pt_list(shape_bis, base_pt, base_draw, dilat, col) ;
        true
      )
    )
  else
    let (sx, sy) : (int * int) = ((fst(shape)).x + !(cur.base).x, (fst(shape)).y + !(cur.base).y) in
    if mymat.(sx).(sy) <> white
    then false
    else insert(cur, rem_fst(shape), param, mymat)
;;


(* ----------------------------------------------- *)
(* ----------------------------------------------- *)
(*    Deplacements et controle des deplacements    *)
(* ----------------------------------------------- *)
(* ----------------------------------------------- *)

(* choix des deplacements suivant le caractere saisi *)
let move(pl, dir : t_play * char) : bool = 
  (
  if dir = 't'
    then rotate_right(pl)
    else
      if dir = 'c'
      then rotate_left(pl)
      else
        if dir = 'd'
        then move_left(pl)
        else
          if dir = 'h'
          then move_right(pl)
          else () ;  
  (dir = 'v')
  )
;;


(* ----------------------------------- *)
(* ----------------------------------- *)
(*    Suppression des lignes pleines   *)
(* ----------------------------------- *)
(* ----------------------------------- *)

let rec is_column_full(mymat, y, mat_szx : t_color matrix * int * int) : bool =
  if mat_szx = 0
  then true
  else
    if mymat.(mat_szx - 1).(y) = white
    then false
    else is_column_full(mymat, y, mat_szx - 1)
;;


let decal(mymat, y, szx, szy, par : t_color matrix * int * int * int * t_param) : unit =
  for a = 0 to szx - 1
  do
    for i = y + 1 to szy - 1
    do
      mymat.(a).(i - 1) <- mymat.(a).(i)
    done
  done
;;


let clear_play(pl : t_play) : unit =
  let szy : int = pl.par.mat_szy in
  let szx : int = pl.par.mat_szx in
  for y = 0 to szy - 1
  do
    if is_column_full(pl.mat, y, szx)
    then decal(pl.mat, y, szx, szy, pl.par)
    else ()
  done
;;


let rec final_insert(cur, shape, mymat : t_cur_shape * t_point list * t_color matrix) : unit =
  let base_pt : t_point = !(cur.base) in
  if isempty(shape)
  then ()
  else
    (
      let st : t_point = {x = (fst(shape)).x + base_pt.x ; y = (fst(shape)).y + base_pt.y } in
      mymat.(st.x).(st.y) <- !(cur.color) ;
      final_insert(cur, rem_fst(shape), mymat)
    )
;;

  
let final_newstep(pl : t_play) : bool =
  let cur : t_cur_shape = pl.cur_shape in
  let p : t_point = !(cur.base) in
  let shape : t_point list = pl.par.shapes.value.(!(cur.shape)).shape in
  let mymat : t_color matrix = pl.mat in
  let param : t_param = pl.par in
  let py_move : t_point = {x = p.x ; y = p.y - 1} in
  if is_free_move(py_move, shape, mymat, param)
  then true
  else
    (
      let shapes : t_shape t_array = pl.par.shapes in
      let mat_szx : int = pl.par.mat_szx in
      let mat_szy : int = pl.par.mat_szy in
      let color_arr : t_color t_array = pl.par.graphics.color_arr in
      let new_cur : t_cur_shape = cur_shape_choice(shapes, mat_szx, mat_szy, color_arr) in
      final_insert(cur, shape, mymat) ;
      clear_play(pl) ;
      cur.base := !(new_cur.base) ;
      cur.shape := !(new_cur.shape) ;
      cur.color := !(new_cur.color) ;
      false
    )
;;

  
(* --------------------- *)
(* --------------------- *)
(*   Une etape de jeu    *)
(* --------------------- *)
(* --------------------- *)

let newstep(pl, new_t, t, dt : t_play * float ref * float * float) : bool = 
  let the_end : bool ref = ref (!new_t -. t > dt) and dec : bool ref = ref false in
  let dir : char ref = ref 'x' and notmove : bool ref = ref false in
    (
    while not(!the_end)
    do 
      if key_pressed()
      then dir := read_key()
      else () ;
      dec := move(pl, !dir) ;
      dir := 'x' ; 
      new_t := Sys.time() ;
      the_end := !dec || (!new_t -. t > dt) ;
    done ; 
    if !dec 
    then (move_at_bottom(pl) ; notmove := true)
    else notmove := not(move_down(pl)) ;
    if !notmove
    then the_end := final_newstep(pl)
    else the_end := false;
    !the_end ;
    )
;;

(* ------------------------ *)
(* ------------------------ *)
(*    Fonction principale   *)
(* ------------------------ *)
(* ------------------------ *)


let jeuCP2() : unit =
  let pl : t_play = init_play() in
  let t : float ref = ref (Sys.time()) and new_t : float ref = ref (Sys.time()) in
  let dt : float ref = ref (time_init(pl.par)) and t_acc : float ref = ref (Sys.time()) in
  let the_end : bool ref = ref false in
    while not(!the_end)
    do
      the_end := newstep(pl, new_t, !t, !dt) ; 
      if ((!new_t -. !t_acc) > time_extent(pl.par))
      then 
        (
        dt := !dt *. time_ratio(pl.par) ; 
        t_acc := !new_t
        ) 
      else () ;
      t := !new_t
    done
;;


let init_play() : t_play =
  let param : t_param = init_param() in
  let mymat : t_color matrix = mat_make(param.mat_szx, param.mat_szy, white) in
  let cur : t_cur_shape = cur_shape_choice(param.shapes, param.mat_szx, param.mat_szy, param.graphics.color_arr) in
  let shape : t_point list = param.shapes.value.(!(cur.shape)).shape in
  (
  open_graph(900, 900) ;
  draw_frame(param.graphics.base, param.mat_szx, param.mat_szy, param.graphics.dilat) ;
  insert(cur, shape, param, mymat) ;
  {par = param ; cur_shape = cur ; mat = mymat}
  )
;;


