open CPutil ;;

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


(* --------------------------------- *)
(* --------------------------------- *)
(*   Types et fonctions graphique    *)
(* --------------------------------- *)
(* --------------------------------- *)

type t_point = {x : int ; y : int} ;;


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
  {shape = [{x = 0 ; y = 0} ; {x = 1 ; y = 0} ; {x = 2 ; y = 0} ; {x = 3 ; y = 0}] ; 
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
  {len = 7 ; value = [|blue ; red ; green ; yellow ; cyan ; magenta ; grey|]} ;;

let init_param() : t_param = 
    {
    time = {init = 1.0 ; extent = 10.0 ; ratio = 0.8} ; 
    mat_szx = 15 ; mat_szy = 28 ;
    graphics = {base = {x = 50 ; y = 50} ; dilat = 20 ; color_arr = init_color()} ; 
    shapes = init_shapes()
    }
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


let draw_absolute_pt(p, base_draw, dilat, col : t_point * t_point * int * t_color) : unit =
  let (p_dilat_x, p_dilat_y) : int * int = (p.x * dilat) + base_draw.x, (p.y * dilat) + base_draw.y in
  draw_rect(p_dilat_x, p_dilat_y, dilat - 1, dilat - 1) ;
  set_color(col)
;;


let fill_absolute_pt(p, base_draw, dilat, col : t_point * t_point * int * t_color) : unit =
  let (p_dilat_x, p_dilat_y) : int * int = (p.x * dilat) + base_draw.x, (p.y * dilat) + base_draw.y in
  fill_rect(p_dilat_x, p_dilat_y, dilat - 1, dilat - 1) ;
  set_color(col)
;;

let drawfill_absolute_pt(p,  base_draw, dilat, col : t_point * t_point * int * t_color) : unit =
  let col_draw : t_color = black in
  draw_absolute_pt(p, base_draw, dilat, col_draw) ;
  fill_absolute_pt(p, base_draw, dilat col)
;;


let draw_relative_pt(p, base_draw, dilat, col : t_point * t_point * int * t_color) : unit =
  let (p_newx, p_newy) : int * int = p.x + base_draw.x, p.y + base_draw.y in
  draw_rect(p_newx, p_newy, dilat - 1, dilat - 1)
  set_color(col)
;;

let fill_relative_pt(p, base_draw, dilat, col : t_point * t_point * int * t_color) : unit =
  let (p_newx, p_newy) : int * int = p.x + base_draw.x, p.y + base_draw.y in
  fill_rect(p_newx, p_newy, dilat - 1, dilat - 1)
  set_color(col)
;;

let drawfill_relative_pt(p,  base_draw, dilat, col : t_point * t_point * int * t_color) : unit =
  let col_draw : t_color = black in
  draw_relative_pt(p, base_draw, dilat, col_draw) ;
  fill_relative_pt(p, base_draw, dilat col)
;;

let rec draw_pt_list(l, base_pt, base_draw, dilat, col : t_point list * t_point * t_point * int * t_color) : unit =
  if isempty(l)
  then draw_relative_pt(base_pt, base_draw, dilat, col)
  else (
    draw_relative_pt(fst(l), base_draw, dilat, col) ;
    draw_pt_list(rem(fst), base_pt, base_draw, dilat, col)
  )
;;


let rec fill_pt_list(l, base_pt, base_draw, dilat, col : t_point list * t_point * t_point * int * t_color) : unit =
  if isempty(l)
  then fill_relative_pt(base_pt, base_draw, dilat, col)
  else (
    fill_relative_pt(fst(l), base_draw, dilat, col) ;
    fill_pt_list(rem(fst), base_pt, base_draw, dilat, col)
  )
;;

let drawfill_pt_list(l, base_pt, base_draw, dilat, col : t_point list * t_point * t_point * int * t_color) : unit =
  let col_draw : t_color = black in
  draw_pt_list(l, base_pt, base_draw, dilat, col_draw) ;
  fill_pt_list(l, base_pt, base_draw, dilat, col)
;;

                           
let draw_frame(base_draw, size_x, size_y, dilat : t_point * int * int * int) : unit =
  let (base_ptx, base_pty) : int * int = base_draw.x + size_x, base_draw.y + size_y in
  let matrix : t_point array = mat_make(base_ptx * dilat, base_pty * dilat, base_draw) in
  for a = base_draw.y * dilat to size_y * dilat
  do
    for i = base_draw.x * dilat to size_x * dilat
    do
      matrix.(a).(i) <- {x = i ; y = a}
    done
    fill_pt_list(!(matrix.(a)), {x = base_ptx ; y = base_pty}, base_draw, dilat, black)
  done
;;


let time_change(ini, ext, rat : float * float * float) : t_param_time =
  {init = ini ; extent = ext ; ratio = rat}
;;

let graphic_change(b, d, col : t_point * int * t_color t_array) : t_param_graphics =
  {base = b ; dilat = d ; color_arr = col}
;;

let param_change(t, mszx, mszy, gra, sha : t_param_time * int * int * t_param_graphics * t_shape t_array) : t_param =
  {time = t ; mat_szx = mszx ; mat_szy = mszy ; graphics = gra ; shapes = sha}
;;

let play_change(p, cs, ma : t_param * t_cur_shape * t_color matrix) : t_play =
  {par = p ; cur_shape = t_cur_shape ; mat = t_color matrix}
;;

let color_choice(t : t_color t_array) : t_color =
  let pos : int = rand_int(0, (arr_len(t.value) - 1)) in
  t.value.(pos)
;;

