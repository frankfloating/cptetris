open CPutil ;;
open JeuCP2 ;;

(*test_graphique_draw_absolute_pt*)

(*
test pour p : {x = 0 ; y = 0}
          base_draw : {x = 50 ; y = 50}
          dilat : 20
          col : blue ;
résultat attendu : carrée de longueur (dilat - 1), de tracé bleue à la position {x = 50 ; y = 50} dans la fenêtre graphique. 
résultat obtenu : exatement comme attendu.
test ok ;
pourquoi ce test : test simple sur les fonctions graphics draw_rect() et set_color() de grahics.cma
 *)


(*test_graphique_fill_absolute_pt*)

(*
test pour p : {x = 0 ; y = 0}
          base_draw : {x = 50 ; y = 50}
          dilat : 20
          col : blue ;
résultat attendu : carrée de longueur (dilat - 3), remplie en bleue à la position {x = 50 ; y = 50} dans la fenêtre graphique.
résultat obtenu : exactement comme attendu.
test ok ;
pourquoi ce test : test simple sur les fonctions graphics fill_rect() et set_color() de grahics.cma
 *)


(*test_graphique_drawfill_absolute_pt*)

(*
test pour p : {x = 0 ; y = 0}
          base_draw : {x = 50 ; y = 50}
          dilat : 20
          col : yellow ;
résultat attendu : carrée de longueur (dilat - 1), de tracé noir et remplie en jaune (meilleure visibilité du tracé) à la position {x = 50 ; y = 50} dans la fenêtre graphique.
résultat obtenu : exactement comme attendu.
test ok ;
pourquoi ce test : fonction fait appel aux deux fonctions précèdentes, on cherche seulement à montrer la difference de couleurs entre le remplissage et tracé.
 *)


(*test_graphique_draw_relative_pt*)

(*
test pour p : {x = 0 ; y = 0}
          base_point : {x = 0 ; y = 28}
          base_draw : {x = 50 ; y = 50}
          dilat : 20
          col : black ;
résultat attendu : carré de longueur (dilat - 1) de tracé noir à la position {x = 50 ; y = 610} dans la fenêtre graphique.
résultat obtenu : comme attendu.
test ok ;
pourquoi ce test : fonction simple qui fait appel à draw_absolute_pt() avec translation de p par base_point.
 *)


(*test_graphique_fill_relative_pt*)

(*
test pour p : {x = 0 ; y = 0}
          base_point : {x = 0 ; y = 28}
          base_draw : {x = 50 ; y = 50}
          dilat : 20
          col : black ;
résultat attendu : carré de longueur (dilat - 3) remplie en noire à la position {x = 50 ; y = 610} dans la fenêtre graphique.
résultat obtenu : comme attendu.
test ok ;
pourquoi ce test : fonction simple que fait appel à fill_absolute_pt() avec translation de p par base_point.
 *)


(*test_graphique_drawfill_relative_pt*)

(*
test pour p : {x = 0 ; y = 0}
          base_point : {x = 0 ; y = 28}
          base_draw : {x = 50 ; y = 50}
          dilat : 20
          col : yellow ;
résultat attendu : carré de longeur (dilat - 1) de tracé noire remplie en jaune à la position {x = 50 ; y = 610} dans la fenêtre graphique.
résultat obtenu : comme attendu.
test ok ;
pourquoi ce test : fonction fait appel à drawfill_absolute_pt() avec translation de p par base_point.
 *)


(*test_graphique_draw_pt_list_1*)

(*
test pour l : [{x = 0 ; y = 0} ; {x = 1 ; y = 0} ; {x = 2 ; y = 0} ; {x = 3 ; y = 0}]
          base_pt : {x = 0 ; y = 28}
          base_draw : {x = 50 ; y = 50}
          dilat : 20
          col : black ;
résulat attendu : quatres carrés de longueur (dilat - 1) l'un à côté de l'autre de tracé noire avec position du point à gauche à {x = 50 ; y = 610} dans la fenêtre graphique.
résultat obtenu : comme attendu.
test ok ;
pourquoi ce test : rentre dans la boucle récursive au moins une fois à travers l'alternance, la fonction fait appel à draw_relative_pt() avant rappel de la fonction initiale.
 *)


(*test_graphique_draw_pt_list_2*)

(*
test pour l : []
          base_pt : {x = 0 ; y = 28}
          base_draw : {x = 50 ; y = 50}
          dilat : 20
          col : black ;
résultat attendu : fenêtre graphique vide.
résultat obtenu : comme attendu.
test ok ;
pourquoi ce test : ne rentre pas dans la boucle récursive. 
 *)


(*test_graphique_fill_pt_list_1*)

(*
test pour l : [{x = 0 ; y = 0} ; {x = 1 ; y = 0} ; {x = 2 ; y = 0} ; {x = 3 ; y = 0}]
          base_pt : {x = 0 ; y = 28}
          base_draw : {x = 50 ; y = 50}
          dilat : 20
          col : black ;
résultat attendu : quatres carrés de longueur (dilat - 1) l'un à côté de l'autre remplies en noire avec position du point à gauche à {x = 50 ; y = 610} dans la fenêtre graphique.
résultat obtenu : comme attendu.
test ok ;
pourquoi ce test : rentre dans la boucle au moins une fois à travers l'alternance avec appel à fonction fill_relative_pt().
 *)


(*test_graphique_fill_pt_list_2*)

(*
test pour l : []
          base_pt : {x = 0 ; y = 28}
          base_draw : {x = 50 ; y = 50}
          dilat : 20
          col : black ;
résultat attendu : fenêtre graphique vide.
résultat obtenu : comme attendu.
test ok ;
pourquoi ce test : ne rentre pas dans la boucle récursive. 
 *)


(*test_graphique_drawfill_pt_list_1*)

(* 
test pour l : [{x = 0 ; y = 0} ; {x = 1 ; y = 0} ; {x = 2 ; y = 0} ; {x = 3 ; y = 0}]
          base_pt : {x = 0 ; y = 28}
          base_draw : {x = 50 ; y = 50}
          dilat : 20
          col : yellow ;
résultat attendu : quatres carrés de longueur (dilat - 1) l'un à côté de l'autre remplies en jaune de tracé noire avec position du point à gauche à {x = 50 ; y = 610} dans la fenêtre graphique.
résultat obtenu : comme attendu.
test ok ;
pourquoi ce test : rentre dans la boucle au moins une fois à travers l'alternance avec appel à fonction drawfill_relative_pt().
 *)


(*test_graphique_drawfill_pt_list_2*)

(*
test pour l : []
          base_pt : {x = 0 ; y = 28}
          base_draw : {x = 50 ; y = 50}
          dilat : 20
          col : yellow ;
résultat attendu : fenêtre graphique vide.
résultat obtenu : comme attendu.
test ok ;
pourquoi ce test : ne rentre pas dans la boucle récursive. 
 *)


(*test_graphique_drawframe_1*)

(*
test pour base_draw : {x = 50 ; y = 50}
          size_x : 15
          size_y : 28
          dilat : 20 ;
résulat attendu : cadre de carrés remplies en noire de longueurs (dilat - 1) entourant zone d'affichage.
résulat obtenu : comme attendu.
test ok ;
pourquoi ce test : la fonction fait appel à la fonction drawfill_absolute_pt() dans 3 boucles itératives différentes, on rentre donc dans chaques.
 *)


(*test_graphique_drawframe_2*)

(* 
test pour base_draw : {x = 50 ; y = 50}
          size_x : -3
          size_y : -3
          dilat : 20 ;
résulat attendu : fenêtre grahique vide. 
résulat obtenu : comme attendu.
test ok ;
pourquoi ce test : on ne veut pas rentrer dans les sous_boucles (celles qui appels drawfill_absolute_pt().
 *)


(*test_structurel_color_choice*)

let test_structurel_color_choice (status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_color_choice") in
  let col_arr : t_color t_array = init_color() in
  let test_result : t_color t_test_result = test_exec(test_step, color_choice, col_arr) in
  for a = 0 to col_arr.len - 1
  do
    if (a = col_arr.len - 1) && (test_get(test_result) <> col_arr.value.(a))
    then test_error(test_step)
    else
      if col_arr.value.(a) = test_get(test_result)
      then assert_equals(test_step, "color exists", test_get(test_result), col_arr.value.(a))
      else ()
  done ;
  test_end(test_step)
;;
(*pourquoi ce test : on veut verifier que l'appel de la fonction color_choice nous rend une couleur définie.*)


(*test_structurel_cur_shape_choice*)

let test_structurel_cur_shape_choice(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_cur_shape_choice") in
  let shape_arr : t_shape t_array = init_shapes() and col_arr : t_color t_array = init_color() in
  let test_result : t_cur_shape t_test_result = test_exec(test_step, cur_shape_choice, (shape_arr, 15, 28, col_arr)) in
  let res : t_cur_shape = test_get(test_result) in
  for a = 0 to col_arr.len - 1
  do
    if (a = col_arr.len - 1) && (col_arr.value.(a) <> !(res.color))
    then test_error(test_step)
    else
      if col_arr.value.(a) = !(res.color)
      then assert_equals(test_step, "couleur dans array", col_arr.value.(a), !(res.color))
      else () 
  done ;
  (
  if !(res.shape) < 0 || !(res.shape) >= shape_arr.len
  then assert_true(test_step, "shape valide", test_is_success(test_result))
  else ()
  )
;;  
(*pourquoi ce test : on veut verifier que l'appel de la fonction cur_shape_choice rend une forme et couleur valide*)

(*test_graphique_insert*)

(*
test pour cur : {base = ref({x = 10 ; y = 15}) ; shape = ref(0) ; color = ref(blue)}
          shape : [{x = 0; y = 0}; {x = 1; y = 0}; {x = 2; y = 0}; {x = 3; y = 0}]
          param : init_param()
          mymat : mat_make(15, 28, white) ;
résultat attendu : forme bleue aux point base graphique {x = 1200 ; y = 1300}.
résultat obtenu : comme attendu.
test ok ;
pourquoi ce test : si bool est vraie, la fonction fait appel à la fonction drawfill_pt_list().
 *)

(*test_structurel_insert_1*)

let test_structurel_insert_1(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_insert_1") in
  let cur : t_cur_shape = cur_shape_choice(init_shapes(), 15, 28, init_color()) and shape : t_point list = (init_sh011()).shape in
  let param : t_param = init_param() and mymat : t_color matrix = mat_make(15, 28, white) in
  let test_result : bool t_test_result = test_exec(test_step, insert, (cur, shape, param, mymat)) in
  assert_true(test_step, "dans la boucle et premier consequence", test_get(test_result)) ;
  test_end(test_step)
;;
(*on verifie que nous rentrons dans les deux conséquences et la première alternance de la fonction, donc dans la boucle récursive*)


(*test_structurel_insert_2*)

let test_structurel_insert_2(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_insert_2") in
  let cur : t_cur_shape = cur_shape_choice(init_shapes(), 15, 28, init_color()) and shape : t_point list = (init_sh011()).shape in
  let param : t_param = init_param() and mymat : t_color matrix = mat_make(15, 28, white) in
  let (shape_in_spacex, shape_in_spacey) : (int * int) = ((fst(shape)).x + !(cur.base).x, (fst(shape)).y + !(cur.base).y) in 
  mymat.(shape_in_spacex).(shape_in_spacey) <- blue ; 
  let test_result : bool t_test_result = test_exec(test_step, insert, (cur, shape, param, mymat)) in
  assert_false(test_step, "dans deuxième alternance", test_get(test_result)) ;
  test_end(test_step)
;;
(*on vérifie que nous rentrons dans la deuxième alternance de la fonction*)


(*test_strucutrel_init_play*)

(*test graphique*)
(*
résultat attendu : cadre avec une forme aléatoire position x randomisé et y à position égale à celle de la taille y du cadre.
résulatat obtenu : comme attendu.
test ok ;
pourquoi ce test : vérifier que les fonctions draw_frame() et drawfill_pt_list() fonctionnent ensembles.
 *)

let test_structurel_init_play(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_init_play") in
  let prm : t_param = init_param() in
  let test_result : t_play t_test_result = test_exec(test_step, init_play, ()) in
  for a = 0 to prm.mat_szx - 1
  do
    for i = 0 to prm.mat_szy - 1
    do
      assert_equals(test_step, "case blanche", (test_get(test_result)).mat.(a).(i), white) ;
      assert_equals(test_step, "score nul", !((test_get(test_result)).score) , 0) ;
      assert_false(test_step, "aucune bomb", !((test_get(test_result)).bomb.bomb))
    done
  done ;
  test_end(test_step)
;;
(*pourquoi ce text : on vérifie que la matrix est blanche, couleur choisit pour case vide,  donc elle ne contient aucune forme, que le score est nul et que la bomb n'existe pas (false)*)


(*test_structurel_valid_matrix_point_1*)

let test_structurel_valid_matrix_point_1(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_valid_matrix_point_1") in
  let p : t_point = {x = 1 ; y = 5} and param : t_param = init_param() in
  let test_result : bool t_test_result = test_exec(test_step, valid_matrix_point, ( p, param)) in
  assert_true(test_step, "point dans mat", test_get(test_result)) ;
  test_end(test_step)
;;
(*pourquoi ce text : on vérifie qu le point est dans l'éspace de travail, la matrix*)


(*test_structurel_vaild_matrix_point_2*)

let test_structurel_valid_matrix_point_2(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_valid_matrix_point_2") in
  let p : t_point = {x = 16 ; y = 5} and param : t_param = init_param() in
  let test_result : bool t_test_result = test_exec(test_step, valid_matrix_point, (p, param)) in
  assert_false(test_step, "point pas dans mat", test_get(test_result)) ;
  test_end(test_step)
;;
(*pourquoi ce test : on vérifie que le point est en dehors de l'éspace de travail*)


(*test_structurel_is_free_move_1*)

let test_structurel_is_free_move_1(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_is_free_move_1") in
  let p : t_point = {x = 1 ; y = 5} and param : t_param = init_param() in
  let shape : t_point list = param.shapes.value.(0).shape and mymat : t_color matrix = mat_make(15, 28, white) in
  let pl : t_play = {par = init_param() ; cur_shape = {base = ref({x = 5 ; y = 10}) ; shape = ref(0) ; color = ref(blue)} ; mat = mat_make(15, 28, white) ; score = ref(0) ; bomb = {bomb = ref(false) ; bomb_pos = ref({x = 0 ; y = 0})}} in
  let test_result : bool t_test_result = test_exec(test_step, is_free_move, (p, shape, mymat, param, pl)) in
  assert_true(test_step, "cases libres", test_get(test_result)) ;
  test_end(test_step)
;;
(*pourquoi ce test : rentre dans la première conséquence à travers la troisième alternance*)


(*test_structurel_is_free_move_2*)

let test_structurel_is_free_move_2(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_is_free_move_2") in
  let p : t_point = {x = 1 ; y = 5} and param : t_param = init_param() in
  let shape : t_point list = param.shapes.value.(0).shape and mymat : t_color matrix = mat_make(15, 28, white) in
  let pl : t_play = {par = init_param() ; cur_shape = {base = ref({x = 5 ; y = 10}) ; shape = ref(0) ; color = ref(blue)} ; mat = mat_make(15, 28, white) ; score = ref(0) ; bomb = {bomb = ref(false) ; bomb_pos = ref({x = 0 ; y = 0})}} in
  mymat.(p.x).(p.y) <- blue ;
  let test_result : bool t_test_result = test_exec(test_step, is_free_move, (p, shape, mymat, param, pl)) in
  assert_false(test_step, "cases non libres", test_get(test_result)) ;
  test_end(test_step)
;;  
(*pourquoi ce test : rentre dans la deuxième alternance*)


(*test_structurel_is_free_move_3*)

let test_structurel_is_free_move_3(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_is_free_move_3") in
  let p : t_point = {x = -1 ; y = 5} and param : t_param = init_param() in
  let shape : t_point list = param.shapes.value.(0).shape and mymat : t_color matrix = mat_make(15, 28, white) in
  let pl : t_play = {par = init_param() ; cur_shape = {base = ref({x = 5 ; y = 10}) ; shape = ref(0) ; color = ref(blue)} ; mat = mat_make(15, 28, white) ; score = ref(0) ; bomb = {bomb = ref(false) ; bomb_pos = ref({x = 0 ; y = 0})}} in
  let test_result : bool t_test_result = test_exec(test_step, is_free_move, (p, shape, mymat, param, pl)) in
  assert_false(test_step, "cases non existante", test_get(test_result)) ;
  test_end(test_step)
;;  
(*pourquoi ce test : rentre dans la dernière alternance*)


(*test_structurel_is_free_move_4*)

let test_structurel_is_free_move_4(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_is_free_move_4") in
  let p : t_point = {x = 1 ; y = 6} and param : t_param = init_param() in
  let shape : t_point list = param.shapes.value.(0).shape and mymat : t_color matrix = mat_make(15, 28, white) in
  let pl : t_play = {par = init_param() ; cur_shape = {base = ref({x = 0 ; y = 10}) ; shape = ref(0) ; color = ref(blue)} ; mat = mat_make(15, 28, white) ; score = ref(0) ; bomb = {bomb = ref(true) ; bomb_pos = ref({x = 0 ; y = 0})}} in
  for a = 0 to pl.par.mat_szx - 1
  do
    pl.mat.(a).(0) <- blue ;
    pl.mat.(a).(1) <- blue ;
    pl.mat.(a).(2) <- blue ;
    pl.mat.(a).(3) <- blue ;
    pl.mat.(a).(4) <- blue ;
  done ;
  mymat.(2).(6) <- black ;
  let test_result : bool t_test_result = test_exec(test_step, is_free_move, (p, shape, mymat, param, pl)) in
  assert_true(test_step, "cases libres", test_get(test_result)) ;
  let count : int ref = ref 0 in
  for a = 0 to pl.par.mat_szx - 1
  do
    for i = 0 to 4
    do
      if pl.mat.(a).(i) = white
      then count := !count + 1
      else ()
    done
  done ;
  assert_true(test_step, "explosion a lieu", !count >= 5) ;
  test_end(test_step)
;;  
(*pourquoi ce test : rentre dans première conséquence en passant par dernière conséquence*)


(*test_graphique_move_left*)

(*
test pour pl : {par = init_param() ; cur_shape = {base = ref({x = 5 ; y = 10}) ; shape = ref(0) ; color = ref(blue)} ; mat = mat_make(15, 28, white) ; score = ref(0) ; bomb = {bomb = ref(false) ; bomb_pos = ref({x = 0 ; y = 0})}} ;
avec : open_graph(900, 900) ;;
     drawfill_pt_list([{x = 0; y = 0}; {x = 1; y = 0}; {x = 2; y = 0}; {x = 3; y = 0}], {x = 5 ; y = 10}, {x =50 ; y = 50}, 20 , blue) ;;
     draw_frame({x = 50 ; y = 50}, 15, 28, 20) ;;
résultat attendu : forme antécédent "éffacé" et remplacé par une translation à gauche d'une case.
résultat obtenu : comme attendu.
test ok ;
pourquoi ce test : test fait appel à fill_pt_list() et drawfill_pt_list().
 *)

(*test_structurel_move_left_1*)

let test_structurel_move_left_1(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_move_left_1") in
  let pl : t_play = {par = init_param() ; cur_shape = {base = ref({x = 5 ; y = 10}) ; shape = ref(0) ; color = ref(blue)} ; mat = mat_make(15, 28, white) ; score = ref(0) ; bomb = {bomb = ref(false) ; bomb_pos = ref({x = 0 ; y = 0})}} in
  test_exec(test_step, move_left, pl) ;
  assert_equals(test_step, "forme bouge à gauche", !(pl.cur_shape.base), {x = 4 ; y = 10}) ;
  test_end(test_step)
;;
(*pourquoi ce test : rentre dans alternance*)


(*test_structurel_move_left_2*)

let test_structurel_move_left_2(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_move_left_2") in
  let pl : t_play = {par = init_param() ; cur_shape = {base = ref({x = 0 ; y = 10}) ; shape = ref(0) ; color = ref(blue)} ; mat = mat_make(15, 28, white) ; score = ref(0) ; bomb = {bomb = ref(false) ; bomb_pos = ref({x = 0 ; y = 0})}} in
  test_exec(test_step, move_left, pl) ;
  assert_equals(test_step, "forme ne bouge pas à gauche", !(pl.cur_shape.base), {x = 0 ; y = 10}) ;
  test_end(test_step)
;;
(*pourquoi ce test : rentre dans conséquence*)



(*test_graphique_move_right*)

(*
test pour pl : {par = init_param() ; cur_shape = {base = ref({x = 5 ; y = 10}) ; shape = ref(0) ; color = ref(blue)} ; mat = mat_make(15, 28, white) ; score = ref(0) ; bomb = {bomb = ref(false) ; bomb_pos = ref({x = 0 ; y = 0})}} ;
avec : open_graph(900, 900) ;;
     drawfill_pt_list([{x = 0; y = 0}; {x = 1; y = 0}; {x = 2; y = 0}; {x = 3; y = 0}], {x = 5 ; y = 10}, {x =50 ; y = 50}, 20 , blue) ;;
     draw_frame({x = 50 ; y = 50}, 15, 28, 20) ;;
résultat attendu : forme antécédent "éffacé" et remplacé par une translation à droite d'une case.
résultat obtenu : comme attendu.
test ok ;
pourquoi ce test : test fait appel à fill_pt_list() et drawfill_pt_list().
 *)


(*test_structurel_move_right_1*)

let test_structurel_move_right_1(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_move_right_1") in
  let pl : t_play = {par = init_param() ; cur_shape = {base = ref({x = 5 ; y = 10}) ; shape = ref(0) ; color = ref(blue)} ; mat = mat_make(15, 28, white) ; score = ref(0) ; bomb = {bomb = ref(false) ; bomb_pos = ref({x = 0 ; y = 0})}} in
  test_exec(test_step, move_right, pl) ;
  assert_equals(test_step, "forme bouge à droite", !(pl.cur_shape.base), {x = 6 ; y = 10}) ;
  test_end(test_step)
;;
(*pourquoi ce test : rentre dans alternance*)


(*test_structurel_move_right_2*)

let test_structurel_move_right_2(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_move_right_2") in
  let pl : t_play = {par = init_param() ; cur_shape = {base = ref({x = 14 ; y = 10}) ; shape = ref(0) ; color = ref(blue)} ; mat = mat_make(15, 28, white) ; score = ref(0) ; bomb = {bomb = ref(false) ; bomb_pos = ref({x = 0 ; y = 0})}} in
  test_exec(test_step, move_right, pl) ;
  assert_equals(test_step, "forme ne bouge pas à droite", !(pl.cur_shape.base), {x = 14 ; y = 10}) ;
  test_end(test_step)
;;
(*pourquoi ce test : rentre dans conséquence*)


(*test_graphique_move_down*)

(*
test pour pl : {par = init_param() ; cur_shape = {base = ref({x = 5 ; y = 10}) ; shape = ref(0) ; color = ref(blue)} ; mat = mat_make(15, 28, white) ; score = ref(0) ; bomb = {bomb = ref(false) ; bomb_pos = ref({x = 0 ; y = 0})}} ;
avec : open_graph(900, 900) ;;
     drawfill_pt_list([{x = 0; y = 0}; {x = 1; y = 0}; {x = 2; y = 0}; {x = 3; y = 0}], {x = 5 ; y = 10}, {x =50 ; y = 50}, 20 , blue) ;;
     draw_frame({x = 50 ; y = 50}, 15, 28, 20) ;;
résultat attendu : forme antécédent "éffacé" et remplacé par une translation vers le bas  d'une case.
résultat obtenu : comme attendu.
test ok ;
pourquoi ce test : test fait appel à fill_pt_list() et drawfill_pt_list().
 *)


(*test_structurel_move_down_1*)

let test_structurel_move_down_1(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_move_down_1") in
  let pl : t_play = {par = init_param() ; cur_shape = {base = ref({x = 5 ; y = 10}) ; shape = ref(0) ; color = ref(blue)} ; mat = mat_make(15, 28, white) ; score = ref(0) ; bomb = {bomb = ref(false) ; bomb_pos = ref({x = 0 ; y = 0})}} in
  let test_result : bool t_test_result = test_exec(test_step, move_down, pl) in
  assert_equals(test_step, "forme descend", !(pl.cur_shape.base), {x = 5 ; y = 9}) ;
  assert_true(test_step, "bool true", test_get(test_result)) ;
  test_end(test_step)
;;
(*pourquoi ce test : rentre dans alternance*)


(*test_structurel_move_down_2*)

let test_structurel_move_down_2(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_move_down_2") in
  let pl : t_play = {par = init_param() ; cur_shape = {base = ref({x = 5 ; y = 0}) ; shape = ref(0) ; color = ref(blue)} ; mat = mat_make(15, 28, white) ; score = ref(0) ; bomb = {bomb = ref(false) ; bomb_pos = ref({x = 0 ; y = 0})}} in
  let test_result : bool t_test_result = test_exec(test_step, move_down, pl) in
  assert_equals(test_step, "forme ne bouge pas", !(pl.cur_shape.base), {x = 5 ; y = 0}) ;
  assert_false(test_step, "bool false", test_get(test_result)) ;
  test_end(test_step)
;;
(*pourquoi ce test : rentre dans conséquence*)


(*test_graphique_rotate_right*)

(*
test pour pl : {par = init_param() ; cur_shape = {base = ref({x = 5 ; y = 10}) ; shape = ref(0) ; color = ref(blue)} ; mat = mat_make(15, 28, white) ; score = ref(0) ; bomb = {bomb = ref(false) ; bomb_pos = ref({x = 0 ; y = 0})}} ;
avec : open_graph(900, 900) ;;
     drawfill_pt_list([{x = 0; y = 0}; {x = 1; y = 0}; {x = 2; y = 0}; {x = 3; y = 0}], {x = 5 ; y = 10}, {x =50 ; y = 50}, 20 , blue) ;;
     draw_frame({x = 50 ; y = 50}, 15, 28, 20) ;;
résultat attendu :  forme antécédent "éffacé" et remplacé par translation (rotation de la forme horizontale vers verticale.
résultat obtenu : comme attendu.
test ok ;
pourquoi ce test : test fait appel à fill_pt_list() et drawfill_pt_list().
 *)

(*test_structurel_rotate_right_1*)

let test_structurel_rotate_right_1(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_rotate_right_1") in
  let pl : t_play = {par = init_param() ; cur_shape = {base = ref({x = 5 ; y = 10}) ; shape = ref(0) ; color = ref(blue)} ; mat = mat_make(15, 28, white) ; score = ref(0) ; bomb = {bomb = ref(false) ; bomb_pos = ref({x = 0 ; y = 0})}} in
  test_exec(test_step, rotate_right, pl) ;
  assert_equals(test_step, "forme tourne à droite", !(pl.cur_shape.base), {x = 6 ; y = 11}) ;
  assert_equals(test_step, "forme devient shape 1", !(pl.cur_shape.shape), 1) ;
  test_end(test_step)
;;
(*pourquoi ce test : rentre dans alternance*)


(*test_structurel_rotate_right_2*)

let test_structurel_rotate_right_2(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_rotate_right_2") in
  let pl : t_play = {par = init_param() ; cur_shape = {base = ref({x = 15 ; y = 10}) ; shape = ref(0) ; color = ref(blue)} ; mat = mat_make(15, 28, white) ; score = ref(0) ; bomb = {bomb = ref(false) ; bomb_pos = ref({x = 0 ; y = 0})}} in
  test_exec(test_step, rotate_right, pl) ;
  assert_equals(test_step, "forme ne tourne pas à droite", !(pl.cur_shape.base), {x = 15 ; y = 10}) ;
  assert_equals(test_step, "forme inchangé", !(pl.cur_shape.shape), 0) ;
  test_end(test_step)
;;
(*pourquoi ce test : rentre dans conséquence*)


(*test_graphique_rotate_left*)

(*
test pour pl : {par = init_param() ; cur_shape = {base = ref({x = 5 ; y = 10}) ; shape = ref(0) ; color = ref(blue)} ; mat = mat_make(15, 28, white) ; score = ref(0) ; bomb = {bomb = ref(false) ; bomb_pos = ref({x = 0 ; y = 0})}} ;
avec : open_graph(900, 900) ;;
     drawfill_pt_list([{x = 0; y = 0}; {x = 1; y = 0}; {x = 2; y = 0}; {x = 3; y = 0}], {x = 5 ; y = 10}, {x =50 ; y = 50}, 20 , blue) ;;
     draw_frame({x = 50 ; y = 50}, 15, 28, 20) ;;
résultat attendu :  forme antécédent "éffacé" et remplacé par translation (rotation de la forme horizontale vers verticale.
résultat obtenu : comme attendu.
test ok ;
pourquoi ce test : test fait appel à fill_pt_list() et drawfill_pt_list().
 *)


(*test_structurel_rotate_left_1*)

let test_structurel_rotate_left_1(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_rotate_left_1") in
  let pl : t_play = {par = init_param() ; cur_shape = {base = ref({x = 5 ; y = 10}) ; shape = ref(0) ; color = ref(blue)} ; mat = mat_make(15, 28, white) ; score = ref(0) ; bomb = {bomb = ref(false) ; bomb_pos = ref({x = 0 ; y = 0})}} in
  test_exec(test_step, rotate_left, pl) ;
  assert_equals(test_step, "forme tourne à gauche", !(pl.cur_shape.base), {x = 7 ; y = 11}) ;
  assert_equals(test_step, "forme devient shape 1", !(pl.cur_shape.shape), 1) ;
  test_end(test_step)
;;
(*pourquoi ce test : rentre dans alternance*)


(*test_structurel_rotate_left_2*)

let test_structurel_rotate_left_2(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_rotate_left_2") in
  let pl : t_play = {par = init_param() ; cur_shape = {base = ref({x = 15 ; y = 10}) ; shape = ref(0) ; color = ref(blue)} ; mat = mat_make(15, 28, white) ; score = ref(0) ; bomb = {bomb = ref(false) ; bomb_pos = ref({x = 0 ; y = 0})}} in
  test_exec(test_step, rotate_left, pl) ;
  assert_equals(test_step, "forme ne tourne pas à gauche", !(pl.cur_shape.base), {x = 15 ; y = 10}) ;
  assert_equals(test_step, "forme inchangé", !(pl.cur_shape.shape), 0) ;
  test_end(test_step)
;;
(*pourquoi ce test : rentre dans conséquence*)


(*test_graphique_move_at_bottom_1*)

(* 
test pour pl : {par = init_param() ; cur_shape = {base = ref({x = 5 ; y = 10}) ; shape = ref(0) ; color = ref(blue)} ; mat = mat_make(15, 28, white) ; score = ref(0) ; bomb = {bomb = ref(false) ; bomb_pos = ref({x = 0 ; y = 0})}} ;
avec : open_graph(900, 900) ;;
       drawfill_pt_list([{x = 0; y = 0}; {x = 1; y = 0}; {x = 2; y = 0}; {x = 3; y = 0}], {x = 5 ; y = 10}, {x =50 ; y = 50}, 20 , blue) ;;
       draw_frame({x = 50 ; y = 50}, 15, 28, 20) ;;
résultat attendu : forme bleu atteint le bas de l'espace de travail.
résultat obtenu : comme attendu.
test ok ;
pourquoi ce test : vérifier que la forme repose sur bas de l'espace d'affichage
 *)


(*test_strucurel_move_at_bottom*)

let test_structurel_move_at_bottom(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_move_at_bottom") in
  let pl : t_play = {par = init_param() ; cur_shape = {base = ref({x = 5 ; y = 10}) ; shape = ref(0) ; color = ref(blue)} ; mat = mat_make(15, 28, white) ; score = ref(0) ; bomb = {bomb = ref(false) ; bomb_pos = ref({x = 0 ; y = 0})}} in
  test_exec(test_step, move_at_bottom, pl);
  assert_equals(test_step, "forme en bas de l'espace de travail", !(pl.cur_shape.base), {x = 5 ; y = 0}) ; 
  test_end(test_step)
;;
(*pourquoi ce test : condition d'arrêt provient de move_down, on rentre dans la boucle malgré les coordonnés du point base de forme*)


(*test_graphique_move_at_bottom_2*)

(* 
test pour pl : {par = init_param() ; cur_shape = {base = ref({x = 5 ; y = 10}) ; shape = ref(0) ; color = ref(blue)} ; mat = mat_make(15, 28, white) ; score = ref(0) ; bomb = {bomb = ref(false) ; bomb_pos = ref({x = 0 ; y = 0})}} ;
avec : open_graph(900, 900) ;;
       drawfill_pt_list([{x = 0; y = 0}; {x = 1; y = 0}; {x = 2; y = 0}; {x = 3; y = 0}], {x = 5 ; y = 10}, {x =50 ; y = 50}, 20 , blue) ;;
       draw_frame({x = 50 ; y = 50}, 15, 28, 20) ;;
résultat attendu : forme bleu atteint s'arrête à la position {x = 5 ; y = 6}.
résultat obtenu : comme attendu.
test ok ;
pourquoi ce test : vérifier que la forme repose sur une autre
 *)


(*test_structurel_is_column_full_1*)

let test_structurel_is_column_full_1(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_is_column_full_1") in
  let mymat : t_color matrix = mat_make(15, 28, white) in
  for a = 0 to 14
  do
    mymat.(a).(0) <- blue
  done ;
  let test_result : bool t_test_result = test_exec(test_step, is_column_full, (mymat, init_param(), 0, 15)) in
  assert_true(test_step, "column full", test_get(test_result)) ;
  test_end(test_step)
;;
(*pourquoi ce test : vérifier que la colonne est remplie en passant par l'alternance*)


(*test_structurel_is_column_full_2*)

let test_structurel_is_column_full_2(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_is_column_full_2") in
  let mymat : t_color matrix = mat_make(15, 28, white) in
  for a = 0 to 14
  do
    mymat.(a).(0) <- blue
  done ;
  mymat.(1).(0) <- white ;
  let test_result : bool t_test_result = test_exec(test_step, is_column_full, (mymat, init_param(), 0, 15)) in
  assert_false(test_step, "column not full", test_get(test_result)) ;
  test_end(test_step)
;;
(*pourquoi ce test : vérifier que la colonne est remplie en passant par la conséquence*)


(*test_structurel_decal_1*)

let test_structurel_decal_1(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_decal_1") in
  let mymat : t_color matrix = mat_make(15, 28, white) in
  for a = 0 to 14
  do
    mymat.(a).(1) <- blue
  done ;
  test_exec(test_step, decal, (mymat, 0, 15, 28, init_param())) ;
  for i = 0 to 14
  do
    assert_equals(test_step, "colonne descend", mymat.(i).(0), blue)
  done ;
  test_end(test_step)
;;
(*pourquoi ce test : on test la fonction en rentrant dans les deux boucles*)


(*test_structurel_decal_2*)

let test_structurel_decal_2(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_decal_2") in
  let mymat : t_color matrix = mat_make(15, 28, white) in
  for a = 0 to 14
  do
    mymat.(a).(1) <- blue
  done ;
  test_exec(test_step, decal, (mymat, 0, -1, 28, init_param())) ;
  for i = 0 to 14
  do
    assert_equals(test_step, "colonne reste", mymat.(i).(0), white)
  done ;
  test_end(test_step)
;;
(*pourquoi ce test : on test la fonction en ne rentrant pas dans les deux boucles*)


(*test_graphique_clear_play_1*)

(*
test pour pl : {par = init_param() ; cur_shape = {base = ref({x = 5 ; y = 10}) ; shape = ref(0) ; color = ref(blue)} ; mat = mat_make(15, 28, white) ; score = ref(0) ; bomb = {bomb = ref(false) ; bomb_pos = ref({x = 0 ; y = 0})}} ;
avec : open_graph(900, 900) ;;
       draw_frame({x = 50 ; y = 50}, 15, 28, 20) ;;
       for a = 0 to 14
       do
        pl.mat.(a).(1) <- blue ;
        pl.mat.(a).(3) <- blue ;
       done ;
résultat attendu : ligne pleine effacé.
résultat obtenu : comme attendu.
test ok ;
pourquoi ce test : vérifier que lignes pleines disparaient et remplacées par ligne du dessus
 *)


(*test_structurel_clear_play_1*)

let test_structurel_clear_play_1(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_clear_play_1") in
  let pl : t_play = {par = init_param() ; cur_shape = {base = ref({x = 5 ; y = 10}) ; shape = ref(0) ; color = ref(blue)} ; mat = mat_make(15, 28, white) ; score = ref(0) ; bomb = {bomb = ref(false) ; bomb_pos = ref({x = 0 ; y = 0})}} in
  for a = 0 to 14
  do
    pl.mat.(a).(1) <- blue ;
    pl.mat.(a).(3) <- blue ;
  done ;
  test_exec(test_step, clear_play, pl) ;
  for i = 0 to 14
  do
    assert_equals(test_step, "colonnes 1 nettoyés", pl.mat.(i).(1), white) ;
    assert_equals(test_step, "colonnes 3 nettoyés", pl.mat.(i).(2), white) ;
  done ;
  test_end(test_step)
;;
(*pourquoi ce test : on test la fonction en rentrant dans la première conséquence*)


(*test_graphique_clear_play_2*)

(*
test pour pl : {par = init_param() ; cur_shape = {base = ref({x = 5 ; y = 10}) ; shape = ref(0) ; color = ref(blue)} ; mat = mat_make(15, 28, white) ; score = ref(0) ; bomb = {bomb = ref(false) ; bomb_pos = ref({x = 0 ; y = 0})}} ;
avec : open_graph(900, 900) ;;
       draw_frame({x = 50 ; y = 50}, 15, 28, 20) ;;
       for a = 0 to 14
       do
        pl.mat.(a).(1) <- blue ;
        pl.mat.(a).(3) <- blue ;
       done ;
       pl.mat.(0).(1) <- white ;
       pl.mat.(0).(3) <- white ;
résultat attendu : lignes non effacées.
résultat obtenu : comme attendu.
test ok ;
pourquoi ce test : vérifier que lignes non pleines inchangés
 *)

(*test_structurel_clear_play_2*)

let test_structurel_clear_play_2(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_clear_play_1") in
  let pl : t_play = {par = init_param() ; cur_shape = {base = ref({x = 5 ; y = 10}) ; shape = ref(0) ; color = ref(blue)} ; mat = mat_make(15, 28, white) ; score = ref(0) ; bomb = {bomb = ref(false) ; bomb_pos = ref({x = 0 ; y = 0})}} in
  for a = 0 to 14
  do
    pl.mat.(a).(1) <- blue ;
    pl.mat.(a).(3) <- blue ;
  done ;
  (
  pl.mat.(0).(1) <- white ;
  pl.mat.(0).(3) <- white ;
  test_exec(test_step, clear_play, pl) ;
  ) ;
  for i = 1 to 14
  do
    assert_equals(test_step, "colonnes 1 non nettoyés", pl.mat.(i).(1), blue) ;
    assert_equals(test_step, "colonnes 3 non nettoyés", pl.mat.(i).(3), blue) ;
  done ;
  test_end(test_step)
;;
(*pourquoi ce test : on test la fonction en rentrant dans la première l'altérnance*)


(*test_structurel_final_insert*)

let test_structurel_final_insert(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_final_insert") in
  let cur : t_cur_shape = {base = ref({x = 5 ; y = 5}) ; shape = ref(0) ; color = ref(blue)} in
  let shape : t_point list = (init_sh011()).shape in
  let mymat : t_color matrix = mat_make(15, 28, white) in
  (
  test_exec(test_step, final_insert, (cur, shape, mymat)) ;
  assert_equals(test_step, "forme carré base", mymat.(5).(5), blue) ;
  assert_equals(test_step, "forme carré 1", mymat.(6).(5), blue) ;
  assert_equals(test_step, "forme carré 2", mymat.(7).(5), blue) ;
  assert_equals(test_step, "forme carré 3", mymat.(8).(5), blue) ;
  ) ;
  test_end(test_step)
;;
(*pourquoi ce test : on rentre dans l'altérnance puis la conséquence*)


(*test_structurel_final_newstep_1*)

let test_structurel_final_newstep_1(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_final_newstep_1") in
  let pl : t_play = {par = init_param() ; cur_shape = {base = ref({x = 5 ; y = 10}) ; shape = ref(0) ; color = ref(blue)} ; mat = mat_make(15, 28, white) ; score = ref(0) ; bomb = {bomb = ref(false) ; bomb_pos = ref({x = 0 ; y = 0})}} in
  let test_result : bool t_test_result = test_exec(test_step, final_newstep, pl) in
  assert_false(test_step, "étape non terminé", test_get(test_result)) ;
  test_end(test_step)
;;
(*pourqoi ce test : on vérifie la conséquence*)


(*test_structurel_final_newstep_2*)

let test_structurel_final_newstep_2(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_final_newstep_2") in
  let pl : t_play = {par = init_param() ; cur_shape = {base = ref({x = 5 ; y = 0}) ; shape = ref(0) ; color = ref(blue)} ; mat = mat_make(15, 28, white) ; score = ref(0) ; bomb = {bomb = ref(false) ; bomb_pos = ref({x = 0 ; y = 0})}} in
  for x = 0 to pl.par.mat_szx - 1
  do
    pl.mat.(x).(27) <- blue
  done ;
  let test_result : bool t_test_result = test_exec(test_step, final_newstep, pl) in
  assert_true(test_step, "étape terminé", test_get(test_result)) ;
  test_end(test_step)
;;
(*pourqoi ce test : on vérifie l'altérnance, les fonctions appelés ont déjà été testés*)


(*test_graphique_fill_frame*)

(*
test pour base_draw : {x = 50 ; y = 50} ;
          mat_col : blue ;
          size_x : 15 ;
          size_y : 28 ;
          dilat : 20 ;
résultat attendu : zone remplie de carrés remplies bleues avec tracé noir de taille horizontale 15 et verticale 28.
résultat obtenu : comme attendu.
test ok ;
pourquoi ce test : fait appel à sous-fonction drawfill_absolute_pt()*)


(*test_structurel_valid_param_1*)

let test_structurel_valid_param_1(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_valid_param_1") in
  let param : t_param = {
    time = {init = 1.0 ; extent = 10.0 ; ratio = 0.8} ;
    mat_szx = -1; mat_szy = 28 ;
    graphics = {base = {x = 50 ; y = 50} ; dilat = 20 ; color_arr = init_color() ; mat_col = white} ;
    shapes = init_shapes()
    } in
  let test_result : bool t_test_result = test_exec(test_step, valid_param, param) in
  assert_false(test_step, "taille matrix invalide", test_get(test_result)) ;
  test_end(test_step)
;;
(*pourquoi ce test : on rentre dans la première conséquence*)


(*test_structurel_valid_param_2*)

let test_structurel_valid_param_2(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_valid_param_2") in
  let param : t_param = {
    time = {init = 1.0 ; extent = 10.0 ; ratio = 0.8} ;
    mat_szx = 15; mat_szy = 28 ;
    graphics = {base = {x = 50 ; y = 50} ; dilat = -20 ; color_arr = init_color() ; mat_col = white} ;
    shapes = init_shapes()
    } in
  let test_result : bool t_test_result = test_exec(test_step, valid_param, param) in
  assert_false(test_step, "taille dilat invalide", test_get(test_result)) ;
  test_end(test_step)
;;
(*pourquoi ce test : on rentre dans la deuxième conséquence*)


(*test_structurel_valid_param_3*)

let test_structurel_valid_param_3(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_valid_param_3") in
  let param : t_param = {
    time = {init = 1.0 ; extent = 10.0 ; ratio = 0.8} ;
    mat_szx = 15; mat_szy = 28 ;
    graphics = {base = {x = -50 ; y = 50} ; dilat = 20 ; color_arr = init_color() ; mat_col = white} ;
    shapes = init_shapes()
    } in
  let test_result : bool t_test_result = test_exec(test_step, valid_param, param) in
  assert_false(test_step, "position base invalide", test_get(test_result)) ;
  test_end(test_step)
;;
(*pourquoi ce test : on rentre dans la troisième conséquence*)


(*test_structurel_valid_param_4*)

let test_structurel_valid_param_4(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_valid_param_4") in
  let param : t_param = {
    time = {init = -1.0 ; extent = 10.0 ; ratio = 0.8} ;
    mat_szx = 15; mat_szy = 28 ;
    graphics = {base = {x = 50 ; y = 50} ; dilat = 20 ; color_arr = init_color() ; mat_col = white} ;
    shapes = init_shapes()
    } in
  let test_result : bool t_test_result = test_exec(test_step, valid_param, param) in
  assert_false(test_step, "position base invalide", test_get(test_result)) ;
  test_end(test_step)
;;
(*pourquoi ce test : on rentre dans la quatrième conséquence*)


(*test_structurel_valid_param_5*)

let test_structurel_valid_param_5(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_valid_param_5") in
  let param : t_param = {
    time = {init = 1.0 ; extent = 10.0 ; ratio = 0.8} ;
    mat_szx = 15; mat_szy = 28 ;
    graphics = {base = {x = 50 ; y = 50} ; dilat = 20 ; color_arr = init_color() ; mat_col = white} ;
    shapes = init_shapes()
    } in
  let test_result : bool t_test_result = test_exec(test_step, valid_param, param) in
  assert_true(test_step, "paramètres valides", test_get(test_result)) ;
  test_end(test_step)
;;
(*pourquoi ce test : on rentre dans dernière alternance*)


(*test_graphique_score_display*)

(*
test pour s : 10 ;
avec open_graph(900, 900) ;
résultat attendu : rectangle jaune de tracé noir à la position graphique x = 500, y = 230 avec text score de taille 10 et s de taille 3 en dessous.
résultat obtenu :/
test incomplet ;
pourqoi ce test : fonction fait appel à des fonctions graphiques : set_color(), fill_rect(), draw_rect(), moveto(), set_text_size(), draw_string().
 *)


(*test_structurel_highest_y_1*)

let test_structurel_highest_y_1(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_highest_y_1") in
  let pl : t_play = {par = init_param() ; cur_shape = {base = ref({x = 5 ; y = 10}) ; shape = ref(0) ; color = ref(blue)} ; mat = mat_make(15, 28, white) ; score = ref(0) ; bomb = {bomb = ref(false) ; bomb_pos = ref({x = 0 ; y = 0})}} in
  pl.mat.(0).(5) <- blue ;
  let test_result : int t_test_result = test_exec(test_step, highest_y, pl) in
  assert_equals(test_step, "point y le plus haut", test_get(test_result), 5) ;
  test_end(test_step)
;;
(*pourquoi ce test : on rentre dans les deux boucles itératives puis on passe plusieurs fois dans l'altérnance et finalement la conséquence*)


(*test_structurel_highest_y_2*)

let test_structurel_highest_y_2(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_highest_y_2") in
  let param : t_param = {
    time = {init = -1.0 ; extent = 10.0 ; ratio = 0.8} ;
    mat_szx = 15; mat_szy = 0 ;
    graphics = {base = {x = 50 ; y = 50} ; dilat = 20 ; color_arr = init_color() ; mat_col = white} ; shapes = init_shapes()
    } in
  let pl : t_play = {par = param ; cur_shape = {base = ref({x = 5 ; y = 10}) ; shape = ref(0) ; color = ref(blue)} ; mat = mat_make(15, 28, white) ; score = ref(0) ; bomb = {bomb = ref(false) ; bomb_pos = ref({x = 0 ; y = 0})}} in
  let test_result : int t_test_result = test_exec(test_step, highest_y, pl) in
  assert_equals(test_step, "aucun point", test_get(test_result), 0) ;
  test_end(test_step)
;;
(*pourquoi ce test : on ne rentre pas dans les boucles itératives*)


(*test pour bomb_pos impossible car valeurs retournés pour pl.bomb_pos subit randomisation*)


(*test_graphique_bomb_1*)

(*
test pour pl : t_play = {par = param ; cur_shape = {base = ref({x = 5 ; y = 10}) ; shape = ref(0) ; color = ref(blue)} ; mat = mat_make(15, 28, white) ; score = ref(0) ; bomb = {bomb = ref(false) ; bomb_pos = ref({x = 0 ; y = 0})}} ;
avec open_graph(900, 900) ;
     draw_frame({x = 50 ; y = 50}, 15, 28 , 20) ;
résultat attendu : carré noir se trouvant dans le cadre de la fenêtre graphique.
résultat obtenu : comme attendu.
test ok ;
pourquoi ce test : fait appel a la fonction fill_absolute_pt().
 *)


(*test_structurel_bomb_1*)

(*let test_structurel_bomb_1(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_bomb_1") in
  let pl : t_play = {par = init_param() ; cur_shape = {base = ref({x = 5 ; y = 10}) ; shape = ref(0) ; color = ref(blue)} ; mat = mat_make(15, 28, white) ; score = ref(0) ; bomb = {bomb = ref(false) ; bomb_pos = ref({x = 0 ; y = 0})}} in
  pl.mat(0).(0) <- black ;
  test_exec(test_step, bomb, pl) ;
  assert_equals(test_step, "bomb deja existante", pl.mat.(0).(0), black) ;
  test_end(test_step)
;;*)
(*pourquoi ce test : on rentre dans première conséquence*)


(*test_structurel_bomb_2*)

(*let test_structurel_bomb_2(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_bomb_2") in
  let pl : t_play = {par = init_param() ; cur_shape = {base = ref({x = 5 ; y = 10}) ; shape = ref(0) ; color = ref(blue)} ; mat = mat_make(15, 28, white) ; score = ref(0) ; bomb = {bomb = ref(false) ; bomb_pos = ref({x = 0 ; y = 0})}} in
  test_exec(test_step, bomb, pl) ;
  assert_equals(test_step, "bomb crée", pl.mat.(0).(0), black) ;
  test_end(test_step)
;;*)
(*pourquoi ce test : on rentre dans deuxième conséquence*)


(*test_structurel_bomb_3*)

(*let test_structurel_bomb_3(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_bomb_3") in
  let pl : t_play = {par = init_param() ; cur_shape = {base = ref({x = 5 ; y = 10}) ; shape = ref(0) ; color = ref(blue)} ; mat = mat_make(15, 28, white) ; score = ref(0) ; bomb = {bomb = ref(false) ; bomb_pos = ref({x = 0 ; y = 0})}} in
  test_exec(test_step, bomb, pl) ;
  assert_equals(test_step, "bomb non crée", pl.mat.(0).(0), black) ;
  test_end(test_step)
;;*)
(*pourquoi ce test : on rentre dans deuxième conséquence*)


(*test_graphique_explosion*)

(*
test pour pl : t_play = {par = init_param() ; cur_shape = {base = ref({x = 5 ; y = 10}) ; shape = ref(0) ; color = ref(blue)} ; mat = mat_make(15, 28, white) ; score = ref(0) ; bomb = {bomb = ref(false) ; bomb_pos = ref({x = 0 ; y = 0})}} ;
avec open_graph(900, 900) ;
     draw_frame({x = 50 ; y = 50}, 15, 28 , 20) ;
     fill_frame({x = 50 ; y = 50}, blue, 15, 28 , 20) ;
résultat attendu : zone de couleur blanche de taille 5 sur 5 dans cadre.
résultat obtenu : comme attendu.
test ok ;
pourquoi ce test : fonction fait appel à fill_absolute_pt(). 
 *)


(*test_structurel_explosion_1*)

let test_structurel_explosion_1(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_explosion_1") in
  let pl : t_play = {par = init_param() ; cur_shape = {base = ref({x = 5 ; y = 10}) ; shape = ref(0) ; color = ref(blue)} ; mat = mat_make(15, 28, blue) ; score = ref(0) ; bomb = {bomb = ref(true) ; bomb_pos = ref({x = 0 ; y = 0})}} in
  let count : int ref = ref 0 in
  test_exec(test_step, explosion, pl) ;
  for x = 0 to pl.par.mat_szx - 1
  do
    for y = 0 to pl.par.mat_szy - 1
    do
      if pl.mat.(x).(y) <> blue
      then count := !count + 1
      else ()
    done
  done ;
  assert_false(test_step, "bomb n'existe plus", !(pl.bomb.bomb)) ;
  assert_equals(test_step, "explosion a eu lieu", !count, 25) ;
  test_end(test_step)
;;
(*pourqoi ce test : rentre dans la conséquence*)
  

(*test_structurel_explosion_2*)

let test_structurel_explosion_2(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_explosion_1") in
  let pl : t_play = {par = init_param() ; cur_shape = {base = ref({x = 5 ; y = 10}) ; shape = ref(0) ; color = ref(blue)} ; mat = mat_make(15, 28, blue) ; score = ref(0) ; bomb = {bomb = ref(false) ; bomb_pos = ref({x = 0 ; y = 0})}} in
  let count : int ref = ref 0 in
  test_exec(test_step, explosion, pl) ;
  for x = 0 to pl.par.mat_szx - 1
  do
    for y = 0 to pl.par.mat_szy - 1
    do
      if pl.mat.(x).(y) <> blue
      then count := !count + 1
      else ()
    done
  done ;
  assert_equals(test_step, "explosion n'a pas eu lieu", !count, 0) ;
  test_end(test_step)
;;
(*pourqoi ce test : rentre dans la conséquence*)


let test_run() : unit =
  let status : t_test_status = create_test_status() in
  open_graph(100, 100) ;
  test_structurel_color_choice(status) ;
  test_structurel_cur_shape_choice(status) ;
  test_structurel_insert_1(status) ;
  test_structurel_insert_2(status) ;
  test_structurel_init_play(status) ;
  test_structurel_valid_matrix_point_1(status) ;
  test_structurel_valid_matrix_point_2(status) ;
  test_structurel_is_free_move_1(status) ;
  test_structurel_is_free_move_2(status) ;
  test_structurel_is_free_move_3(status) ;
  test_structurel_is_free_move_4(status) ;
  test_structurel_move_left_1(status) ;
  test_structurel_move_left_2(status) ;
  test_structurel_move_right_1(status) ;
  test_structurel_move_right_2(status) ;
  test_structurel_move_down_1(status) ;
  test_structurel_move_down_2(status) ;
  test_structurel_rotate_right_1(status) ;
  test_structurel_rotate_right_2(status) ;
  test_structurel_rotate_left_1(status) ;
  test_structurel_rotate_left_2(status) ;
  test_structurel_move_at_bottom(status) ;
  test_structurel_is_column_full_1(status) ;
  test_structurel_is_column_full_2(status) ;
  test_structurel_decal_1(status) ;
  test_structurel_decal_2(status) ;
  test_structurel_clear_play_1(status) ;
  test_structurel_clear_play_2(status) ;
  test_structurel_final_insert(status) ;
  test_structurel_final_newstep_1(status) ;
  test_structurel_final_newstep_2(status) ;
  test_structurel_valid_param_1(status) ;
  test_structurel_valid_param_2(status) ;
  test_structurel_valid_param_3(status) ;
  test_structurel_valid_param_4(status) ;
  test_structurel_valid_param_5(status) ;
  test_structurel_highest_y_1(status) ;
  test_structurel_highest_y_2(status) ;
  test_structurel_explosion_1(status) ;
  test_structurel_explosion_2(status) ;
  close_graph() ;
  print_test_report(status) ;
;;

test_run() ;;
close_graph() ;;
