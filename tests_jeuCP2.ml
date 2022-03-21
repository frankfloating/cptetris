open CPutil ;;
open JeuCP2 ;;

(*test_structurel_draw_absolute_pt*)

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


(*test_structurel_fill_absolute_pt*)

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


(*test_structurel_drawfill_absolute_pt*)

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


(*test_structurel_draw_relative_pt*)

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


(*test_structurel_fill_relative_pt*)

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


(*test_structurel_drawfill_relative_pt*)

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


(*test_structurel_draw_pt_list_1*)

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


(*test_structurel_draw_pt_list_2*)

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


(*test_structurel_fill_pt_list_1*)

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


(*test_structurel_fill_pt_list_2*)

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


(*test_structurel_drawfill_pt_list_1*)

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


(*test_structurel_drawfill_pt_list_2*)

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


(*test_structurel_drawframe_1*)

(*
test pour base_draw : {x = 50 ; y = 50}
          size_x : 15
          size_y : 28
          dilat : 20 ;
résulat attendu : cadre de carrés remplies en noire de longueurs (dilat - 1) entourant zone d'affichage d'épaisseur de 2 carrés.
résulat obtenu : comme attendu.
test ok ;
pourquoi ce test : la fonction fait appel à la fonction drawfill_absolute_pt() dans 3 boucles itératives différentes, on rentre donc dans chaques.
 *)


(*test_structurel_drawframe_2*)

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
    if (a = col_arr.len - 1) && (test_get(test_result) <> grey)
    then assert_notequals(test_step, "color does not exists", test_get(test_result), col_arr.value.(a))
    else
      if col_arr.value.(a) = test_get(test_result)
      then assert_equals(test_step, "color exists", test_get(test_result), col_arr.value.(a))
      else ()
  done ;
  test_end(test_step)
;;
(*pourquoi ce test : on veut verifier que l'appel de la fonction color_choice nous rend une couleur définie.*)


(*test_structurel_cur_shape_choice*)

let test_structurel_cur_shape_choice (status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_structurel_cur_shape_choice") in
  let shape_arr : t_shape t_array = init_shapes() and col_arr : t_color t_array = init_color() in
  let test_result : t_cur_shape t_test_result = test_exec(test_step, cur_shape_choice, (shape_arr, 0, 0, col_arr)) in
  (
  if(test_get(test_result).shape < 0) || (test_get(test_result).shape > shape_arr.len - 1)
  then assert_notequals(test_step, "invalid shape", test_get(test_result).shape, shape_arr.len - 1)
  else assert_true(test_step, "valid shape", test_is_success(test_result))
  ) ;
  if (a = col_arr.len - 1) && (test_get(test_result).color <> grey)
    then assert_notequals(test_step, "color does not exists", test_get(test_result).color, col_arr.value.(a))
    else
      if col_arr.value.(a) = test_get(test_result)
      then assert_equals(test_step, "color exists", test_get(test_result).color, col_arr.value.(a))
      else ()
done ;
test_end(test_step)
;;
(*pourquoi ce test : on veut verifier que l'appel de la fonction cur_shape_choice rend une forme et couleur valide*)



  
