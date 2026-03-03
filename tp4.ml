(*
COQUARD-MOREL Lou-Ann L2-B

                TP3 NONOGRAM PROGRAMMATION FONCTIONELLE
 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
*)

(*Moves qui remplissent l'array solution (au début vide)*)
type move = Empty | X

(*Mon board composé d'un nombre de colonnes, de lignes, d'un tableau de moves
attendus pour les colonnes, d'un tableau de move attendus pour les lignes et
d'un tableau pour la solution*)
type board = {
  mutable c: int;
  mutable l: int;
  mutable c_moves: int array array;
  mutable l_moves: int array array;
  mutable data: move array array
}

let int_array_of_string s : int array =
  let string_list = String.split_on_char ' ' s in
  let filtered_list = List.filter (fun x -> x <> "") string_list in
  let int_list = List.map int_of_string filtered_list in
  Array.of_list int_list

let string_of_int_array t : string =
  let string_list = Array.to_list t |> List.map string_of_int in
  String.concat " " string_list

let string_of_move m =
  match m with
  | X -> "X"
  | Empty -> "."

let move_of_string s =
  match s with
  | "X" -> X
  | _ -> Empty

(*print mon board data*)
let print_board b =
  Array.iter (fun row ->
    Array.iter (fun cell -> print_string (string_of_move cell)) row;
    print_newline ()
  ) b.data

(*Remet en forme les informations d'une ligne pour les utiliser*)
let clean_string s =
  let nouv = String.map (fun c ->
    match c with
      | '\r' | 'L' | 'C' -> ' '
      | ':' -> ';'
      | _ -> c) s in
    String.trim nouv

(*Fonction vu en cour qui vient lire une ligne*)
let read_lines (filename: string) =
  let ic = open_in filename in
  let s1 = try input_line ic with End_of_file -> "" in
  let s2 = try input_line ic with End_of_file -> "" in
  let s3 = try input_line ic with End_of_file -> "" in
  close_in ic;
  (clean_string s1, clean_string s2, clean_string s3)

(*fonction d'initialisation*)
let init_board (filename: string) : board =
  (*lignes*)
  let (s1, s2, s3) = read_lines filename in
  let l_moves =
    let parts = String.split_on_char ';' s2 in
    let map_rows row =
      let items = String.split_on_char ' ' row in
      let filtered = List.filter (fun x -> x <> "") items in
      let integers = List.map int_of_string filtered in
      Array.of_list integers
    in
    Array.of_list (List.map map_rows parts)
  in
  (*colonnes*)
  let c_moves =
    let parts = String.split_on_char ';' s3 in
    let map_cols col =
      let items = String.split_on_char ' ' col in
      let filtered = List.filter (fun x -> x <> "") items in
      let integers = List.map int_of_string filtered in
      Array.of_list integers
    in
    Array.of_list (List.map map_cols parts)
  in
  (*dimensions*)
  let dimensions = String.split_on_char ' ' s1 in
  if List.length dimensions <> 2 then failwith "mauvaises dimensions";
  let c = int_of_string (List.hd dimensions) in
  (*mon board*)
  let l = int_of_string (List.hd (List.tl dimensions)) in
  {
    c;
    l;
    c_moves;
    l_moves;
    data = Array.make_matrix l c Empty;
  }

let test_full_line (b: board) (i: int) =
  let l_num = Array.fold_left (+) 0 b.l_moves.(i) in
  let rec count_x j x_count =
    if j = b.c then x_count = l_num
    else if b.data.(i).(j) = X then count_x (j + 1) (x_count + 1)
    else count_x (j + 1) x_count
  in
  count_x 0 0

let test_full_coll (b: board) (i: int) =
  let c_num = Array.fold_left (+) 0 b.c_moves.(i) in
  let rec count_x j x_count =
    if j = b.c then x_count = c_num
    else if b.data.(j).(i) = X then count_x (j + 1) (x_count + 1)
    else count_x (j + 1) x_count
  in
  count_x 0 0

let test_full_board (b: board) =
  let rec check_lines i =
    if i = b.l then true
    else if not (test_full_line b i) then false
    else check_lines (i + 1)
  in
  let rec check_cols i =
    if i = b.c then true
    else if not (test_full_coll b i) then false
    else check_cols (i + 1)
  in
  check_lines 0 && check_cols 0

let find_first_x (b: board) (i: int) =
  let row = b.data.(i) in
  let rec find j =
    if j < 5 then
      if row.(j) = X then j
      else find (j + 1)
    else -1
  in
  find 0

let find_last_x (b: board) (i: int) =
  let row = b.data.(i) in
  let rec find j =
    if j >= 0 then
      if row.(j) = X then j
      else find (j - 1)
    else -1
  in
  find 4

let find_first_x_col (b: board) (col_idx: int) =
  let rec find row_idx =
    if row_idx < 5 then
      if b.data.(row_idx).(col_idx) = X then row_idx
      else find (row_idx + 1)
    else -1
  in
  find 0

let find_last_x_col (b: board) (col_idx: int) =
  let rec find row_idx =
    if row_idx >= 0 then
      if b.data.(row_idx).(col_idx) = X then row_idx
      else find (row_idx - 1)
    else -1
  in
  find 4

(*check les colonnes d'une ligne*)
let check_full_but_one (b:board) =
  let rec check s final_i j =
    if j=5 then (
      if s=4 then final_i
      else 6
    )
    else if test_full_coll b j then check (s+1) final_i (j+1)
    else check s j (j+1)
  in check 0 6 0

(*check les lignes d'une colonne*)
let check_full_but_one_2 (b: board) =
  let rec check s final_i j =
    if j = 5 then (
      if s = 4 then final_i
      else 6
    )
    else if test_full_line b j then check (s + 1) final_i (j + 1)
    else check s j (j + 1)
  in
  check 0 6 0

(*_ _ _ _ _ _ _ _ _ __ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _*)

(*rule 1 sert à remplir les lignes complètes pré déterminées*)
let rule1 (b:board) =
  let rec r1 b i =
    if i = b.l then b
    else
      let target_moves = Array.to_list b.l_moves.(i) in
      if target_moves = [1; 1; 1] then (
        b.data.(i) <- [| X; Empty; X; Empty; X |];
        r1 b (i + 1)
      )
      else if target_moves = [3; 1] then (
        b.data.(i) <- [| X; X; X; Empty; X |];
        r1 b (i + 1)
      )
      else if target_moves = [1; 3] then (
        b.data.(i) <- [| X; Empty; X; X; X |];
        r1 b (i + 1)
      )
      else if target_moves = [2; 2] then (
        b.data.(i) <- [| X; X; Empty; X; X |];
        r1 b (i + 1)
      )
      else if target_moves = [5] then (
        b.data.(i) <- [| X; X; X; X; X |];
        r1 b (i + 1)
      )
      else
        r1 b (i + 1)
  in
  r1 b 0

(*rule 2 sert à remplir les colonnes complètes pré déterminées*)
let rule2 (b:board) =
  let rec fill_column col_idx target_moves row_idx =
    if row_idx = b.l then ()
    else
      match target_moves with
      | [1; 1; 1] ->
          b.data.(row_idx).(col_idx) <- if row_idx mod 2 = 0 then X else Empty;
          fill_column col_idx target_moves (row_idx + 1)
      | [3; 1] ->
          b.data.(row_idx).(col_idx) <-
            if row_idx < 3 then X
            else if row_idx = 3 then Empty
            else X;
          fill_column col_idx target_moves (row_idx + 1)
      | [1; 3] ->
          b.data.(row_idx).(col_idx) <-
            if row_idx = 0 then X
            else if row_idx = 1 then Empty
            else X;
          fill_column col_idx target_moves (row_idx + 1)
      | [2; 2] ->
          b.data.(row_idx).(col_idx) <-
            if row_idx < 2 then X
            else if row_idx = 2 then Empty
            else X;
          fill_column col_idx target_moves (row_idx + 1)
      | [5] ->
          b.data.(row_idx).(col_idx) <- X;
          fill_column col_idx target_moves (row_idx + 1)
      | _ -> ()
  in
  let rec process_columns col_idx =
    if col_idx = b.c then ()
    else
      let target_moves = Array.to_list b.c_moves.(col_idx) in
      fill_column col_idx target_moves 0;
      process_columns (col_idx + 1)
  in
  process_columns 0;
  b

(*rule 3 sert à remplir les cases du milieu lorsque c'est le nombre 3 ou 4 qui est demandé pour les lignes*)
let rule3 (b:board) =
  let rec r1 b i =
    if i = b.l then b
    else
      let target_moves = Array.to_list b.l_moves.(i) in
      if target_moves = [3] then (
        b.data.(i) <- [| Empty; Empty; X; Empty; Empty|];
        r1 b (i + 1)
      )
      else if target_moves = [4] then (
        b.data.(i) <- [| Empty; X; X; X; Empty |];
        r1 b (i + 1)
      )
      else
        r1 b (i + 1)
  in
  r1 b 0

(*rule 4 sert à remplir les cases du milieu lorsque c'est le nombre 3 ou 4 qui est demandé pour les colonnes*)
let rule4 (b:board) =
  let rec fill_column col_idx target_moves row_idx =
    if row_idx = b.l then ()
    else
      match target_moves with
      | [3] ->
          b.data.(row_idx).(col_idx) <- if row_idx=2 then X else Empty;
          fill_column col_idx target_moves (row_idx + 1)
      | [4] ->
          b.data.(row_idx).(col_idx) <- if row_idx=1 || row_idx=2 || row_idx=3 then X else Empty;
          fill_column col_idx target_moves (row_idx + 1)
      | _ -> ()
  in
  let rec process_columns col_idx =
    if col_idx = b.c then ()
    else
      let target_moves = Array.to_list b.c_moves.(col_idx) in
      fill_column col_idx target_moves 0;
      process_columns (col_idx + 1)
  in
  process_columns 0;
  b

(*rule 5 sert à compléter automatiquement les cas simple de nonogram pour les lignes*)
let rule5 (b:board) i =
  if test_full_line b i then b
  else
    let l_moves_row = Array.to_list b.l_moves.(i) in
    let row = b.data.(i) in
    if (l_moves_row = [2] || l_moves_row = [3]) && row.(4) = X && not (test_full_line b i) then (
      row.(3) <- X;
      b
    )
    else if (l_moves_row = [2] || l_moves_row = [3]) && row.(0) = X && not (test_full_line b i) then (
      row.(1) <- X;
      b
    )
    else if (l_moves_row = [2] && (i!=0 && i!=4) && not(test_full_coll b (i-1)) && (test_full_coll b (i+1))) then (
      row.(i-1) <- X;
      b
    )
    else if (l_moves_row = [2] && (i!=0 && i!=4) && (test_full_coll b (i-1)) && not(test_full_coll b (i+1))) then (
      row.(i+1) <- X;
      b
    )
    else if l_moves_row = [1; 1] && (row.(1) = X || row.(3) = X) && not (test_full_line b i) then (
      row.(1) <- X;
      row.(3) <- X;
      b
    )
    else if l_moves_row = [2; 1] && (row.(1) = X) && (row.(2) = X) && not (test_full_line b i) then (
      row.(4) <- X;
      b
    )
    else if l_moves_row = [2; 1] && (row.(3) = X) && not (test_full_line b i) then (
      row.(0) <- X;
      row.(1) <- X;
      b
    )
    else if l_moves_row = [1; 2] && (row.(2) = X) && (row.(3) = X) && not (test_full_line b i) then (
      row.(0) <- X;
      b
    )
    else if l_moves_row = [1; 2] && (row.(1) = X) && not (test_full_line b i) then (
      row.(3) <- X;
      row.(4) <- X;
      b
    )
    else b

(*rule 6 sert à compléter automatiquement les cas simple de nonogram pour les colonnes*)
let rule6 (b:board) i =
  if test_full_coll b i then b
  else
    let c_moves_col = Array.to_list b.c_moves.(i) in
    let col = Array.init b.l (fun j -> b.data.(j).(i)) in  (* Récupère la colonne i du tableau *)
    if (c_moves_col = [2] || c_moves_col = [3]) && col.(4) = X && not (test_full_coll b i) then (
      b.data.(3).(i) <- X;
      b
    )
    else if (c_moves_col = [2] || c_moves_col = [3]) && col.(0) = X && not (test_full_coll b i) then (
      b.data.(1).(i) <- X;
      b
    )
    else if (c_moves_col = [2] && (i!=0 && i!=4) && not(test_full_line b (i-1)) && (test_full_line b (i+1))) then (
      b.data.(i-1).(i) <- X;
      b
    )
    else if (c_moves_col = [2] && (i!=0 && i!=4) && (test_full_line b (i-1)) && not(test_full_line b (i+1))) then (
      b.data.(i+1).(i) <- X;
      b
    )
    else if c_moves_col = [1; 1] && (col.(1) = X || col.(3) = X) && not (test_full_coll b i) then (
      b.data.(1).(i) <- X;
      b.data.(3).(i) <- X;
      b
    )
    else if c_moves_col = [2; 1] && (col.(1) = X) && (col.(2) = X) && not (test_full_coll b i) then (
      b.data.(4).(i) <- X;
      b
    )
    else if c_moves_col = [2; 1] && (col.(3) = X) && not (test_full_coll b i) then (
      b.data.(0).(i) <- X;
      b.data.(1).(i) <- X;
      b
    )
    else if c_moves_col = [1; 2] && (col.(2) = X) && (col.(3) = X) && not (test_full_coll b i) then (
      b.data.(0).(i) <- X;
      b
    )
    else if c_moves_col = [1; 2] && (col.(1) = X) && not (test_full_coll b i) then (
      b.data.(3).(i) <- X;
      b.data.(4).(i) <- X;
      b
    )
    else b

(*rule 7 sert à compléter les lignes de X en fonction de ce qui est présent dans
le board sachant qu'il aura déjà traité toutes les exceptions des rules précédentes :*)
let rule7 (b: board) i =
  if test_full_line b i then b
  else
    let l_moves_row = Array.to_list b.l_moves.(i) in
    let row = b.data.(i) in
    if (l_moves_row = [3] || l_moves_row = [4] || l_moves_row = [2]) then
      let first_x = find_first_x b i in
      let last_x = find_last_x b i in
      if first_x = -1 || last_x = -1 then b (* Aucun X trouvé *)
      else
        if (not(test_full_coll b (first_x - 1))) && (not(test_full_coll b (last_x + 1))) then b
        else if (not(test_full_coll b (first_x - 1))) && test_full_coll b (last_x + 1) then (
          row.(first_x - 1) <- X;
          b
        )
        else if test_full_coll b (first_x - 1) && not (test_full_coll b (last_x + 1)) then (
          row.(last_x + 1) <- X;
          b
        )
        else b
    else if l_moves_row = [1] then (
      if (check_full_but_one b)=6 then b
      else
        let idx = check_full_but_one b in
        row.(idx) <- X;
        b
    )
    else if l_moves_row = [1;1] then (
      let first_x = find_first_x b i in
      if first_x = -1 then b
      else
        if (first_x = 2) then (
          if (not(test_full_coll b 0)) && (not(test_full_coll b 4)) then b
          else if (not(test_full_coll b 0)) && (test_full_coll b 4) then (
            row.(0) <- X;
            b
          )
          else if (test_full_coll b 0) && (not(test_full_coll b 4)) then (
            row.(4) <- X;
            b
          )
          else b
        )
        else
          if (check_full_but_one b)=6 then b
          else
            let idx = check_full_but_one b in
            row.(idx) <- X;
            b
    )
    else if l_moves_row = [2;1] then (
      row.(1) <- X;
      if (test_full_line b i) then b
      else
        if (row.(0) = Empty) && (row.(2) = Empty) then (
          if (test_full_coll b 0) && not(test_full_coll b 2) then(
            row.(2) <- X;
            b
          )
          else if not(test_full_coll b 0) && (test_full_coll b 2) then(
            row.(0) <- X;
            b
          )
          else b
        )
        else if (row.(3) = Empty) && (row.(4) = Empty) then (
          if (test_full_coll b 3) && not(test_full_coll b 4) then(
            row.(4) <- X;
            b
          )
          else if not(test_full_coll b 3) && (test_full_coll b 4) then(
            row.(3) <- X;
            b
          )
          else b
        )
        else b
    )
    else if l_moves_row = [1;2] then (
      row.(3) <- X;
      if (test_full_line b i) then b
      else
        if (row.(4) = Empty) && (row.(2) = Empty) then (
          if (test_full_coll b 4) && not(test_full_coll b 2) then(
            row.(2) <- X;
            b
          )
          else if not(test_full_coll b 4) && (test_full_coll b 2) then(
            row.(4) <- X;
            b
          )
          else b
        )
        else if (row.(0) = Empty) && (row.(1) = Empty) then (
          if (test_full_coll b 0) && not(test_full_coll b 1) then(
            row.(1) <- X;
            b
          )
          else if not(test_full_coll b 0) && (test_full_coll b 1) then(
            row.(0) <- X;
            b
          )
          else b
        )
        else b
    )
    else b

(*rule 7 sert à compléter les lignes de X en fonction de ce qui est présent dans
le board sachant qu'il aura déjà traité toutes les exceptions des rules précédentes :*)
let rule8 (b: board) col_idx =
  if test_full_coll b col_idx then b
  else
    let c_moves_col = Array.to_list b.c_moves.(col_idx) in
    if c_moves_col = [3] || c_moves_col = [4] || c_moves_col = [2] then
      let first_x = find_first_x_col b col_idx in
      let last_x = find_last_x_col b col_idx in
      if first_x = -1 || last_x = -1 then b
      else
        if (first_x > 0 && b.data.(first_x - 1).(col_idx) = Empty) &&
           (last_x < b.l - 1 && b.data.(last_x + 1).(col_idx) = Empty)
        then b
        else if (first_x > 0 && b.data.(first_x - 1).(col_idx) = Empty) &&
                (last_x < b.l - 1 && b.data.(last_x + 1).(col_idx) = X)
        then (
          b.data.(first_x - 1).(col_idx) <- X;
          b
        )
        else if (first_x > 0 && b.data.(first_x - 1).(col_idx) = X) &&
                (last_x < b.l - 1 && b.data.(last_x + 1).(col_idx) = Empty)
        then (
          b.data.(last_x + 1).(col_idx) <- X;
          b
        )
        else b
    else if c_moves_col = [1] then (
      if check_full_but_one_2 b = 6 then b
      else
        let idx = check_full_but_one_2 b in
        b.data.(idx).(col_idx) <- X;
        b
    )
    else if c_moves_col = [1; 1] then (
      let first_x = find_first_x_col b col_idx in
      if first_x = -1 then b
      else
        if first_x = 2 then (
          if b.data.(0).(col_idx) = Empty && b.data.(4).(col_idx) = Empty then b
          else if b.data.(0).(col_idx) = Empty && b.data.(4).(col_idx) = X then (
            b.data.(0).(col_idx) <- X;
            b
          )
          else if b.data.(0).(col_idx) = X && b.data.(4).(col_idx) = Empty then (
            b.data.(4).(col_idx) <- X;
            b
          )
          else b
        )
        else
          if check_full_but_one_2 b = 6 then b
          else
            let idx = check_full_but_one_2 b in
            b.data.(idx).(col_idx) <- X;
            b
    )
    else if c_moves_col = [2; 1] then (
      b.data.(1).(col_idx) <- X;
      if test_full_coll b col_idx then b
      else
        if b.data.(0).(col_idx) = Empty && b.data.(2).(col_idx) = Empty then (
          if b.data.(0).(col_idx) = Empty && b.data.(2).(col_idx) = X then (
            b.data.(0).(col_idx) <- X;
            b
          )
          else if b.data.(0).(col_idx) = X && b.data.(2).(col_idx) = Empty then (
            b.data.(2).(col_idx) <- X;
            b
          )
          else b
        )
        else if b.data.(3).(col_idx) = Empty && b.data.(4).(col_idx) = Empty then (
          if b.data.(3).(col_idx) = Empty && b.data.(4).(col_idx) = X then (
            b.data.(3).(col_idx) <- X;
            b
          )
          else if b.data.(3).(col_idx) = X && b.data.(4).(col_idx) = Empty then (
            b.data.(4).(col_idx) <- X;
            b
          )
          else b
        )
        else b
    )
    else if c_moves_col = [1; 2] then (
      b.data.(3).(col_idx) <- X;
      if test_full_coll b col_idx then b
      else
        if b.data.(2).(col_idx) = Empty then (
          b.data.(2).(col_idx) <- X;
          b
        )
        else b
    )
    else b

(*_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ *)

(*solution modifie et résoud le board vide avec les rules implémentées*)
let solution (b: board) : board =
  let b = rule3 b in
  let b = rule4 b in
  let b = rule1 b in
  let b = rule2 b in
  let rec apply_rules b j =
    if test_full_board b then b
    else
      let rec apply b i =
        if i >= Array.length b.data then b
        else
          let b =
            if i < Array.length b.data then (
              let b = rule5 b i in
              let b = rule6 b i in
              let b = rule7 b i in
              let b = rule8 b i in
              b
            ) else b
          in
          apply b (i + 1)
      in
      let b = apply b 0 in
      apply_rules b (j + 1)
  in
  apply_rules b 0
(* Main execution *)
let () =
  let b = init_board "nonogram.txt" in
  let solved_board = solution b in
  print_board solved_board