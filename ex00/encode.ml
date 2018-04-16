let rec count_cons ensemble count tuples =
    match ensemble with
    |[] -> tuples
    |tete::suite::queue -> (
        if tete = suite
        then (count_cons (suite::queue) (count + 1) tuples)
        else count_cons (suite::queue) 1 (tuples @ [(count, tete)])
    )
  (*  |tete::queue -> ( if tete = queue
        then count_cons (queue) (n + 1) tuple
        else count_cons (queue) 0 ([(n, tete)] :: tuple)
        )*)

     |tete::queue -> count_cons queue 1 (tuples @ [(count, tete)])

let encode ensemble = 
    count_cons ensemble 1 []

(******************************************************************************)

let get_1_2 (a,_) = a ;;
let get_2_2 (_,a) = a ;;

let () = 
    let rec test ensemble = match ensemble with
    |[] -> print_endline "done"
    |head::tail -> ( print_int (get_1_2 head); print_char (get_2_2 head); print_char '\n'); test tail

    in
    test (encode []);
    test (encode ['a';'c';'c';'c';'c';'a';'a';'a']);
    test (encode ['a';'c';'e';'c';'c';'d'])
