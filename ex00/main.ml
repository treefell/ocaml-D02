
let get_1_2 (a,_) = a ;;
let get_2_2 (_,a) = a ;;

let () = 
    let rec test ensemble = match ensemble with
    |[] -> print_endline "done"
    |head::tail -> ( print_int (get_1_2 head); print_char (get_2_2 head); print_char '\n'); test tail

    in
    test (Encode.encode []);
    test (Encode.encode ['a';'c';'c';'c';'c';'a';'a';'a'])
