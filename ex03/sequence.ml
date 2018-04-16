let rec  count_cons ensemble count lst_elem =
       match ensemble with
    |[] -> lst_elem
    |tete::suite::queue -> (
                if tete = suite
                    then count_cons (suite::queue) (count + 1) lst_elem
                else count_cons (suite::queue) 1 (lst_elem @ [ count; tete])
                )
    |tete::queue -> count_cons queue 1 (lst_elem @ [count; tete])
        
let encode ensemble =
    count_cons ensemble 1 []

let rec rec_encode ensemble n =
    if n < 2
    then ensemble
    else rec_encode (encode ensemble) (n-1)

let rec string_list_int ensemble str =
        match ensemble with
        |[] -> str
        |head::tail -> string_list_int tail (str ^ (string_of_int head))

let sequence n = 
    if n <= 0 
        then ""
    else string_list_int (rec_encode [1] n) ""

let () =
    print_endline(sequence 1);
    print_endline(sequence 2);
    print_endline(sequence 3);
    print_endline(sequence 4);
    print_endline(sequence 5);
    print_endline(sequence 6);
    print_endline(sequence (-1));
    print_endline(sequence 0)
