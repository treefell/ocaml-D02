let rec do_exist head1 l2 = match l2 with
    |[] ->false
    |head2::tail2 -> if head2 = head1
                    then true
                    else
                        do_exist head1 tail2

let rec do_compare l1 l2 l3 = match l1 with
    |[] -> l3
    |head1::tail1 -> if do_exist head1 l2
                        then do_compare tail1 l2 (l3 @ [head1])
                        else do_compare tail1 l2 l3
                                    
let crossover l1 l2 =
    do_compare l1 l2 []


let rec test ensemble = match ensemble with
    |[] -> print_endline " over\n"
    |head::tail-> (print_char head; print_char; test tail)

let () =
            test (crossover ['a';'d';'c';'r'] ['w';'r';'d']);
            test (crossover ['d';'c';'r']['w';'r';'d']);
            test (crossover ['a';'d';'c';'r'] []);
            test (crossover [] ['w';'r';'d']);
            test (crossover ['a';'d';'c';'r'] ['r';'c';'a';'d'])
