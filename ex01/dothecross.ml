let rec test ensemble = match ensemble with
    |[] -> print_endline "end"
        |head::tail-> (print_char head; print_char '\n'; test tail)

        let () =

            test (crossover ['a';'d';'c';'r'] ['w';'r';'d']);
            test (crossover ['d';'c';'r']['w';'r';'d']);
            test (crossover ['a';'d';'c';'r'] []);
            test (crossover [] ['w';'r';'d'])
