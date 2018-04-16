type phosphate = string
type deoxyribose = string
type nucleobase = A | C | G | T | U | None
(*type nucleotide = (phosphate, deoxyribose, nucleobase)

let generate_nucleotide base = ("phosphate", "deoxyribose", base) *)

type nucleotide = {phosphate:phosphate; deoxyribose:deoxyribose; nucleobase: nucleobase}

let generate_nucleotide base =
    {phosphate = "phosphate"; deoxyribose = "deoxyyribose"; nucleobase = base}


let print_nucleobase base = match base with
    |A -> print_char 'A';
    |C -> print_char 'C';
    |T -> print_char 'T';
    |G -> print_char 'G';
    |U -> print_char 'U';
    |None -> print_string "None"

type helix = nucleotide list

let rand_gen_nbase () =
    Random.self_init();
    let  base = (Random.int 4) in
        match base with
        |0 -> A 
        |1 -> C
        |2 -> G
        |3 -> T
        |_ -> None

let  generate_helix n =
    let rec auto_gen_helix n helix = 
        if n < 1
        then helix
        else
            auto_gen_helix (n - 1) (helix @ [generate_nucleotide (rand_gen_nbase())])
    in auto_gen_helix n []

let rec string_of_nucleobase nbase =
        match nbase with
        |A  -> "A"
        |C  -> "C"
        |G  -> "G"
        |T  -> "T"
        |_ -> "None" 

let helix_to_string l_nucleo =
    let rec rec_hel_to_string l_nucleo str =
        match l_nucleo with
            |[] -> str
            |head::tail ->  rec_hel_to_string tail (str ^ (string_of_nucleobase head.nucleobase))
        in
        rec_hel_to_string l_nucleo ""


let comp_nucleo nbase =
    match nbase with
            |A  -> T
            |C  -> G
            |G  -> C
            |T  -> A
            |_ -> None

let rec rec_comp_helix l_helix l_comp =
    match l_helix with
            |[] -> l_comp
            |head::tail -> rec_comp_helix tail (l_comp @ [generate_nucleotide (comp_nucleo head.nucleobase)])

let complimentary_helix l_helix =
    rec_comp_helix l_helix []

type rna = nucleobase list 

let rec rec_comp_rna l_helix l_comp =
    match l_helix with
            |[] -> l_comp
            |head::tail -> if (comp_nucleo head.nucleobase) = T
                            then rec_comp_rna tail (l_comp @ [U])
                            else rec_comp_rna tail (l_comp @ [(comp_nucleo head.nucleobase)])

let generate_rna l_helix =
    rec_comp_rna l_helix []

let rec print_nucleo  g = 
    match g with
    |head::tail -> print_nucleobase head; print_nucleo tail
    |[]-> print_char '\n'


let () =
    let f_helix = generate_helix 20 in
    let c_helix = (complimentary_helix f_helix) in
    print_string (helix_to_string f_helix);
    print_char '\n';
    print_string (helix_to_string c_helix);
    print_char '\n';
    print_nucleo (generate_rna f_helix)
