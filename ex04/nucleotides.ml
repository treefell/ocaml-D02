type phosphate = string
type deoxyribose = string
type nucleobase = A | C | T | G | None
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
    |None -> print_string "None"

let () =
    let t = generate_nucleotide T in 

    let e = generate_nucleotide None in
    let a = generate_nucleotide A in
    let c = generate_nucleotide C in
    print_string "T :"; print_string t.phosphate;print_char ' ';
    print_string t.deoxyribose; print_char ' '; print_nucleobase t.nucleobase;print_char '\n';
    print_string "None :"; print_string e.phosphate;print_char ' ';
    print_string e.deoxyribose; print_char ' ';
    print_nucleobase e.nucleobase;print_char '\n';
    print_string "A :";print_string a.phosphate;print_char ' ';
    print_string a.deoxyribose; print_char ' '; print_nucleobase a.nucleobase;print_char '\n';
    print_string "C :"; print_string c.phosphate;print_char ' ';
    print_string c.deoxyribose; print_char ' '; print_nucleobase c.nucleobase; print_char '\n'
