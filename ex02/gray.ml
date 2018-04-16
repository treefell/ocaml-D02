let rec pow_duo x = 
        if x < 1
        then 1
        else 2 * pow_duo (x - 1)

let rec printgrb nb digit=
    if digit > 0
    then match nb with
        |nb when ((((nb+ pow_duo(digit - 1)) / (pow_duo (digit))) mod 2) < 1) -> print_int 0; printgrb nb (digit - 1)
        |_ -> print_int 1;printgrb nb (digit - 1)


let rec printgray nb fin digit=
    if nb <= fin
        then (printgrb nb digit; print_char ' '; printgray (nb + 1) fin digit)
    else print_char '\n'

let gray digit = 
    let count = 0 in
    let finish = (pow_duo digit - 1) in
    if digit > 0
        then printgray count finish digit

let() =
    gray 0;
    gray 3;
    gray 4
