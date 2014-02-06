{
    open Svgpathparser
}

let blank = [' ''\n''\t''\r']

let digit = ['0'-'9']
let unsigned_integer = digit+
let fractional_constant = unsigned_integer? '.' unsigned_integer | unsigned_integer '.'
let exponent_part = ('e' | 'E') ('+' | '-')? unsigned_integer
let unsigned_real = fractional_constant exponent_part? | unsigned_integer exponent_part

rule token = parse
           | blank { token lexbuf }

           | unsigned_real as lxm
                    { NUM (float_of_string lxm) }

           | unsigned_integer as lxm
                    { NUM (float_of_int (int_of_string lxm)) }

           | "," { COMMA }
           | "+" { PLUS }
           | "-" { MINUS }

           | "Q" { Q }
           | "q" { Qrel }
           | "M" { M }
           | "m" { Mrel }
           | "L" { L }
           | "l" { Lrel }
           | "H" { H }
           | "h" { Hrel }
           | "V" { V }
           | "v" { Vrel }
           | "C" { C }
           | "c" { Crel }
           | "S" { S }
           | "s" { Srel }
           | "T" { T }
           | "t" { Trel }
           | "A" { A }
           | "a" { Arel }
           | "Z" { Z }
           | "z" { Z }

           | eof { EOF }

