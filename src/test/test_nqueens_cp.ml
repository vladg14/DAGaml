let start = Sys.time() ;;

let n = (int_of_string Sys.argv.(1));;

module T = Cp.TACX;;

let man = T.newman ();;

let ( *! ) : T.edge -> T.edge -> T.edge
    = Cp.( *! ) man
and ( &! ) : T.edge -> T.edge -> T.edge
    = Cp.( &! ) man
and ( ^! ) : T.edge -> T.edge -> T.edge
    = Cp.( ^! ) man;;

let no = Cp.no;;

let var_n : T.edge array
    = Oops.array_make_n_var man n;;

let ue_var_n : T.edge = Oops.vect_exactly_one (&!) n var_n;;

let rows : T.edge array
    = Oops.copy_fun_t n ue_var_n;;
let cols : T.edge array
    = Oops.copy_fun n ue_var_n;;

let et_cols : T.edge = Oops.vect_et (&!) cols;;
let et_rows : T.edge = Oops.vect_et (&!) rows;;
let n_tower : T.edge = et_cols &! et_rows;;

let liste = ref [];;

for k = 1 to n-1
do
    (*    i0 + j0 = k
        i1 + (n-j1) = k
        (n-i2) + j2 = k
        (n-i3) + (n-j3) = k *)
    let vars = Oops.array_make_n_var man (k+1) in
    let uniq = Oops.vect_atmost_one (&!) (k+1) vars in
    let t0 = Array.make (n*n) (CpTypes.P)
    and t1 = Array.make (n*n) (CpTypes.P)
    and t2 = Array.make (n*n) (CpTypes.P)
    and t3 = Array.make (n*n) (CpTypes.P) in
    for i = 0 to k
    do
        let j = k - i in
        t0.(i*n            + j)<-(CpTypes.S);
        t1.((n-1-i)*n    + j)<-(CpTypes.S);
        t2.(i*n            + (n-1-j))<-(CpTypes.S);
        t3.((n-1-i)*n    + (n-1-j))<-(CpTypes.S);
    done;
    let uniq0 = Oops.(->>) (Array.to_list t0) uniq
    and uniq1 = Oops.(->>) (Array.to_list t1) uniq
    and uniq2 = Oops.(->>) (Array.to_list t2) uniq
    and uniq3 = Oops.(->>) (Array.to_list t3) uniq in
    liste := uniq0::uniq1::uniq2::uniq3::(!liste);
done;

print_string "starting computation (#and = "; print_int (List.length(!liste)); print_string" )";
print_newline();;

let diag_uniq = Oops.list_et (&!) (!liste);;

let n_queens = n_tower &! diag_uniq;;
(*print_string "n_queens : "; cnt n_queens;;
Oops.store_ALLsat man n_queens "../dots/nQueens.sat";;*)

print_string "------ end of computation ------";;
print_newline();;

print_string "time:\t";;
print_float (Sys.time() -. start);;
print_string " s.";;
print_newline();;


let name = "workdir/"^(string_of_int n)^"-nqueens.cp.tacx"

let dump_man = Udag.STree.newman ();;
let dump_queens = T.dump man dump_man [n_queens];;
let dump_queens = Udag.STree.dump dump_man dump_queens;;
STree.dumpfile [dump_queens] name;;

let strman = Udag.String.newman ();;
let strqueens = T.to_dot man strman [n_queens];;
Udag.String.to_dot_file strman strqueens (name^".dot");;

exit 0;;
