Q7
  
type t = {
  digits : int list;
  base : int;
}

let decompose a b = let x =
  let rec rec_dec a b =
    if b <> 0 then (b mod a) :: rec_dec a (b/a)
    else []
  in rec_dec a b
  in {digits = x; base = a};;


Q8
let print t =
  Printf.printf "Nombre : ";
  let nom = t.digits in
  List.iter (fun a -> Printf.printf "%d" a) (List.rev nom);
  Printf.printf "En base : ";
  Printf.printf "%d" t.base;;

Q9
let to_int t =
  List.fold_right (fun a b -> b*t.base + a) t.digits 0;;


Q10
let add a b =
  let a_10 = to_int a in
  let b_10 = to_int b in
  decompose a.base (a_10 + b_10);;

Q11
let positions t =
  let (x, y) =
    List.fold_left (fun (list,pos) b -> if b <> 0 then (pos::list,pos+1) else (list,pos+1 )) ([],0) t.digits
  in x;;
