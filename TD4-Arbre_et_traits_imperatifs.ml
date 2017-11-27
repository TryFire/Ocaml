(*1*)

type 'a arbre = F of 'a | N of 'a * 'a arbre * 'a arbre;;

let a = N(2,N(5, F(4), F(7)), F(3));;

let rec elements a = match a with
	| F(v) -> [v]
	| N(v, g, d) -> v::(List.append (elements g) (elements d));;

let rec feuilles a = match a with 
	| F(v) -> 1
	| N(v, g, d) -> feuilles g + feuilles d;; 

let rec noeuds a = match a with 
	| F(v) -> 0
	| N(v, g, d) -> 1 + noeuds g + noeuds d;;

let rec profondeur a = match a with
	| F(v) -> 1
	| N(v, g, d) -> 1 + max (profondeur g) (profondeur d);;

(*foldleft de List*)
let rec foldleft f l r = 
	match l with
	| [] -> r
	| v::d -> foldleft f d (f v r);;

let add l = foldleft (fun a b -> a+b) l 0;;
(**)

(*E1*)
let rec fold ff fn a = match a with	
	| F(v) -> ff v 
	| N(v, g, d) -> fn v (fold ff fn g) (fold ff fn d)

(*E2*)
let elements a = fold (fun a -> [a]) (fun a b c-> a :: b @ c) a;;

let feuilles a = fold (fun a -> 1) (fun a b c -> b + c) a;;

let noeuds a = fold (fun a -> 0) (fun a b c -> 1 + b + c) a;;

let profondeur a = fold (fun a -> 1) (fun a b c -> 1 + max b c) a;;

(*E3*)
let t = [|1;2;3;4;5|];;
let t1 = [|1;7;3;4;5|];;

let est_trie t = 
	let is_trie = ref true in
	let length = Array.length t in
	if length > 1 then
		for i = 0 to length - 2 do
			if t.(i) > t.(i+1) then is_trie := false
		done;
	(!is_trie)
;;

(*E4*)
let echange t = 
	let length = Array.length t in
	for i = 0 to length - 2 do 
		if t.(i) > t.(i+1) then 
			begin
				let temp = t.(i) in
				t.(i) <- t.(i+1);
				t.(i+1) <- temp
			end
	done
;;

(*E5*)
let t2 = [|5;3;4;1;2|];;
let tri t = 
	let length = Array.length t in
	let bulle = ref (length - 1) in
	while !bulle >= 1 do
		echange t;
		bulle := !bulle - 1
	done
;;

(*E6*)
let l = [1;2;3;];;
let l1 = [1;2;0;4];;
let rec mult l = 
	match l with 
	| v::[] -> v
	| v::left -> v * (mult left);;

(*E7*)
let rec mult_g_zero l = 
	match l with 
	| v::[] -> v
	| v::left -> if v = 0 then mult_g_zero left 
							 else v * (mult_g_zero left);;

(*E8*)
exception Zero;;
let mult3 l = 
	let rec aux l = 
		match l with
		| [] -> 1
		| 0::left -> raise Zero
		| v::[] -> v
		| v::left -> v * (aux left)
	in try aux l with Zero -> 0;;

(*E9*)
let mult4 l =
	try
		List.fold_left (fun a b -> if b = 0 then raise Zero else a*b) 1 l
	with 
		| Zero -> 0;;


(*E10&&E11*)
exception Not_Found;;
exception More_than_une_cle;;
let tl = [(1,2); (2,2); (3,4)];;
let assoc_int k l = 
	let res = 
	List.fold_left (fun a b -> let (k1, b1) = b in if (k1 = k) then b1::a else a) [] l
	in match res with
	| [] -> raise Not_Found
	| [v] -> v
	| v::left -> raise More_than_une_cle;;





