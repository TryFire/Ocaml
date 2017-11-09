let a1 = [1;2];;
let a2 = [1;2;3;4];;
let a3 = [4;5;2;1];;

"A1"
let trois_ou_plus l = match l with
	| [] -> false
	| _::[] -> false
	| _::_::[] -> false
	| _ -> true;;
	
let trois_ou_plus1 l = 
	let num = List.length l in
	if num>=3 then true else false;;
	
Printf.printf(trois_ou_plus a1);;
Printf.printf(trois_ou_plus1 a2);;

"A2"
let rec dernier_element l = match l with 
	| [] -> -1
	| h1::[] -> h1
	| h1::h2 -> dernier_element h2;;
	
"A3"
let rec somme l = match l with 
	| [] -> 0
	| h1::left -> h1 + somme left;;

"A4"
let rec est_croissante l = match l with 
	| [] -> true
	| h1::[] -> true
	| h1::h2::left -> if h1 >= h2 then false else est_croissante (h2::left);;

"A5"
let rec nb_occ e l = 
	match l with 
		| [] -> 0
		| h1::left -> if h1 = e then 1 + (nb_occ e left) else nb_occ e left;;

"A6"
let rec nieme n l = match l with
	| [] -> -1
	| h1::left -> if n = 1 then h1 else nieme (n-1) left;;
	
	
"A7"
let rec max l = match l with 
	| [] -> -1
	| h1::[] -> h1
	| h1::left -> if h1 >= max left then h1 else max left;;


"B"
let alph = ['a';;;;;;;;;;;;;;;;;;;;;;;]



































