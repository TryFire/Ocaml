type 'a btree = Empty | Node of 'a * 'a btree * 'a btree;;

let tree = Node(10, Node(2, Node(3, Empty, Empty), Empty), Node(12, Empty, Empty));;

let rec taille t = match t with 
	| Empty -> 0
	| Node(_, g, d) -> 1 + taille g + taille d;;

let rec hauter t = match t with 
	| Empty -> 0
	| Node(_, g, d) -> 1 + max (hauter g) (hauter d);;

let rec somme t = match t with 
	| Empty -> 0
	| Node(r, g, d) -> r + (somme g) + (somme d);; 
	

2.1
type arith = 
	| Const of int
	| Add of arith * arith
	| Times of arith * arith
	| Exp of arith * int
	| X;;

2.2
let p = Times(Add(Exp(X, 2), Const(1)), Add(Times(X, Const(2)), Const(-3)));;

2.3.1
let rec interp x arith = match arith with
	|X -> x 
	|Const(v) -> v
	|Add(ag, ad) -> (interp x ag) + (interp x ad)
	|Times(ag, ad) -> (interp x ag) * (interp x ad)
	|Exp(ag, v) when v = 1 -> interp x (Times(ag, Const(1)))
	|Exp(ag, v) when v = 2 -> interp x (Times(ag, ag))
	|Exp(ag, v) when (v > 2) && (v mod 2 = 0) -> interp x (Times(Exp(ag, v/2), Exp(ag, v/2)))
	|Exp(ag, v) -> interp x (Times(Times(Exp(ag, (v-1)/2), Exp(ag, (v-1)/2)), Exp(ag, 1)));;
	
2.3.2
let rec interp x arith = match arith with
	|X -> x 
	|Const(v) -> v
	|Add(ag, ad) -> (interp x ag) + (interp x ad)
	|Times(ag, ad) -> (interp x ag) * (interp x ad)
	|Exp(ag, v) -> int_of_float(float_of_int(interp x ag)**float_of_int(v));;
	
2.4
interp 1 p;;

2.5
je pense que c'est fait a 2.3

2.6
let q = Exp(p, 2);;
interp (-1) q;;
c'est vrai

2.8
type horner = int list;;

let a : horner = [2;3;4];;

2.9
let rec cal_horner x h = match h with
	|[] -> 0 
	|v::l -> v + x*(cal_horner x l);;


2.10
let rec addh h1 h2 = match (h1, h2) with
	|([], h) -> h
	|(h, []) -> h
	|(r1::l1, r2::l2) -> (r1+r2) :: addh l1 l2;; 


2.11


















