String.make n c 
String.sub s  g d

(*on defini le type t qui contient *)
(* Str(s, i, n) correspond `a la sous-chaˆıne s[i..i+n-1]
	 App(r1, r2, n) correspond `a la concat´enation de deux cordes r1
	 et r2 dont la longueur totale est n*)
type t = Str of string * int * int | App of t * t * int;;


(*on cree deux example de type t*)
let empty = ("", 0 ,0);;
let t1 = App(Str("bon", 0, 3),Str("jour", 0, 4), 7);;


(*la fonction length peux renvoie la length d'un example de t*)
let length = function
	| Str(_, _, n)
	| App(_, _, n) -> n;;


(*la fonction of_length peux transformer string a type t*)
let of_string s = Str(s, 0, String.length s);;


(*la fonction make prends 2 arguments et renvoie type t*)
let make n c = of_string(String.make n c);;


(*la fonction unsafe_get prends 2 arguments(type t, et int i) et renvoie *)
(* la i ieme lettre d'un argument de t*)
(* mais il est unsafe si i moins de 0 ou i grand de length de t*)
let rec unsafe_get t i = match t with
	| Str(s, g, _) -> s.[g+i]
	| App(t1, t2, _) -> if i < length t1 then unsafe_get t1 i 
									 else unsafe_get t2 (i - (length t1)) ;;


(*la fonction get prends 2 arguments (type t et int i) et renvoie *)
(* la i ieme lettre d'un argument de t*)
let get t i = if i < 0 || i >= length t then invalid_arg "get";
unsafe_get t i;;


let append_string s1 sg1 sd1 s2 sg2 sd2 = 
	let ss1 = String.sub s1 sg1 sd1 in
	let ss2 = String.sub s2 sg2 sd2 in
	Str(ss1 ^ ss2, 0 )














