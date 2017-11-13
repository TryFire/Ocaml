(*PARTIE 1*)

(*définir le type arbre*)
type 'a arbre = Vide | Noeud of 'a * 'a arbre * 'a arbre;;

let a = Noeud(10, Noeud(2, Noeud(8, Vide, Vide), Noeud(12, Noeud(3, Vide, Vide), Vide)), Noeud(5, Vide, Noeud(15, Vide, Vide)));;

(*la fonction taille renvoie le nombre d'un arbre binaire*)
let rec taille a = 
	match a with 
	| Vide -> 0
	| Noeud(_, g, d) -> 1 + taille g + taille d;;

(*la fonction profondeur renvoie la longeur de la plus grand branche d'un arbre binaire*)
let rec profondeur a = 
	match a with 
	| Vide -> 0
	| Noeud(_, g, d) -> 1 + max (profondeur g) (profondeur d);;

(*la fonction moroir renvoie la miroir d'un arbre binaire*)
let miroir a = 
	match a with 
	| Vide -> Vide
	| Noeud(r, g, d) -> Noeud(r, d, g);;

(*la fonction recherche recherche si un element est dans un arbre binaire *)
let rec recherche e a = 
	match a with 
	| Vide -> false
	| Noeud(r, g, d) ->  r = e || recherche e g || recherche e d;;

(*definir le type arbre n-aires*)
type 'a n_arbre = Vide | Noeud of 'a * 'a n_arbre list;;

let n_a = Noeud(3,[Noeud(0,[]); Noeud(2,[Noeud(8,[]); Noeud(12,[Noeud(20, [])])]); Noeud(4, [Noeud(5,[]); Noeud(74,[]); Noeud(30,[]); Noeud(9,[])])]);;

let rec n_taille a = 
	match a with 
	| Vide -> 0
	| Noeud(_, l) -> 1 + List.fold_left (fun x y -> x + n_taille y) 0 l;;

let rec n_profondeur a = 
	match a with 
	| Vide -> 0
	| Noeud(_, l) -> 1 + List.fold_left (fun x y -> max x (n_profondeur y)) 0 l;;

(*la fonction list_arbre renvoie une list forme des elements d'un arbre n-naires*)
let rec list_arbre a = 
	match a with 
	| Vide -> []
	| Noeud(r, l) -> r::List.fold_left (fun x y -> List.append x (list_arbre y)) [] l;;


(*PARTIE 2 : arbre binaire de recherche*)

(* connaissance:
   1. les ´el´ements du sous-arbre gauche g sont inf´erieurs `a la racine x
   2. la valeur x stock´ee `a la racine de l’arbre est inf´erieure aux ´el´ements du sous-arbre droit d
	 3. les sous-arbres g et d sont eux-mˆemes ordonn´es*)

let a2 = Noeud(5, Noeud(4, Noeud(2, Vide, Noeud(3, Vide, Vide)), Vide), Noeud(9,Noeud(7, Vide, Vide), Vide));;

(*function recherche*)
(*type 1*)
let rec recherche e = function
	| Vide -> false
	| Noeud(r, g, d) -> if r = e then true else if e < r then recherche e g else recherche e d;;
(*type 2*)
let rec recherche e = function
	| Vide -> false
	| Noeud(r, g, d) when r = e -> true 
	| Noeud(r, g, _) when e < r -> recherche e g
	| Noeud(_, _, d) -> recherche e d;;

(*PARTIE 3 : ajout d'un element*)
(*ajout aux feuilles*)
let rec ajout e a = 
	match a with
	| Vide -> Noeud(e, Vide, Vide)
	| Noeud(r, _, _) when r = e -> a
	| Noeud(r, g, d) when e < r -> Noeud(r, ajout e g, d)
	| Noeud(r, g, d) -> Noeud(r, g, ajout e d);;


(*ajout a la racine*)

(*coupe un arbre to 2 arbre*)
let rec coupe e a = 
	match a with 
	| Vide -> (Vide, Vide)
	| Noeud(r, g, d) when r = e -> (g, d)
	| Noeud(r, g, d) when r < e -> let (x, y) = coupe e d in (Noeud(r, g, x), y)
	| Noeud(r, g, d) -> let (x, y) = coupe e g in (x, Noeud(r, y, d));;

(*ajout 2 arbre coupe par la fonction coupe*)
let ajout_racine e a = 
	let (x, y) = coupe e a in 
	Noeud(e, x, y);;


let rec enlever_plus_grand a = 
	match a with
	| Vide -> raise Not_found
	| Noeud(r, g, Vide) -> (r, g)
	| Noeud(r, g, d) -> let (x, h) = enlever_plus_grand d in (x, Noeud(r, g, h));;

let rec suppression e a = 
	match a with
	| Vide -> Vide
	| Noeud(r, Vide, d) when e = r -> d
	| Noeud(r, g, d) when e = r -> let (x, h) = enlever_plus_grand g in Noeud(x, h, d)
	| Noeud(r, g, d) when e < r -> Noeud(r, suppression e g, d)
	| Noeud(r, g, d) -> Noeud(r, g, suppression e d);;















