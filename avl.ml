(*definir de AVL:*)
(* AVL est sur la base de arbre binaire*)
(* la difference entre les hauteurs des fils gauche et des fils droit de*)
(* tout noeud ne peut exceder 1*)
(* pour tout noeuds, value d'element gauche est petit que moi et*)
(* celui droit est grand que moi*)
(* on ajoute in entier dans les noeuds afin de momoriser la hauteur des arbres*)


type 'a avl = Vide | Noeud of 'a * 'a avl * 'a avl * int;;

let a = Noeud(5, Noeud(3, Vide, Vide, 1), Noeud(10, Vide, Vide, 1), 2);;


(*la fonction height prends une argument(il doit le type de avl) et*)
(* renvoie la height de avl*)
let height = function 
	| Vide -> 0
	| Noeud(_, _, _, h) -> h;;


(*la fonction creer prends 3 arguments et renvoie un nouveau avl*)
(* value: l'element qui est la racine lie aux 2 avl*)
(* gauche: un avl qui va etre la fil gauche dans un nouveau avl*)
(* droit: un avl qui va etre la fil dtoir dans un nouveau avl*)
(* renvoie: un nouveau avl qui combaine value, 2 avl*)
let creer value gauche droit = 
	Noeud(value, gauche, droit, 1 + max (height gauche) (height droit));;


(*la fonction equilibrage construit un arbre equilibre forme a un avl*)
(* value: la value de racine dans Noeud(value, gauche, droit)*)
(* gauche: un arbre qui est dan fil gauche dans Noeud(value, gauche, droit)*)
(* droit: un arbre qui est dan fil droit dans Noeud(value, gauche, droit)*)
(* renvoie: un avl*)
let equilibrage value gauche droit = 
	if (height gauche) > (height droit) +1 then
		begin
			match gauche with
			| Noeud(v, ng, nd, _) when (height ng) >= (height nd) -> 
				creer v ng (creer value nd droit)
			| Noeud(v, ng, Noeud(vd, ndg, ndd, _), _) -> 
				creer vd (creer v ng ndg) (creer value ndd droit)
			| _ -> assert false
		end
	else if (height droit) > (height gauche) +1 then
		begin
			match droit with
			| Noeud(v, ng, nd, _) when (height ng) <= (height nd) ->
				creer v (creer value gauche ng) nd
			| Noeud(v, Noeud(vg, ngg, ngd, _), nd, _) ->
				creer vg (creer value gauche ngg) (creer v ngd nd)
			|_ -> assert false
		end
	else 
		creer value gauche droit;;


(*la fonction ajoute construit a ajoute un element a un avl*)
let rec ajoute e a = match a with
	| Vide -> creer e Vide Vide
	| Noeud(v, g, d, _) -> if v = e then a
												 else if e < v then equilibrage v (ajoute e g)	d
												 else equilibrage v g (ajoute e d);;	 


(*en suite on va faire la fonction suppression*)

(*la fonction obtenir_plus_petit_element construit a *)
(* obtenir la plus petit element dans un avl*)
(* a: un avl*)
(* renvoie: la plus petit element*)
let rec obtenir_plus_petit_element a = match a with
	| Vide -> raise Not_found
	| Noeud(v, Vide, d, _) -> v
	| Noeud(v, g, d, _) ->  obtenir_plus_petit_element g;;

(*la fonction supp_plus_petit_element construit a *)
(* suppremer la plus petit element dans un avl*)
(* renvoie: un nouveau avl qui n'a la plus petit element dan avl original*)
let rec supp_plus_petit_element a = match a with
	| Vide -> raise Not_found
	| Noeud(v, Vide, d, _) -> d
	| Noeud(v, g, d, _) -> equilibrage v (supp_plus_petit_element g) d;;

(* la fonction fusion prends 2 avl et renvoie un nouveau avl*)
let fusion g d = match (g, d) with 
	| (Vide, t)|(t, Vide) -> t
	| (_, _) -> 
		equilibrage (obtenir_plus_petit_element d) g (supp_plus_petit_element d);;

(*la fonction suppression construit a supprimer d'un element dans un avl*)
let rec suppression e a = 
	match a with
	| Vide -> assert false
	| Noeud(v, g, d, _) when e = v -> fusion g d
	| Noeud(v, g, d, _) -> if e < v then equilibrage v (suppression e g) d
												 else equilibrage v g (suppression e d);;












