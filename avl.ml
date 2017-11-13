(*definir de AVL:*)
(* AVL est sur la base de arbre binaire*)
(* la difference entre les hauteurs des fils gauche et des fils droit de*)
(* tout noeud ne peut exceder 1*)
(* pour tout noeuds, value d'element gauche est petit que moi et*)
(* celui droit est grand que moi*)
(* on ajoute in entier dans les noeuds afin de momoriser la hauteur des arbres*)


type 'a avl = Vide | Noeud of 'a * 'a avl * 'a avl * int;;

let a = Noeud(5, Noeud(8, Vide, Vide, 1), Noeud(10, Vide, Vide, 1), 2);;


let height = function 
	| Vide -> 0
	| Noeud(_, _, _, h) -> h;;

let creer value gauche droit = 
	Noeud(value, gauche, droit, 1 + max (height g) (height d));;

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


let rec ajoute e a = match a with
	| Vide -> creer e Vide Vide
	| Noeud(v, g, d, _) -> if v = e then a
												 else if e < v then equilibrage v (ajoute e g)	d
												 else equilibrage v g (ajoute e d);;	 





