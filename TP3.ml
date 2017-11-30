open Graphics;;

let white = Graphics.white;;
let black = Graphics.black;;

type 'a t = 
	|F of 'a
	|N of 'a t * 'a t * 'a t * 'a t;;
	
type image = int t;;

let a0 : image = N (N (F white,F black,F black,F black),F black,F white,F black);;

let t0 = [|
    [| white ; black ; white ; white |] ;
    [| black ; black ; white ; white |] ;
    [| black ; black ; black ; black |] ;
    [| black ; black ; black ; black |]
  |];;

let rec get_pixel x y l a = match a with
	|F(c) -> c
	|N(so, se, no, ne) -> let l' = l/2 in
						  let x' = x mod l' in
						  let y' = y mod l' in
						  if x >= l' then 
							 if y >= l' then get_pixel x' y' l' ne
							 else get_pixel x' y' l' se
					      else 
							 if y >= l' then get_pixel x' y' l' no
							 else get_pixel x' y' l' so;;



let image_matrix_of_tree longueur arbre = 
	let t0 = Array.make_matrix longueur longueur black in
	for i = 0 to longueur-1 do
		for j = 0 to longueur-1 do
			let color = get_pixel i j longueur arbre in
			(t0.(i)).(j) <- color
		done
	done; t0;;

let image_tree_of_matrix image_matrix x y longueur

