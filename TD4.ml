(*partage*)
type humain = {prenom : string; mutable age : int};;

let san = {prenom = "San"; age = 28};;

let anniversaire h = h.age <- h.age + 1;;


let mic = {prenom = "mic"; age = san.age};;
let mic = {san with prenom = "mic"}

(*todo*)

let san3 = {san with age = san.age};;
let san3 = {prenom = san.prenom; age = san.age};;

(*reference*)
(*E6*)
let a = ref 0;;
(*E7*)
let incr r = r := !r + 1;;
(*E8*)
let copie r = ref !r;;
(*E9*)
let x = ref 0;;
let y = copie x;;
let z = x;;

(*E10*)
let rt = ref [|1;2|];;
let rt' = copie rt;;


let t = Array.make 3 0;;
let m = Array.make 3 t;;

type humain2 = {prenom : string; mutable age : int ref}

let san = {prenom = "san"; age = ref 2};;

let anni h = h.age := !(h.age) + 1;;

let san2 = san;;

let mic = {san with prenom = "mic"}
let mic = {prenom = "mic"; age = san.age}

let san3 = {san with age = san.age}









