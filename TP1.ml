(* ------ EXERCICE _1_ ------ *)
(* TTC *)
let prixttc prixht = prixht *. 1.20;;

prixttc 2.;;

(* BISSEXTILE *)
let biss annee =
	if annee mod 4 = 0 && annee mod 100 <> 0 then true 
	else if annee mod 400 = 0 then true
		else false;;

biss 2019;; (* pas div par 4 ni 400 *)
biss 2008;; (* div par 4 *)
biss 2000;; (* pas div par 4 mais oui par 400 *)

(* MINUSCULE *)
let min c = c>='a' && c<='z';;

min 'a';;
min 'A';;

(* MOYENNE *)
let moy x y = (x+.y)/.2.;;
moy 10. 20.;;
moy 17.2 14.3;;

(* QUOTIENT *)
let quot_reste x y = (x/y, x mod y);;

quot_reste 50 3;;

(* PUISSANCE 4 *)
let puiss_4 x =
	let carre x = x*x
	in carre (carre x);;

puiss_4 2;;

(* UPPER *)
let upper c = 
	if min c then char_of_int (int_of_char c + int_of_char 'A' - int_of_char 'a')
	else failwith "Pas un caractere minuscule";;

upper 'a';;
upper 'R';;
upper 'l';;

(* ------ EXERCICE _2_ ------ *)
(* FIBONACCI *)
let rec fibo x =
	match x with 
	0 -> 0
	| 1 -> 1
	| y -> fibo (x-1)+ fibo (x-2);;
fibo 3;;
fibo 8;;

(* SOMME N CARRES *)
let rec somme_n x =
	match x with
	0 -> 0
	|_ -> somme_n(x-1) + x*x;;

somme_n 4;;

(* ------ EXERCICE _3_ ------ *)
(* SIGMA *)
let rec sigma f x =
	let res = 0 in	
		match x with
		0 -> res
		|_ ->  res + f x + sigma f (x-1);;

let somme_n_sigma x =
	sigma(fun y -> y*y) x;;

somme_n_sigma 4;;

(* ROND *)

let rond f g x = g (f x);;

let carre x = x*x;;

rond carre somme_n_sigma 2;;

(* ------ EXERCICE _4_ ------ *)
let abs x =
	if x<0. then (-.x)
	else x;;

let rec newton x y eps =
	if abs(y*.y-.x) <= eps then y
	else newton x ((y+.x/.y)/.2.) eps;;

newton 2. 3. 5.;;


