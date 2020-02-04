(** ------ EXERCICE _1_ ------ **)
(* LONGUEUR *)
let rec longueur l1 =
	match l1 with
	[] -> 0
	| x::r -> 1 + longueur r;;

let maListeVide = [];;
let maListe = [3; 5; 4; 8; 6; 8; 1];;
let maListeBis = [1; 2; 3];;

longueur maListe;;

(* CONCATENATION *)
let rec concat l1 l2 =
	match l1 with
	| [] -> l2
	| x::r -> [x] @ concat r l2;;


concat maListe maListeBis;;

(* NNIEME *)
let rec nieme l1 n =
	match (l1,n) with
	| ([]	, _) -> failwith "Pas assez d'arguments"
	| (y::_	, 1) -> y
	| (_::r	, _) -> nieme r (n-1);;

nieme maListe 4;;

(** ------ EXERCICE _2_ ------ **)
(* NPREMIERS *)
let rec npremiers l1 n =
	match (l1,n) with
	| ([]	, _) -> failwith "Pas assez d'arguments !"
	| (y::_	, 1) -> [y]
	| (y::r	, _) -> [y] @ npremiers r (n-1);;

npremiers maListe 4;;

(* MET A PLAT *)
let rec met_a_plat ll1 =
	match ll1 with
	| [] 	-> []
	| x::r	-> x @ met_a_plat r;;

let maListeDeListe = [[]; [1; 8; 5]; [1]];;
met_a_plat maListeDeListe;;

(* PAIRE VERS LISTE *)(***)
(* ([1; 2; 3], [4; 5; 6]) -> [(1,4); (2,5); (3, 6)] *)

(* SUPPRIME 1ère OCC *)
let rec supprime1 l1 test =
	match l1 with
	| []	-> failwith "Pas trouve"
	| x::r	-> if test=x then r
			else [x] @ supprime1 r test;;

supprime1 maListe 8;;
(* SUPPRIME ALL OCC *)
let rec supprime2 l1 test =
	match l1 with
	| []	-> []
	| x::r	-> if test=x then supprime2 r test
			else [x] @ supprime2 r test;;

supprime2 maListe 8;;

(* MINIMUM*)
let rec min_liste l1 =
	match l1 with
	| []	-> failwith "liste vide"
	| [x]	-> x
	| x::r	-> let m = min_liste r
			in if x<m then x else m;;

min_liste maListe;;

(* DOUBLON *)
let rec doublon l1 =
	match l1 with





