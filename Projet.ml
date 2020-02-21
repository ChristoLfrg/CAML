			(** ** ** ** PROJET ** ** ** **)

	(** ALGO 1 **)

let graphe1 = [	(1,[6;7;8]) ; 
		(2,[1;4]) ; 
		(3,[2]) ; 
		(4,[3;5]) ; 
		(5,[1]) ; 
		(6,[5;7]) ; 
		(7,[]) ; 
		(8,[6;7])];;
		
				(* LISTE DES SOMMETS *)
			(* «'a list» des sommets «a» d'un graphe «g» *)
let rec liste_sommets g =
	match g with
	| [] -> []
	| (a,l)::r -> a::liste_sommets r;;
	
liste_sommets graphe1;;

				(* LISTE DES SUCCESSEURS *)
		(* «'a list» des successeurs «l» d'un sommet «s» d'un graphe «g» *)
let rec liste_succ s g =
	match g with
	| [] -> failwith "Sommet introuvable"
	| (a,l)::r -> if s=a then l
			else liste_succ s r;;

liste_succ 1 graphe1;;
liste_succ 7 graphe1;;

				(* INVERSER UN GRAPHE *)
(** 	Pour inverser un graphe, (a,li) chaque membre de li doit recevoir comme successeur le sommet a 
	J'ai donc besoin d'une fonction qui fasse cela sur chaque sommet (couple) du graphe
	
	J'ai [(1,[6;7;8]),(6,[5;7])]
	       s   li
	Je veux [(5,[6]);(6,[1]);(7,[1;6]);(8,[1])]
	             s       s       s
**)
	(* faire succéder un sommet «s» à chacun des sommets d'une liste «li» *)
let rec inserer_liste_couple s li =
	match li with
	| [] -> []
	| x::r -> (x,[s])::inserer_liste_couple s r;;
	
(**Ex : (1,[3;6]) -> [(3,[1]);(6,[1])]**)
inserer_liste_couple 1 [3;6];;

let rec inverse_graphe g =
	match g with
	| [] -> []
	| (a,l)::r -> (inserer_liste_couple a l)@(inverse_graphe r);;

let rec distribuer s li =
	List.fold_left (fun f l -> match li with x::r ->  ) [] li;; 

(** D'ABORD créer un graphe avec la liste des sommets G2 = (1;2;3;4;5;6;7;8)
	G1 = [(1,[6;7;8]),(6,[5;7])]
	PUIS pour chaque sommets successeurs, donner au sommet correspondant le sommet de base  *)
(*
let inverse_graphe2 g =
	List.fold_left (inserer_liste_couple) [] g;;*)

inverse_graphe graphe1;;









