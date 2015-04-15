type couleur= Rouge | Bleu | Vert | Blanc | Jaune | Noir | Violet | Orange;;

let taille=5;;
let liste_couleurs =  [ "rouge";"vert";"blanc";"noir";"bleu";"jaune";"violet";"orange"] ;;
let rec  construire taille =
  let rec construire_aux liste =
    match liste with
    |[]->[]
    |h::t -> (("rouge"::h)::(("vert"::h)::(("blanc"::h)::(("noir"::h)::(("bleu"::h)::(("jaune"::h)::(("violet"::h)::(("orange"::h)::(construire_aux t)))))))))
  in match taille with
  |1 ->[["rouge"];["vert"];["blanc"];["noir"];["bleu"];["jaune"];["violet"];["orange"]]
  |_->construire_aux (construire(taille-1));;


let  occur liste couleur=
  let rec aux liste couleur n =
    match liste with 
    |[] -> n
    |h::t -> if h=couleur then aux t couleur (n+1) else aux t couleur n 
  in aux liste couleur 0;;

(*
  let rec elag_utile liste=
  match liste with
  [] -> false
  |h::t -> if occur liste h > 1 then true else elag_utile t;;
  
  let elagage liste =
  if elag_utile liste then (elaguerliste liste) else liste ;; *)
let elaguerliste l = 
  let rec aux l res =
    match l with 
      []-> res 
    |h::t -> if (occur res h)> 1 then aux t res else aux res::h
  in aux l [];;
type couleur= Rouge | Bleu | Vert | Blanc | Jaune | Noir | Violet | Orange;;

let taille=5;;
let liste_couleurs =  [ "rouge";"vert";"blanc";"noir";"bleu";"jaune";"violet";"orange"] ;;
let rec  construire taille =
  let rec construire_aux liste =
    match liste with
    |[]->[]
    |h::t -> (("rouge"::h)::(("vert"::h)::(("blanc"::h)::(("noir"::h)::(("bleu"::h)::(("jaune"::h)::(("violet"::h)::(("orange"::h)::(construire_aux t)))))))))
  in match taille with
  |1 ->[["rouge"];["vert"];["blanc"];["noir"];["bleu"];["jaune"];["violet"];["orange"]]
  |_->construire_aux (construire(taille-1));;


let  occur liste couleur=
  let rec aux liste couleur n =
    match liste with 
    |[] -> n
    |h::t -> if h=couleur then aux t couleur (n+1) else aux t couleur n 
  in aux liste couleur 0;;

(*
  let rec elag_utile liste=
  match liste with
  [] -> false
  |h::t -> if occur liste h > 1 then true else elag_utile t;;
  
  let elagage liste =
  if elag_utile liste then (elaguerliste liste) else liste ;; *)
let elaguerliste l = 
  let rec aux l res =
    match l with 
      []-> res 
    |h::t -> if (occur res h)> 1 then aux t res else aux res::h
  in aux l [];;
  
let game1 x =
  let rec aux x=
    match x with
    |0->failwith "End Game"
    |1->jouer(elaguerliste(construire 5))
    |2->main()
    |_->failwith "Erreur"
  in aux x;;

let game2 x =
  let rec aux x=
    match x with
    |0->failwith "End Game"
    |1->jouer(construire 5)
    |2->main()
    |_->failwith "Erreur"
  in aux x;;

let game3 x =
  let rec aux x=
    match x with
    |0->failwith "End Game"
    |1->jouer(construire 5)
    |2->main()
    |_->failwith "Erreur"
  in aux x;;

let partie x =   
    match x with
    |1->game1 x 
    |2->game2 x
    |3->game3 x
    |_->failwith "Erreur";;

let main()=print_string "Bienvenue dans le Mastermind \n";
	   print_string "Quel mode de jeu voulez vous jouez?\n";
	   print_string "1.Sans plusieurs occurence de couleurs\n";
	   print_string "2.Avec plusieurs occurence de couleurs\n";
	   print_string "3.Avec plusieurs occurence de couleur et limité à 20 essais\n";
	   let s=read_line() in
	   partie s;;

main();;

