type couleur= Rouge | Bleu | Vert | Blanc | Jaune | Noir | Violet | Orange;;

let rec  construire taille =
  let rec construire_aux liste =
    match liste with
    |[]->[]
    |h::t -> ((Rouge::h)::((Vert::h)::((Blanc::h)::((Noir::h)::((Bleu::h)::((Jaune::h)::((Violet::h)::((Orange::h)::(construire_aux t)))))))))
  in match taille with
  |1 ->[[Rouge];[Vert];[Blanc];[Noir];[Bleu];[Jaune];[Violet];[Orange]]
  |_->construire_aux (construire(taille-1));;

construire 5;;

let  occur liste couleur=
  let rec aux liste couleur n =
    match liste with 
    |[] -> n
    |h::t -> if h=couleur then aux t couleur (n+1) else aux t couleur n 
  in aux liste couleur 0;;

let elaguerliste l = 
  let rec aux l res =
    match l with 
      []-> res 
    |h::t -> if (occur res h)> 1 then aux t res else aux res::h
  in aux l [];;

let couleur_to_string c = 
  match c with 
  |Bleu -> "Bleu;"
  |Blanc -> "Blanc;"
  |Rouge ->"Rouge;"
  |Vert->"Vert "
  |Noir->"Noir;"
  |Violet->"Violet;"
  |Jaune->"Jaune;"
  |Orange->"Orange;";;
  
let liste_couleur_to_string liste_couleur =
  let rec liste_couleur_to_string_aux liste_couleur s=
    match liste_couleur with
    |[] -> "" 
    |h::t -> (couleur_to_string h)^(liste_couleur_to_string_aux t s)
  in liste_couleur_to_string_aux liste_couleur "";;

liste_couleur_to_string [Rouge;Bleu;Noir];;

let proposer_combi propo liste_possible =
  print_string "Est ce la bonne combinaison?Indiquez le nombre de couleurs bien placées\n";
  print_string(liste_couleur_to_string liste)^"\n";
  
  

let jouer liste = 
  let rec aux liste =
    match liste with
    |[]->print_string "Tricheur,vous avez menti!"
    |[good_combi]->print_string "La bonne réponse est :\n";
		   print_string(liste_couleur_to_string good_combi);
    |h::t->aux (proposer_combi h t)
  in aux liste;;

  
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
