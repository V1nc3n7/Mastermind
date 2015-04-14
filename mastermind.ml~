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
