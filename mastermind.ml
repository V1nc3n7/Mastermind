type couleur= Rouge | Bleu | Vert | Blanc | Jaune | Noir | Violet | Orange;;

let taille=5;;

let rec construire taille=
  let rec construire_aux liste=
    match liste with
    |[]->[]
    |h::t -> (("rouge"::h)::(("vert"::h)::(("blanc"::h)::(("noir"::h)::(("bleu"::h)::(("jaune"::h)::(("violet"::h)::(("orange"::h)::(construire_aux t)))))))))
  in match taille with
     |1 ->[["rouge"];["vert"];["blanc"];["noir"];["bleu"];["jaune"];["violet"];["orange"]]
     |_->construire_aux (construire(taille-1));;

