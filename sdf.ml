#use "./math.ml";;

(** PRIMITIVES **)

let sphereSDF s p =
(* Prend la sphère et le vecteur directeur du photon *)
    norme(somme_vecteur(vecteur(origine)(s.centre))(produit_vecteur(-1.)(p))) -. s.rayon;;

let planSDF pl p  =
    let (a,b,c,d) = (pl.compA, pl.compB, pl.compC, pl.compD)
    in produit_scalaire(unitaire({vx=a;vy=b;vz=c}))(p) +. d;;

(** OPERATEURS **)

let opRep de period p =
    let pseudophoton = somme_vecteur(mod_vecteur(somme_vecteur(p)(produit_vecteur(0.5)(period)))(period))(produit_vecteur(-0.5)(period))
    in de(pseudophoton);;

let opSymoX de p =
    let p_bis = {vx=abs_float(p.vx); vy=p.vy; vz= p.vz}
    in de(p_bis);;
    
let opSymoY de p =
    let p_bis = {vx=p.vx; vy=abs_float(p.vy); vz= p.vz}
    in de(p_bis);;

let opSymoZ de p =
    let p_bis = {vx=p.vx; vy=p.vy; vz= abs_float(p.vz)}
    in de(p_bis);;

let opSymoXY de = opSymoX(opSymoY(de));;

let opSymoXZ de = opSymoX(opSymoZ(de));;

let opUnion de1 de2 p =
    min(de1(p))(de2(p));;

let opInter de1 de2 p =
    max(de1(p))(de2(p));;

let opSub de1 de2 p =
    max(de1(p))(-.de2(p));;

let foldOp op des =
    List.fold_left (op) (List.hd(des)) (List.tl(des)) ;;