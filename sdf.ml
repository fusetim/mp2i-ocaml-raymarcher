#use "./math.ml";;

(* * Module SDF (Signed Distance Function) * *)

(** PRIMITIVES **)
let sphereSDF s p =
    (* Prend la sphère et le vecteur directeur du photon pour en déduire sa distance. *)
    norme(somme_vecteur(vecteur(origine)(s.centre))(produit_vecteur(-1.)(p))) -. s.rayon;;

let planSDF pl p  =
    (* Prend le plan et le vecteur directeur du photon pour en déduire sa distance. *)
    let (a,b,c,d) = (pl.compA, pl.compB, pl.compC, pl.compD)
    in produit_scalaire(unitaire({vx=a;vy=b;vz=c}))(p) +. d;;

(** OPERATEURS **)

(*
 * Opérateur de répétition de l'espace.
 *
 * Forme un pli de l'espace de période (vecteur period) pour le DE/SDF (Distance Estimator 
 * / Signed Distance Function) fourni.
 * Autrement dit, il est censé répéter une partie de l'espace (définie par period) à l'infini.
 *)
let opRep de period p =
    let pseudo = somme_vecteur(mod_vecteur(p)(period))(produit_vecteur(-0.5)(period))
    in de(pseudo);;

(*
 * Opérateur de symétrie de l'espace par l'axe X.
 *)
let opSymoX de p =
    let p_bis = {vx=abs_float(p.vx); vy=p.vy; vz= p.vz}
    in de(p_bis);;

(*
 * Opérateur de symétrie de l'espace par l'axe Y.
 *)  
let opSymoY de p =
    let p_bis = {vx=p.vx; vy=abs_float(p.vy); vz= p.vz}
    in de(p_bis);;

(*
 * Opérateur de symétrie de l'espace par l'axe Z.
 *)
let opSymoZ de p =
    let p_bis = {vx=p.vx; vy=p.vy; vz= abs_float(p.vz)}
    in de(p_bis);;

(*
 * Opérateur de symétrie de l'espace par l'axe X et l'axe Y.
 *)
let opSymoXY de = opSymoX(opSymoY(de));;

(*
 * Opérateur de symétrie de l'espace par l'axe X et l'axe Z.
 *)
let opSymoXZ de = opSymoX(opSymoZ(de));;

(*
 * Opérateur de symétrie de l'espace par l'axe Y et l'axe Z.
 *)
let opSymoYZ de = opSymoY(opSymoZ(de));;

(*
 * Opérateur d'union booléen.'
 *)
let opUnion de1 de2 p =
    min(de1(p))(de2(p));;

(*
 * Opérateur d'intersection booléen.'
 *)
let opInter de1 de2 p =
    max(de1(p))(de2(p));;

(*
 * Opérateur de soustraction booléen.'
 *)
let opSub de1 de2 p =
    max(de1(p))(-.de2(p));;

(*
 * Opérateur de dispersion par sinus.
 *
 * Pas très utile mais permet de faire de la dispersion de l'espace.
 *)
let opDisp de p =
    let pseudo = {
        vx = p.vx+.sin(p.vx);
        vy = p.vy+.sin(p.vy);
        vz = p.vz+.sin(p.vz)
    }
        in de(pseudo);;

(*
 * Applique à une liste de DE/SDF (Distance Estimator / Signed Distance Function)
 * une opération par la gauche.
 *)
let foldOp op des =
    List.fold_left (op) (List.hd(des)) (List.tl(des)) ;;