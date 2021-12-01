#use "topfind";;
#require "graphics";;

open Graphics;;

open_graph ":0";;

type point = {x: float; y: float; z: float};;
type vecteur = {vx: float; vy: float; vz: float};;
type camera = {cam_origine: point; cam_cible: vecteur};;
type photon = {photon_origine: point; photon_direction: vecteur};;
type intersection = {intersect_origine: point; intersect_normale: vecteur};;
type sphere = {centre: point; rayon: float};;
type plan = {compA: float; compB: float; compC: float; compD: float};;

let x0 = ref 0.
  and y0 = ref 0.;;

let hauteur = size_x()
and largeur = size_y();;


let origine = {x = 0.; y = 0.; z = 0.};;
let vec_nul = {vx = 0.; vy = 0.; vz = 0.};;

let couleur_lumiere = (0, 0, 255);;
let direction_lumiere = {vx=0.; vy= 0.; vz=1.};;
let base = ref ({vx = 1.; vy = 0.; vz = 0.},
  {vx = 0.; vy = 1.; vz = 0.},
  {vx = 0.; vy = 0.; vz = 1.});;

let vecteur p1 p2 = 
    {vx = p2.x -.p1.x; vy = p2.y-.p1.y; vz = p2.z-.p1.z};;

let produit_scalaire v1 v2 =
    (v2.vx-.v1.vx)+.(v2.vy-.v1.vy)+.(v2.vz-.v1.vz);;

let norme v1 =
    sqrt(v1.vx**2.+.v1.vy**2.+.v1.vz**2.);;

let unitaire v1 =
    let normev = norme v1 in
        {vx = v1.vx/. normev; vy = v1.vy/. normev; vz = v1.vz/. normev};;

let produit_vectorielle v1 v2 =
    {vx = v1.vy *. v2.vz -. v1.vz *. v2.vy;
    vy = v1.vz *. v2.vx -. v1.vx *. v2.vz;
    vz = v1.vx *. v2.vy -. v1.vy *. v2.vx};;

let produit_vecteur facteur vec =
    {vx = facteur*. vec.vx; vy=facteur *. vec.vy; vz = facteur *. vec.vz};;

let somme_vecteur v1 v2 =
    {vx = v1.vx +. v2.vx;
    vy = v1.vy +. v2.vy;
    vz = v1.vz +. v2.vz;};;

let somme_vecteur_point v1 p1 =
    {x = v1.vx +. p1.x;
    y = v1.vy +. p1.y;
    z = v1.vz +. p1.z;};;

let dans_base v1 v2 v3 p1 =
    {vx = produit_scalaire(p1)(v1);
    vy = produit_scalaire(p1)(v2);
    vz = produit_scalaire(p1)(v3)};;

let rotation_Ox v1 theta =
    {vx = v1.vx;
    vy = v1.vy*.cos(theta) -. sin(theta)*.v1.vz;
    vz = -.v1.vx*.sin(theta) +. cos(theta)*.v1.vz};;

let rotation_Oy v1 theta =
    {vx = v1.vx*.cos(theta) +. sin(theta)*.v1.vz;
    vy = v1.vy;
    vz = -.v1.vx*.sin(theta) +. cos(theta)*.v1.vz};;

let rotation_Oz v1 theta =
    {vx = v1.vx*.cos(theta) -. sin(theta)*.v1.vy;
    vy = v1.vx*.sin(theta) +. cos(theta)*.v1.vy;
    vz = v1.vz};;

let rotBase_rot rotfun theta = 
    let (u, v, w) = !base in
        let u_ = rotfun u theta
        and v_ = rotfun v theta
        and w_ = rotfun w theta
        in 
            base := (u_,v_,w_);;

let rotBase_Ox = rotBase_rot rotation_Ox;;
let rotBase_Oy = rotBase_rot rotation_Oy;;
let rotBase_Oz = rotBase_rot rotation_Oz;;

(* Intersection entre photons et objets *)

let intersect_sphere photon sphere =
    (* On se permet de représenter la situation avec les points suivant:
        * O correspond à l'origine porté par le photon
        * I correspond au potentiel point d'intersection 
        * C correspond au centre de la sphère touché *)
    let (origine, direction, centre, rayon) = (photon.photon_origine, photon.photon_direction, sphere.centre, sphere.rayon)
    in let vecteur_OC = vecteur(origine)(centre)
    in let distance_OI = produit_scalaire(vecteur_OC)(direction)
    in let rayon_carre = rayon ** 2
    in if (distance < 0.) then
        None
    else
        let distance_CI_carre = produit_scalaire(vecteur_OC)(vecteur_oc) -. distance_OI**2
        in if (distance_CI_carre > rayon_carre) then
            None (* Le point d'intersection ne touche pas le sphère *)
        else 
            let discriminant = rayon_carre - distance_CI_carre
            in let (sol1, sol2) = (distance_OI + sqrt(discriminant), distance_OI - sqrt(discriminant))
            in match (sol1 < sol2, 0.01 < sol1, 0.01 < sol2) with
                | (_, false, false) -> None
                | (true, true, _) -> let point_I = somme_vecteur_point(produit_vecteur(sol1)(direction))(origine)
                    in Some({intersect_origine = point_I; intersect_normale = unitaire(vecteur(centre)(origine))})
                | (false, _, true) -> let point_I = somme_vecteur_point(produit_vecteur(sol2)(direction))(origine)
                    in Some({intersect_origine = point_I; intersect_normale = unitaire(vecteur(centre)(origine))})


let intersect_plan photon plan =
    let (A,B,C,D) = (plan.compA, plan.compB, plan.compC, plan.compD)
    in let normale = {vx=A;vy=B;vz=C}
    in let t = -.(produit_scalaire(normale)(vecteur(photon.photon_origine))+D)/.(produit_scalaire(normale))
    in if (t > 0.01) then 
        Some({intersect_origine = somme_vecteur_point(produit(t)(photon_direction))(photon.photon_origine); intersect_normale = unitaire(vecteur(centre)(origine))})
    else    
        None