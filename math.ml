(* * Module Math * *)

(* Quelques types basiques pour représenter des points, vecteurs et bases *)
type point = {x: float; y: float; z: float};;
type vecteur = {vx: float; vy: float; vz: float};;
type base = {u:vecteur;v:vecteur;w:vecteur};;

(* Quelques constantes de bases : vecteur nul et origine *)
let origine = {x = 0.; y = 0.; z = 0.};;
let vec_nul = {vx = 0.; vy = 0.; vz = 0.};;

(* Quelques primitives 3D, plan et sphère basé sur leur équation cartésienne. *)
type sphere = {centre: point; rayon: float};;
type plan = {compA: float; compB: float; compC: float; compD: float};;

(* Prend deux points, en forme un vecteur au sens mathématique du terme. *)
let vecteur p1 p2 = 
    {vx = p2.x -.p1.x; vy = p2.y-.p1.y; vz = p2.z-.p1.z};;

(* Produit scalaire de deux vecteurs au sens mathématique du terme. *)
let produit_scalaire v1 v2 =
    (v2.vx*.v1.vx)+.(v2.vy*.v1.vy)+.(v2.vz*.v1.vz);;

(* Produit d'un vecteur par un scalaire au sens mathématique du terme.  *)
let produit_vecteur facteur vec =
    {vx = facteur*. vec.vx; vy=facteur *. vec.vy; vz = facteur *. vec.vz};;

(* Norme d'un vecteur dans sa base propre *)
let norme v1 =
    sqrt(v1.vx**2.+.v1.vy**2.+.v1.vz**2.);;

(* 
 * Donne le vecteur de norme 1 du vecteur donné. 
 * Aucune garantie sur le résultat donné pour le vecteur nul.
 *)
let unitaire v1 =
    produit_vecteur(1./.(norme v1))(v1)

(* Produit vectorielle de deux vecteurs au sens mathématique du terme. *)
let produit_vectoriel v1 v2 =
    {vx = v1.vy *. v2.vz -. v1.vz *. v2.vy;
    vy = v1.vz *. v2.vx -. v1.vx *. v2.vz;
    vz = v1.vx *. v2.vy -. v1.vy *. v2.vx};;

(* Somme de deux vecteurs au sens mathématique du terme. *)
let somme_vecteur v1 v2 =
    {vx = v1.vx +. v2.vx;
    vy = v1.vy +. v2.vy;
    vz = v1.vz +. v2.vz;};;

(* Différence de deux vecteurs au sens mathématique du terme. *)
let difference_vecteur v1 v2 = (* Par simplicité, on utilise pas produit_vecteur et somme_vecteur *)
    {vx = v1.vx -. v2.vx;
    vy = v1.vy -. v2.vy;
    vz = v1.vz -. v2.vz;};;

(* Somme d'un vecteur à un point au sens mathématique du terme. *)
let somme_vecteur_point v1 p1 =
    {x = v1.vx +. p1.x;
    y = v1.vy +. p1.y;
    z = v1.vz +. p1.z;};;

(* 
 * Modulo vectorielle 
 * Applique l'opérateur modulo à chaque composante du premier vecteur par le premier du vecteur modulo.
 *)
let mod_vecteur v1 vmod =
    {
        vx = mod_float(v1.vx)(vmod.vx);
        vy = mod_float(v1.vy)(vmod.vy);
        vz = mod_float(v1.vz)(vmod.vz);
    };;

(* 
 * Valeur absolu vectorielle 
 * Applique l'opérateur valeur absolu à chaque composante du vecteur.
 *)
let abs_vecteur v1 =
    {
        vx = abs_float(v1.vx);
        vy = abs_float(v1.vy);
        vz = abs_float(v1.vz)
    }

(*
 * Distance de deux points dans la base supposée commune et euclidienne au deux.
 * Équivalent à norme(vecteur(p1)(p2)).
 *)
let distance_points p1 p2 =
    sqrt((p2.x-.p1.x)**2.+.(p2.y-.p1.y)**2.+.(p2.z-.p1.z)**2.);;

(*
 * Convertit un angle en dégré en radian. 
 *)
let deg_rad deg = deg*.Float.pi/.180.0
