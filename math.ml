type point = {x: float; y: float; z: float};;
type vecteur = {vx: float; vy: float; vz: float};;
type base = {u:vecteur;v:vecteur;w:vecteur};;

let origine = {x = 0.; y = 0.; z = 0.};;
let vec_nul = {vx = 0.; vy = 0.; vz = 0.};;

type sphere = {centre: point; rayon: float};;
type plan = {compA: float; compB: float; compC: float; compD: float};;

let vecteur p1 p2 = 
    {vx = p2.x -.p1.x; vy = p2.y-.p1.y; vz = p2.z-.p1.z};;

let produit_scalaire v1 v2 =
    (v2.vx*.v1.vx)+.(v2.vy*.v1.vy)+.(v2.vz*.v1.vz);;

let norme v1 =
    sqrt(v1.vx**2.+.v1.vy**2.+.v1.vz**2.);;

let unitaire v1 =
    let normev = norme v1 in
        {vx = v1.vx/. normev; vy = v1.vy/. normev; vz = v1.vz/. normev};;

let produit_vectoriel v1 v2 =
    {vx = v1.vy *. v2.vz -. v1.vz *. v2.vy;
    vy = v1.vz *. v2.vx -. v1.vx *. v2.vz;
    vz = v1.vx *. v2.vy -. v1.vy *. v2.vx};;

let produit_vecteur facteur vec =
    {vx = facteur*. vec.vx; vy=facteur *. vec.vy; vz = facteur *. vec.vz};;

let somme_vecteur v1 v2 =
    {vx = v1.vx +. v2.vx;
    vy = v1.vy +. v2.vy;
    vz = v1.vz +. v2.vz;};;

let difference_vecteur v1 v2 =
    {vx = v1.vx -. v2.vx;
    vy = v1.vy -. v2.vy;
    vz = v1.vz -. v2.vz;};;

let somme_vecteur_point v1 p1 =
    {x = v1.vx +. p1.x;
    y = v1.vy +. p1.y;
    z = v1.vz +. p1.z;};;

let mod_vecteur v1 vmod =
    {
        vx = mod_float(v1.vx)(vmod.vx);
        vy = mod_float(v1.vy)(vmod.vy);
        vz = mod_float(v1.vz)(vmod.vz);
    };;

let abs_vecteur v1 =
    {
        vx = abs_float(v1.vx);
        vy = abs_float(v1.vy);
        vz = abs_float(v1.vz)
    }

let distance_points p1 p2 =
    sqrt((p2.x-.p1.x)**2.+.(p2.y-.p1.y)**2.+.(p2.z-.p1.z)**2.);;

let deg_rad deg = deg*.Float.pi/.180.0
