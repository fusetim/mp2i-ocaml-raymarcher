#use "topfind";;
#require "graphics";;

(*open Vecteur;;*)
open Graphics;;

open_graph ":0";;
type point = {x: float; y: float; z: float};;
type vecteur = {vx: float; vy: float; vz: float};;
type base = {u:vecteur;v:vecteur;w:vecteur};;

type camera = {cam_origine: point; cam_cible: vecteur; cam_fov: float; hauteur: int; largeur: int};;
type photon = {photon_origine: point; photon_direction: vecteur};;
type intersection = {intersect_origine: point; intersect_normale: vecteur};;
type sphere = {centre: point; rayon: float};;
type plan = {compA: float; compB: float; compC: float; compD: float};;

let x0 = ref 0.
  and y0 = ref 0.;;


let origine = {x = 0.; y = 0.; z = 0.};;
let vec_nul = {vx = 0.; vy = 0.; vz = 0.};;
let world = {
    u = {vx=1.;vy=0.;vz=0.};
    v = {vx=0.;vy=1.;vz=0.};
    w = {vx=0.;vy=0.;vz=1.};
};;


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

let distance_points p1 p2 =
    sqrt((p1.x-.p2.x)**2.+.(p1.y-.p2.y)**2.+.(p1.z-.p2.z)**2.);;

let deg_rad deg = deg*.Float.pi/.180.0

let rayon_primaire camera x y = 
    (* Note: Ne prend pas encore en compte la cible de la caméra !! TODO *)
    let (c_origine, dir, fov, hauteur, largeur) = (camera.cam_origine, camera.cam_cible, camera.cam_fov, camera.hauteur, camera.largeur)
    in let aspect_ratio = float_of_int(hauteur)/.float_of_int(largeur)
    in let ajust_fov = tan(deg_rad(fov) /. 2.0);
    in let sensor_y = ((float_of_int(x) +. 0.5) /. float_of_int(hauteur) *. 2.0 -. 1.0) *. aspect_ratio *.ajust_fov
    in let sensor_x =  (1.0 -. (float_of_int(y) +. 0.5) /. float_of_int(largeur) *. 2.0)*.ajust_fov
    in let (w, u) = (unitaire(dir), unitaire(produit_vectorielle(world.w)(dir)))
    in let v = produit_vectorielle(w)(u)
    in let ndir = unitaire(somme_vecteur(somme_vecteur(produit_vecteur(sensor_x)(u))(produit_vecteur(sensor_y)(v)))(w))
    in {photon_origine=c_origine; photon_direction=ndir};;
                (* sensor_x * u + sensor_y * v + w *)
                

let camera = {cam_origine = {x=0.0;y=0.;z=0.}; cam_cible = unitaire({vx= 0.1;vy= 0.1;vz= -1.});cam_fov=90.0; hauteur =size_x(); largeur=size_y()};;

let sphereDE photon_ sphere_ =
        (* On se permet de représenter la situation avec les points suivant:
        * O correspond à l'origine porté par le photon
        * I correspond au potentiel point d'intersection 
        * C correspond au centre de la sphère touché *)
    let (origin, direction,centre, rayon) = (photon_.photon_origine, photon_.photon_direction, sphere_.centre, sphere_.rayon)
    in let distance_au_centre = distance_points(origin)(centre)
    in distance_au_centre -. rayon

let sphereSBF s p =
(* Prend la sphère et le vecteur directeur du photon *)
    distance_points(s.centre)(p) -. s.rayon

let opRep photon period de =
    let pseudophoton = difference_vecteur(mod_vecteur(somme_vecteur(vecteur(origine)(photon.photon_origine))(produit_vecteur(0.5)(period)))(period))(produit_vecteur(0.5)(period))
    in de({photon_origine=origine; photon_direction=pseudophoton});;

let planSBF pl p  =
    let (a,b,c,d) = (pl.compA, pl.compB, pl.compC, pl.compD)
    in produit_scalaire(unitaire({vx=a;vy=b;vz=c}))(vecteur(origine)(p)) +. d

let ma_sphere = {centre = {x=0.;y=0.;z= -5.0}; rayon = 1.5};;
let ma_sphere2 = {centre = {x=0.5;y=0.5;z= -4.0}; rayon = 0.5};;
let ma_sphere3 = {centre = {x=5.;y= -2.;z= -3.0}; rayon = 0.5};;


let mon_plan = {compA= 0.0 ; compB= 0.0;compC= 10.0;compD= 7.0};;

let esp = 0.0005;;

let rayon_parcourt primaire =
    let etapes = ref 0
    and trouve = ref false
    and dir = primaire.photon_direction
    and ori = primaire.photon_origine
    and pas = ref (-.1.0)
    and distance = ref 0.0
    in while ((not !trouve && !etapes < 100) && !distance <= 1000.0) do
        if (!pas = -.1.0 || (!pas > esp && !pas > 0.)) then
            let photon_ori = if (!pas = -.1.0) then
                ori
                else
                {
                    x = ori.x +. !distance *. dir.vx;
                    y = ori.y +. !distance *. dir.vy;
                    z = ori.z +. !distance *. dir.vz
                }
            in let photon = {photon_direction=dir; photon_origine= photon_ori }
            in begin 
                pas :=  (* min(min(max(sphereDE(photon)(ma_sphere))(-.sphereDE(photon)(ma_sphere2)))(planDE(photon)(mon_plan)))(sphereDE(photon)(ma_sphere3));  (*sphereDE(photon)(ma_sphere); *)
                    (*opRep(photon)({vx=2.0;vy=2.0;vz=2.0})(fun x -> sphereDE(x)(ma_sphere));*)*)
                    min(sphereSBF(ma_sphere)(photon_ori))(planSBF(mon_plan)(photon_ori));
                distance := !distance +. !pas;
                etapes := !etapes + 1
            end
        else
            trouve := true
    done;
    (!trouve, !distance, !etapes)

let dessine camera =
    for y = 0 to camera.hauteur do 
        for x = 0 to camera.largeur do 
            let prim = rayon_primaire(camera)(x)(y)
            in let (trouve, dist, pas) = rayon_parcourt(prim)
            in let color = if trouve then int_of_float(255.0*.min(float_of_int(pas)/.dist/.2.)(1.0)) else 0
            in begin
                (*Printf.printf("pixel x: %d; y: %d; distance parcourue: %f; nombre de pas: %d\n")(x)(y)(dist)(pas);*)
                set_color(rgb(color)(color)(color));
                fill_rect(y*1)(x*1)(1)(1)
            end
        done;
        if (y mod 40 = 0) then
            synchronize();
    done;;

auto_synchronize false;;
dessine camera;;
synchronize();;
read_key();;