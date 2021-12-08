#use "topfind";;
#require "graphics";;
#use "./math.ml";;
#use "./sdf.ml";;

(*open Vecteur;;*)
open Graphics;;


open_graph ":0";;

type camera = {cam_origine: point; cam_cible: vecteur; cam_fov: float; hauteur: int; largeur: int};;
type photon = {photon_origine: point; photon_direction: vecteur};;

let x0 = ref 0.
  and y0 = ref 0.;;
let lumiere = unitaire({vx=0.1;vy=3.0;vz= 5.});;

let epsilon = 0.0005;;
let gradient = 0.002;;

let world = {
    u = {vx=1.;vy=0.;vz=0.};
    v = {vx=0.;vy=1.;vz=0.};
    w = {vx=0.;vy=0.;vz=1.};
};;

let rayon_primaire camera x y = 
    (* Note: Ne prend pas encore en compte la cible de la caméra !! TODO *)
    let (c_origine, dir, fov, hauteur, largeur) = (camera.cam_origine, camera.cam_cible, camera.cam_fov, camera.hauteur, camera.largeur)
    in let aspect_ratio = float_of_int(hauteur)/.float_of_int(largeur)
    in let ajust_fov = tan(deg_rad(fov) /. 2.0);
    in let sensor_y = ((float_of_int(y) +. 0.5) /. float_of_int(largeur) *. 2.0 -. 1.0) *.ajust_fov
    in let sensor_x =  (1. -. (float_of_int(x) +. 0.5) /. float_of_int(hauteur) *. 2.0)*.ajust_fov  *. aspect_ratio 
    in let (w, u) = (unitaire(dir), unitaire(produit_vectorielle(world.w)(dir)))
    in let v = produit_vectorielle(w)(u)
    in let ndir = unitaire(somme_vecteur(somme_vecteur(produit_vecteur(sensor_x)(u))(produit_vecteur(sensor_y)(v)))(w))
    in {photon_origine=c_origine; photon_direction=ndir};;
                (* sensor_x * u + sensor_y * v + w *)
                
let estimeNormale sdf p =
    let x_axis = sdf({vx=p.vx+.gradient;vy=p.vy;vz=p.vz}) -. sdf({vx=p.vx-.gradient;vy=p.vy;vz=p.vz})
    and y_axis = sdf({vx=p.vx;vy=p.vy+.gradient;vz=p.vz}) -. sdf({vx=p.vx;vy=p.vy-.gradient;vz=p.vz})
    and z_axis = sdf({vx=p.vx;vy=p.vy;vz=p.vz+.gradient}) -. sdf({vx=p.vx;vy=p.vy;vz=p.vz-.gradient})
    in unitaire({vx=x_axis;vy=y_axis;vz=z_axis});;

let rayon_parcourt sdf primaire =
    let etapes = ref 0
    and trouve = ref false
    and dir = primaire.photon_direction
    and ori = primaire.photon_origine
    and pas = ref (-.1.0)
    and distance = ref 0.0
    in while ((not !trouve && !etapes < 100) && !distance <= 1000.0) do
        if (!pas = -.1.0 || (!pas > epsilon && !pas > 0.)) then
            let photon_ori = 
                {
                    x = ori.x +. !distance *. dir.vx;
                    y = ori.y +. !distance *. dir.vy;
                    z = ori.z +. !distance *. dir.vz
                }
            in let photon = vecteur(origine)(photon_ori)
            in begin 
                pas := sdf(photon);
                distance := !distance +. !pas;
                etapes := !etapes + 1
            end
        else
            trouve := true
    done;
    let photon_ori = {
                    x = ori.x +. !distance *. dir.vx;
                    y = ori.y +. !distance *. dir.vy;
                    z = ori.z +. !distance *. dir.vz
                }
    in let photon = vecteur(origine)(photon_ori)
    in let normale = estimeNormale(sdf)(photon)
    in (!trouve, !distance, !etapes, normale)

let dessine camera sdf =
    for x = 0 to camera.hauteur do 
        for y = 0 to camera.largeur do 
            let prim = rayon_primaire(camera)(x)(y)
            in let (trouve, dist, pas, normale) = rayon_parcourt(sdf)(prim)
            in let scale = produit_scalaire(lumiere)(normale)
            in let color = if trouve then int_of_float(max(0.)(scale*.255.0)) else 0
            (*in let color = if trouve then int_of_float(245.0*.min(float_of_int(pas)/.dist/.3.)(1.0)) else 0*)
            in begin
                Printf.printf("pixel x: %d; y: %d; distance parcourue: %f; nombre de pas: %d; normale_x: %f, normale_y: %f, normale_z:%f; prod: %f\n")(x)(y)(dist)(pas)(normale.vx)(normale.vy)(normale.vz)(scale);
                set_color(rgb(color)(color)(color));
                fill_rect(x*1)(y*1)(1)(1)
            end
        done;
        if (x mod 40 = 0) then
            synchronize();
    done;;


(*********)
(* SCENE *)
(*********)


let camera = {cam_origine = {x= -1.0;y= -1.0;z= 8.0}; cam_cible = unitaire({vx= 1.;vy= 1.;vz= -1.});cam_fov=90.0; hauteur =size_x(); largeur=size_y()};;

let ma_sphere = {centre = {x= 2.;y= 2.;z= 5.0}; rayon = 1.5};;

let planOX = {compA= 1. ; compB= 0.0;compC= 0.0;compD= 7.0};;
let planOY = {compA= 0. ; compB= 1.0;compC= 0.0;compD= 7.0};;
let planOZ = {compA= 0. ; compB= 0.0;compC= 1.0;compD= 7.0};;


let sceneSDF =
    foldOp(opUnion)([
        planSDF planOX;
        planSDF planOY;
        planSDF planOZ;
        sphereSDF ma_sphere;
    ]);;           
(*********)

auto_synchronize false;;
dessine camera sceneSDF;;
synchronize();;
read_key();;