#use "topfind";;
#require "graphics";;
#use "./math.ml";;
#use "./sdf.ml";;
#use "./object.ml";;

(*open Vecteur;;*)
open Graphics;;


open_graph ":0";;

type camera = {cam_origine: point; cam_cible: vecteur; cam_fov: float; hauteur: int; largeur: int};;
type photon = {photon_origine: point; photon_direction: vecteur};;


let x0 = ref 0.
  and y0 = ref 0.;;
let lumiere = unitaire({vx=1.;vy= -1.;vz= 3.});;

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

(*let fresnel =*)

let reflect incident normale = 
    somme_vecteur incident (produit_vecteur(-2.0*.(produit_scalaire(incident)(normale)))(normale))

let refract iorI_ iorR_ incident normale_ =
    let cosi = produit_scalaire(unitaire(normale_))(unitaire(incident))
    in let iorI = if cosi < 0. then iorR_ else iorI_
    and iorR = if cosi < 0. then iorI_ else iorR_
    and normale = if cosi < 0. then produit_vecteur(-1.)(normale_) else normale_
    in 
        let eta = iorI/.iorR
        in let k = 1. -. (eta *. eta *. (1. -. (cosi *. cosi))) (* k = 1 - eta^2 x (1 - cosi²)*)
        in if k < 0. then
            None
        else 
            (* eta x I + eta x cosi x sqrt(k) * N *)
            Some(somme_vecteur (produit_vecteur(eta)(incident)) (produit_vecteur(eta*.cosi*.sqrt(k))(normale)))

let fresnel iorI_ iorR_ incident normale = 
    let cosi = produit_scalaire(unitaire(normale))(unitaire(incident))
    in let iorI = if cosi > 0. then iorR_ else iorI_
    and iorR = if cosi > 0. then iorI_ else iorR_
    in
    let sint = iorI/.iorR*.sqrt(max(0.)(1. -. cosi *.cosi))
        in if sint > 1. then 
            1. 
        else 
            let cost = sqrt(max(0.)(1. -. sint *. cosi))
            in let rs = ((iorR *. cosi) -. (iorI *. cost)) /. ((iorR *. cosi) +. (iorI *. cost))
            and rp = ((iorI *. cosi) -. (iorR *. cost)) /. ((iorI *. cosi) +. (iorR *. cost))
            in (rs *. rs +. rp *. rp) /. 2.

let rec reflectAndRefract scene intersection depth iorI iorR incident normale =
    let kr = fresnel iorI iorR incident normale 
(*)    and dehors = produit_scalaire(p)(normale) < 0 ; utile pour les displacement maps*)
    and reflDir = reflect (unitaire(incident)) (unitaire(normale))
    in let reflColor = rayon_parcourt scene ({photon_origine= intersection; photon_direction=reflDir}) (depth+1)
    and refrDir = refract iorI iorR incident normale
    in if kr < 1. && Option.is_some(refrDir) then
        let refrColor = rayon_parcourt scene ({photon_origine=intersection; photon_direction=Option.get(refrDir)}) (depth+1)
        in addColor(multColor(kr)(reflColor))(multColor(1. -. kr)(refrColor))
    else 
        reflColor 

and rayon_couleur trouve distance etapes normale = 
    let scale = min(1.0)(produit_scalaire(lumiere)(normale))
    in let degrad = 1. (*0.05*.(-.exp(4.*.((min(distance)(1000.))/.1000.)-.1.0)+.20.2)*)
    in if trouve then max(5.0)((max(0.)(scale))*.degrad*.250.0) else 0.

and rayon_parcourt scene primaire depth =
    let etapes = ref 0
    and trouve = ref false
    and sdf = cObjAsSDF(scene)
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
    in let nearest = getNearestSObj(scene)(photon)
    in let couleur = 
        let level = rayon_couleur !trouve !distance !etapes normale
        in match sObjGetProperty(nearest)(Couleur) with 
        | Some(CouleurValeur(color)) -> multColor(level)(color)
        | _ -> {
            r=int_of_float(125.*.normale.vx+.125.);
            g=int_of_float(125.*.normale.vy+.125.);
            b=int_of_float(125.*.normale.vz+.125.)
        }
        (*| _ -> let rgb = int_of_float(level) in {r=rgb;g=rgb; b=rgb}*)
    in match sObjGetProperty(nearest)(Transparence) with 
    | Some(TransparenceValeur(k)) -> begin
        let iorR = match sObjGetProperty(nearest)(IndiceOptique) with
            | Some(IndiceOptiqueValeur(i)) -> i
            | _ -> 1.0
        in if (depth < 5) then
                addColor(multColor(k)(reflectAndRefract(scene)(photon_ori)(depth)(1.0)(iorR)(photon)(normale)))(multColor(1. -. k)(couleur))
            else couleur
    end
    | _ -> couleur;;

let dessine camera scene =
    for x = 0 to camera.hauteur do 
        for y = 0 to camera.largeur do 
            let prim = rayon_primaire(camera)(x)(y)
            in let color = rayon_parcourt(scene)(prim)(1)
            (*in let color = if trouve then int_of_float(245.0*.min(float_of_int(pas)/.dist/.3.)(1.0)) else 0*)
            in begin
                (*Printf.printf("pixel x: %d; y: %d; couleur: %d\n")(x)(y)(color);*)
                set_color(rgb(color.r)(color.g)(color.b));
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

let ma_sphere = {centre = {x= 2.;y= 2.;z= 5.0}; rayon = 2.};;
let ma_sphere2 = {centre = {x= 4.;y= 4.;z= 4.0}; rayon = 1.};;

let planOX = {compA= 1. ; compB= 0.0;compC= 0.0;compD= 7.0};;
let planOY = {compA= 0. ; compB= 1.0;compC= 0.0;compD= 7.0};;
let planOZ = {compA= 0. ; compB= 0.0;compC= 1.0;compD= 7.0};;


let sceneObj =
    let glassSphere = 
        let sph = sdfIntoSObject( (sphereSDF ma_sphere))
        in (*sObjWithProperty sph Transparence (TransparenceValeur(0.9));*)
           (*sObjWithProperty sph IndiceOptique (IndiceOptiqueValeur(1.5));*)
           (*sObjWithProperty sph Couleur (CouleurValeur({r=20;g=20;b=255}));*)
           sph
    and redSphere = 
        let sph = sdfIntoSObject( (sphereSDF ma_sphere2))
        in (*sObjWithProperty sph Couleur (CouleurValeur({r=255;g=20;b=20}));*)
        sph
    in sObjListToCObj([
        sdfIntoSObject(planSDF planOX);
        sdfIntoSObject(planSDF planOY);
        sdfIntoSObject(planSDF planOZ);
        redSphere;
        glassSphere
    ]);;           
(*********)

auto_synchronize false;;
dessine camera sceneObj;;
synchronize();;
read_key();;