#use "topfind";;
#thread;;
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
  and y0 = ref 0.
  and max_depth = 4;;

let epsilon = 0.0005;;
let gradient = 0.002;;
let shadow = 12.;;

let background = {r=0.3;g=0.3;b=0.9};;

let world = {
    u = {vx=1.;vy=0.;vz=0.};
    v = {vx=0.;vy=1.;vz=0.};
    w = {vx=0.;vy=0.;vz=1.};
};;

let rayon_primaire camera x y = 
    let (c_origine, dir, fov, hauteur, largeur) = (camera.cam_origine, camera.cam_cible, camera.cam_fov, camera.hauteur, camera.largeur)
    (* Placement des rayons dans le repère caméra (avec la déformation nécessaire) *)
    in let aspect_ratio = float_of_int(hauteur)/.float_of_int(largeur)
    in let ajust_fov = tan(deg_rad(fov) /. 2.0);
    in let sensor_y = ((float_of_int(y) +. 0.5) /. float_of_int(largeur) *. 2.0 -. 1.0) *.ajust_fov
    in let sensor_x =  (1. -. (float_of_int(x) +. 0.5) /. float_of_int(hauteur) *. 2.0)*.ajust_fov  *. aspect_ratio 
    (* Placement de la caméra dans le monde *)
    in let (w, u) = (unitaire(dir), unitaire(produit_vectoriel(world.w)(dir)))
    in let v = produit_vectoriel(w)(u)
    in let ndir = unitaire(somme_vecteur(somme_vecteur(produit_vecteur(sensor_x)(u))(produit_vecteur(sensor_y)(v)))(w))
    (* Creation du rayon primaire *)
    in {photon_origine=c_origine; photon_direction=ndir};;
                
let estimeNormale sdf p =
    (* Estimation de la normale par gradient de la fonction sdf *)
    let x_axis = sdf({vx=p.vx+.gradient;vy=p.vy;vz=p.vz}) -. sdf({vx=p.vx-.gradient;vy=p.vy;vz=p.vz})
    and y_axis = sdf({vx=p.vx;vy=p.vy+.gradient;vz=p.vz}) -. sdf({vx=p.vx;vy=p.vy-.gradient;vz=p.vz})
    and z_axis = sdf({vx=p.vx;vy=p.vy;vz=p.vz+.gradient}) -. sdf({vx=p.vx;vy=p.vy;vz=p.vz-.gradient})
    in unitaire({vx=x_axis;vy=y_axis;vz=z_axis});;

(*let fresnel =*)

let reflect incident normale = 
    (* Calcul de la direction du rayon réflechi (un peu de trigo en somme) *)
    somme_vecteur incident (produit_vecteur(-2.0*.(produit_scalaire(incident)(normale)))(normale))

let refract iorI_ iorR_ incident normale_ =
    (* Calcul de la direction du rayon refracté s'il existe (réflexion totale) *)
    let cosi = max(-1.)(min(1.0)(produit_scalaire(unitaire(normale_))(unitaire(incident))))
    in let iorI = if cosi > 0. then iorR_ else iorI_
    and iorR = if cosi > 0. then iorI_ else iorR_
    and normale = if cosi > 0. then produit_vecteur(-1.)(normale_) else normale_
    and cosi_ = if cosi > 0. then cosi else -.cosi
    in 
        let eta = iorI/.iorR
        in let k = 1. -. (eta *. eta *. (1. -. (cosi_ *. cosi_))) (* k = 1 - eta^2 x (1 - cosi²)*)
        in if k < 0. then
            None (* Réflexion totale *)
            (*Some({vx=1.;vy=0.;vz=0.}) (* TEMPORAIRE!! *)*)
        else 
            (* eta x I + eta x cosi x sqrt(k) * N *)
            Some(somme_vecteur (produit_vecteur(eta)(incident)) (produit_vecteur(-.eta*.cosi_*.sqrt(k))(normale)))

let fresnel iorI_ iorR_ incident normale = 
    (* Équation de fresnel, permet de calculer le coefficient de réflectivité par rapport à l'angle d'incidence *)
    let cosi = produit_scalaire(unitaire(normale))(unitaire(incident))
    in let iorI = if cosi > 0. then iorR_ else iorI_
    and iorR = if cosi > 0. then iorI_ else iorR_
    in
    let sint = iorI/.iorR*.sqrt(max(0.)(1. -. cosi *.cosi))
        in if sint >= 1. then 
            1. 
        else 
            let cost = sqrt(max(0.)(1. -. sint *. sint))
            in let rs = ((iorR *. cosi) -. (iorI *. cost)) /. ((iorR *. cosi) +. (iorI *. cost))
            and rp = ((iorI *. cosi) -. (iorR *. cost)) /. ((iorI *. cosi) +. (iorR *. cost))
            in (rs *. rs +. rp *. rp) /. 2.

let rec reflectAndRefract p scene lights intersection depth iorI iorR normale =
    (* Mise en oeuvre des 3 précédentes fonctions, calcul la couleur d'un pixel en tenant compte de la rélfexion et réfraction *)
    let kr = fresnel iorI iorR p normale 
    and dehors = produit_scalaire(p)(normale) > epsilon
    and reflDir = reflect (unitaire(p)) (unitaire(normale))
    and bias = produit_vecteur(10.*.epsilon)(normale)
    in let startPointRefl = if dehors then somme_vecteur_point(produit_vecteur(-1.)(bias))(intersection) else somme_vecteur_point(bias)(intersection)
    and startPointRefr = if dehors then somme_vecteur_point(bias)(intersection) else somme_vecteur_point(produit_vecteur(-1.)(bias))(intersection)
    in let reflColor = rayon_couleur scene lights iorI ({photon_origine= startPointRefl; photon_direction=reflDir}) (depth+1)
    and refrDir = refract iorI iorR p normale
    in if kr < 1. && Option.is_some(refrDir) then
        let refrColor = rayon_couleur scene lights iorR ({photon_origine=startPointRefr; photon_direction=Option.get(refrDir)}) (depth+1)
        in mixColor(kr)(reflColor)(1.-.kr)(refrColor)
    else 
        reflColor

and rayon_couleur scene lights iorI primaire depth = 
    let (trouve, distance, etapes, _, _) = rayon_parcourt(scene)(primaire)
    in if trouve then 
        let photon_ori = avance_photon(primaire.photon_origine)(primaire.photon_direction)(distance)
        in let photon = vecteur(origine)(photon_ori)
        in let nearest = getNearestSObj(scene)(photon)
        in let normale = estimeNormale(sObjAsSDF(nearest))(photon)
        and (DiffusionValeur(diffColor, diffI), SpeculaireValeur(specColor, specI, specExp)) = (Option.get(sObjGetProperty(nearest)(Diffusion)), Option.get(sObjGetProperty(nearest)(Speculaire)))
        in let calc_lum acc light = 
            let light_dir = unitaire(vecteur(light.pos_l)(photon_ori))
            in let light_dir_inv = produit_vecteur(-1.)(light_dir)
            and diff, spec = acc
            in let trouve, distance, _, distance_pjpo, distance_pppo  = rayon_parcourt(scene)({photon_origine=light.pos_l; photon_direction=light_dir})
            in if trouve && distance+.epsilon < norme(vecteur(photon_ori)(light.pos_l)) then
                acc
            else let shad = if trouve && distance_pjpo < norme(vecteur(photon_ori)(light.pos_l)) then (shadow*.distance_pppo)/.distance_pjpo else 1.0
                in let d = max(0.)(produit_scalaire(light_dir_inv)(normale))*.shad 
                in (  
                        diff+.light.intensite_l*.d,
                        spec +. light.intensite_l*.d*.(max(0.)(produit_scalaire(produit_vecteur(-1.)(unitaire(reflect(light_dir_inv)(normale))))(unitaire(photon))**specExp))
                )
        in let diffuse, specular = List.fold_left(calc_lum)((0.0,0.0))(lights)
        in let intTotal = float_of_int(List.length(lights))
        in let k_spec = max(0.0)(specular*.specI/.intTotal)
        and k_diff = max(0.0)(diffuse*.diffI/.intTotal)
        in let k = max(0.001)(k_spec+.k_diff)
        in let mix =  mixColor(k_diff/.k)(diffColor)(k_spec/.k)(specColor)
        (*in let mix = multColor(diffI*.diffuse/.intTotal)(diffColor)
        in let mix = multColor(specular*.specI/.intTotal)(specColor)*)
        in if (depth < max_depth && Option.is_some(sObjGetProperty(nearest)(Diffusion))) then
            match sObjGetProperty(nearest)(Transparence) with 
            | Some(TransparenceValeur(k)) -> 
                let iorR = match sObjGetProperty(nearest)(IndiceOptique) with
                    | Some(IndiceOptiqueValeur(i)) -> i
                    | _ -> 1.0
                in mixColor(k)(reflectAndRefract(photon)(scene)(lights)(photon_ori)(depth)(iorI)(iorR)(normale))(1. -. k)(mix)
            | _ -> (*Printf.printf("mix: %f %f %f")(mix.r)(mix.g)(mix.b);*)mix
        else 
            mix
    else
        background

and avance_photon ori dir distance = 
        {
            x = ori.x +. distance *. dir.vx;
            y = ori.y +. distance *. dir.vy;
            z = ori.z +. distance *. dir.vz
        }

and rayon_parcourt scene primaire =
    let etapes = ref 0
    and trouve = ref false
    and sdf = cObjAsSDF(scene)
    and dir = primaire.photon_direction
    and ori = primaire.photon_origine
    and pas = ref (-.1.0)
    and distance = ref 0.0
    and distance_pppo = ref 10000.0
    and distance_pjpo = ref 10000.0
    in while ((not !trouve && !etapes < 200) && !distance <= 1000.0) do
        if ((!pas > epsilon || !pas < -.epsilon)) then
            let photon_ori = avance_photon(ori)(dir)(!distance)
            in let photon = vecteur(origine)(photon_ori)
            in begin 
                pas := sdf(photon);
                distance := !distance +. !pas;
                etapes := !etapes + 1;
                if !pas < !distance_pppo then begin
                    distance_pppo := !pas;
                    distance_pjpo := !distance
                end
            end
        else
            trouve := true
    done;
    (!trouve, !distance, !etapes, !distance_pjpo, !distance_pppo);;
(*  let photon_ori = avance_photon(ori)(dir)(!distance)
    in let photon = vecteur(origine)(photon_ori)
    in let normale = estimeNormale(sdf)(photon)
    in let nearest = getNearestSObj(scene)(photon)
    in let couleur = 
        let level = rayon_couleur lights !trouve !distance !etapes normale
        in match sObjGetProperty(nearest)(Diffusion) with 
        | Some(DiffusionValeur(color)) -> multColor(level)(color)
        (*| _ -> {
            r=(0.5*.normale.vx+.0.5); (* NORMAL *)
            g=(0.5*.normale.vy+.0.5);
            b=(0.5*.normale.vz+.0.5)
        }*)
        | _ ->  if !trouve then {r=level;g=level; b=level} else {r=0.0;g=0.6;b=0.8}
    in match sObjGetProperty(nearest)(Transparence) with 
    | Some(TransparenceValeur(k)) -> begin
        let iorR = match sObjGetProperty(nearest)(IndiceOptique) with
            | Some(IndiceOptiqueValeur(i)) -> i
            | _ -> 1.0
        in if (depth < 8) then
                if ignore_refl then 
                    let refrDir = refract 1.0 iorR photon normale
                    and dehors = produit_scalaire(photon)(normale) > 0.
                    and bias = produit_vecteur(epsilon)(normale)
                    in let hitPoint = if dehors then somme_vecteur_point(produit_vecteur(-1.)(bias))(photon_ori) else somme_vecteur_point(bias)(photon_ori)
                    in mixColor(k)(rayon_parcourt scene lights ({photon_origine=hitPoint; photon_direction=Option.get(refrDir)}) iorR (depth+1) false)(1. -. k)(couleur)
                else 
                    mixColor(k)(reflectAndRefract(photon)(scene)(lights)(photon_ori)(depth)(iorI)(iorR)(photon)(normale))(1. -. k)(couleur)
            else couleur
    end
    | _ -> couleur;;*)

let render_block camera scene lights x0 y0 xf yf=
    for x = x0 to xf do 
        for y = y0 to yf do 
            let prim = rayon_primaire(camera)(x)(y)
            in let (r,g,b) = toInvertedGamma(rayon_couleur(scene)(lights)(1.0)(prim)(1))
            in begin
                set_color(rgb(r)(g)(b));
                plot(x)(y)
            end
        done;
    done;
    synchronize();;

let render camera scene lights block_size = 
    let largeur = camera.largeur/block_size
    and hauteur = camera.hauteur/block_size
    in let x = ref (camera.hauteur/2)
        and y = ref (camera.largeur/2)
        and lim = ref 1
        and x_dir = ref (-1)
        and y_dir = ref 0
        and blocks = ref 0
        and radius = ref 1
        in while (!blocks < largeur*hauteur) do
            lim := !radius;
            while (!lim > 0) do begin
                    Thread.create (render_block camera scene lights (!x) (!y) (!x+block_size))(!y+block_size);
                    x := !x + !x_dir * block_size;
                    y := !y + !y_dir * block_size;
                    blocks := !blocks + 1;
                    lim := !lim - 1;
                end
            done;
            match (!x_dir, !y_dir, 0 < !x, !x<hauteur, 0 < !y, !y<largeur) with
            | (-1, 0,_,_,_,_) -> x_dir := 0;
                         y_dir := 1;
            | (1, 0,_,_,_,_) -> x_dir := 0;
                         y_dir := -1;
            | (0, -1,_,_,_,_) -> x_dir := -1;
                         y_dir := 0;
                         radius := !radius + 1;
            | (0, 1,_,_,_,_) -> x_dir := 1;
                         y_dir := 0;
                         radius := !radius + 1
            | (_, _, true, _, _, _) -> 
                        x_dir := -1;
                        y_dir := 0;
            | (_, _, _, _, _, true) -> 
                        x_dir := 0;
                        y_dir := 1;
                        radius := !radius + 1
            | (_, _, _, true, _, _) -> 
                        x_dir := 1;
                        y_dir := 0;
            | (_, _, _, _, true, _) -> 
                        x_dir := 0;
                        y_dir := -1;
                        radius := !radius + 1
            | _ -> failwith("Bad state")
        done;;

(*********)
(* SCENE *)
(*********)
read_key ();;

let camera = {cam_origine = {x= -1.0;y= -1.0;z= 8.0}; cam_cible = unitaire({vx= 1.;vy= 1.;vz= -1.});cam_fov=90.0; hauteur =size_x(); largeur=size_y()};;

let ma_sphere = {centre = {x= 2.;y= 2.;z= 5.0}; rayon = 2.};;
let ma_sphere_inner = {centre = {x= 2.;y= 2.;z= 5.0}; rayon = 1.8};;
let ma_sphere2 = {centre = {x= 2.;y= 2.;z= 8.0}; rayon = 0.75};;
let ma_sphere3 = {centre = {x= 8.;y= 2.;z= 4.0}; rayon = 1.};;
let ma_sphere4 = {centre = {x= 2.;y= 8.;z= 4.0}; rayon = 1.};;
let ma_sphere5 = {centre = {x= 1.;y= 2.;z= 5.5}; rayon = 0.5};;

let planOX = {compA= 1. ; compB= 0.0;compC= 0.0;compD= 7.0};;
let planOY = {compA= 0. ; compB= 1.0;compC= 0.0;compD= 7.0};;
let planOZ = {compA= 0. ; compB= 0.0;compC= 1.0;compD= 7.0};;


let sceneObj =
    let glassSphere = 
        let sph = sdfIntoSObject( opSub(sphereSDF ma_sphere)(sphereSDF ma_sphere_inner) )
        in sObjWithProperty sph Transparence (TransparenceValeur(0.7));
           sObjWithProperty sph IndiceOptique (IndiceOptiqueValeur(1.5));
           sObjWithProperty sph Diffusion (DiffusionValeur({r=0.05;g=0.;b=0.6}, 0.1));
           sObjWithProperty sph Speculaire (SpeculaireValeur({r=0.1;g=0.3;b=1.}, 0.8, 125.));
           sph
    and redSphere = 
        let sph = sdfIntoSObject( (sphereSDF ma_sphere2))
        in sObjWithProperty sph Diffusion (DiffusionValeur({r=0.9;g=0.05;b=0.05}, 0.3));
           sObjWithProperty sph Speculaire (SpeculaireValeur({r=0.9;g=0.1;b=0.1}, 0.8, 30.));
        sph
    and greenSphere = 
        let sph = sdfIntoSObject( (sphereSDF ma_sphere3))
        in sObjWithProperty sph Diffusion (DiffusionValeur({r=0.05;g=0.9;b=0.05}, 0.2));
           sObjWithProperty sph Speculaire (SpeculaireValeur({r=0.05;g=0.9;b=0.05}, 0.5, 30.));
        sph
    and littleMirror = 
        let sph = sdfIntoSObject( (sphereSDF ma_sphere4))
        in sObjWithProperty sph Transparence (TransparenceValeur(0.5));
           sObjWithProperty sph IndiceOptique (IndiceOptiqueValeur(1.5));
           sObjWithProperty sph Diffusion (DiffusionValeur({r=1.;g=1.;b=1.}, 0.1));
           sObjWithProperty sph Speculaire (SpeculaireValeur({r=1.;g=1.;b=1.}, 0.9, 1425.));
           sph
    and planZ = 
        let plan = sdfIntoSObject(planSDF planOZ)
        in sObjWithProperty plan Diffusion (DiffusionValeur({r=0.9;g=0.9;b=0.9}, 0.9));
           sObjWithProperty plan Speculaire (SpeculaireValeur({r=1.;g=1.;b=1.}, 0.5, 125.));
           plan
    in sObjListToCObj([
        (*sdfIntoSObject(planSDF planOX);
        sdfIntoSObject(planSDF planOY);*)
        planZ;
        redSphere;
        greenSphere;
        littleMirror;
        glassSphere
    ]);;           

let lightObj = [
        {pos_l={x= 1.;y= 1.;z= 2.}; intensite_l=1.};
        {pos_l={x= 1.;y= 2.;z= -2.}; intensite_l=0.9};
        {pos_l={x= 0.;y= 0.;z= 8.}; intensite_l=1.0};
    ];;
(*********)
auto_synchronize false;;
render camera sceneObj lightObj 40;;
synchronize();;
read_key();;