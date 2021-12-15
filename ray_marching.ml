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
    in let (w, u) = (unitaire(dir), unitaire(produit_vectoriel(world.w)(dir)))
    in let v = produit_vectoriel(w)(u)
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
    let cosi = max(-1.)(min(1.0)(produit_scalaire(unitaire(normale_))(unitaire(incident))))
    in let iorI = if cosi > 0. then iorR_ else iorI_
    and iorR = if cosi > 0. then iorI_ else iorR_
    and normale = if cosi > 0. then produit_vecteur(-1.)(normale_) else normale_
    and cosi_ = if cosi > 0. then cosi else -.cosi
    in 
        let eta = iorI/.iorR
        in let k = 1. -. (eta *. eta *. (1. -. (cosi_ *. cosi_))) (* k = 1 - eta^2 x (1 - cosi²)*)
        in if k < 0. then
            None
        else 
            (* eta x I + eta x cosi x sqrt(k) * N *)
            Some(somme_vecteur (produit_vecteur(eta)(incident)) (produit_vecteur(eta*.cosi_*.sqrt(k))(normale)))

let fresnel iorI_ iorR_ incident normale = 
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

let rec reflectAndRefract p scene intersection depth iorI iorR incident normale =
    let kr = fresnel iorI iorR incident normale 
    and dehors = produit_scalaire(p)(normale) > 0.
    and reflDir = reflect (unitaire(incident)) (unitaire(normale))
    and bias = produit_vecteur(epsilon)(normale)
    in let startPointRefl = if dehors then somme_vecteur_point(produit_vecteur(-1.)(bias))(intersection) else somme_vecteur_point(bias)(intersection)
    and startPointRefr = if dehors then somme_vecteur_point(bias)(intersection) else somme_vecteur_point(produit_vecteur(-1.)(bias))(intersection)
    in let reflColor = rayon_parcourt scene ({photon_origine= startPointRefl; photon_direction=reflDir}) (depth+1) false
    and refrDir = refract iorI iorR incident normale
    in if kr < 1. && Option.is_some(refrDir) then
        let refrColor = rayon_parcourt scene ({photon_origine=startPointRefr; photon_direction=Option.get(refrDir)}) (depth+1) false
        in mixColor(kr)(reflColor)(1.-.kr)(refrColor)
    else 
        reflColor

and rayon_couleur trouve distance etapes normale = 
    let scale = min(1.0)(produit_scalaire(lumiere)(normale))
    in let degrad = 1. (*0.05*.(-.exp(4.*.((min(distance)(1000.))/.1000.)-.1.0)+.20.2)*)
    in if trouve then min(1.0)(max(0.05)((max(0.)(scale))*.degrad)) else 0.

and rayon_parcourt scene primaire depth ignore_refl =
    let etapes = ref 0
    and trouve = ref false
    and sdf = cObjAsSDF(scene)
    and dir = primaire.photon_direction
    and ori = primaire.photon_origine
    and pas = ref (-.1.0)
    and distance = ref 0.0
    in while ((not !trouve && !etapes < 100) && !distance <= 1000.0) do
        if ((!pas > epsilon || !pas < -.epsilon)) then
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
        (*| _ -> {
            r=(0.5*.normale.vx+.0.5); (* NORMAL *)
            g=(0.5*.normale.vy+.0.5);
            b=(0.5*.normale.vz+.0.5)
        }*)
        | _ ->  {r=level;g=level; b=level}
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
                    in mixColor(k)(rayon_parcourt scene ({photon_origine=hitPoint; photon_direction=Option.get(refrDir)}) (depth+1) false)(1. -. k)(couleur)
                else 
                    mixColor(k)(reflectAndRefract(photon)(scene)(photon_ori)(depth)(1.0)(iorR)(photon)(normale))(1. -. k)(couleur)
            else couleur
    end
    | _ -> couleur;;

let render_block camera scene x0 y0 xf yf=
    for x = x0 to xf do 
        for y = y0 to yf do 
            let prim = rayon_primaire(camera)(x)(y)
            in let (r,g,b) = toInvertedGamma(rayon_parcourt(scene)(prim)(1)(false))
            (*in let color = if trouve then int_of_float(245.0*.min(float_of_int(pas)/.dist/.3.)(1.0)) else 0*)
            in begin
                (*Printf.printf("pixel x: %d; y: %d; couleur: %d\n")(x)(y)(color);*)
                set_color(rgb(r)(g)(b));
                fill_rect(x*1)(y*1)(1)(1)
            end
        done;
    done;;

let render camera scene block_size = 
    let largeur = camera.largeur/20
    and hauteur = camera.hauteur/20
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
                    render_block camera scene (!x) (!y) (!x+block_size) (!y+block_size);
                    x := !x + !x_dir * block_size;
                    y := !y + !y_dir * block_size;
                    blocks := !blocks + 1;
                    lim := !lim - 1;
                    synchronize()
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
let ma_sphere2 = {centre = {x= 8.;y= 8.;z= 3.0}; rayon = 2.};;
let ma_sphere3 = {centre = {x= 8.;y= 2.;z= 4.0}; rayon = 1.};;
let ma_sphere4 = {centre = {x= 2.;y= 8.;z= 4.0}; rayon = 1.};;
let ma_sphere5 = {centre = {x= 1.;y= 2.;z= 5.5}; rayon = 0.5};;

let planOX = {compA= 1. ; compB= 0.0;compC= 0.0;compD= 7.0};;
let planOY = {compA= 0. ; compB= 1.0;compC= 0.0;compD= 7.0};;
let planOZ = {compA= 0. ; compB= 0.0;compC= 1.0;compD= 7.0};;


let sceneObj =
    let glassSphere = 
        let sph = sdfIntoSObject( (sphereSDF ma_sphere))
        in sObjWithProperty sph Transparence (TransparenceValeur(0.9));
           sObjWithProperty sph IndiceOptique (IndiceOptiqueValeur(1.5));
           sObjWithProperty sph Couleur (CouleurValeur({r=0.05;g=0.05;b=0.6}));
           sph
    and redSphere = 
        let sph = sdfIntoSObject( (sphereSDF ma_sphere2))
        in sObjWithProperty sph Couleur (CouleurValeur({r=1.0;g=0.05;b=0.05}));
        sph
    and greenSphere = 
        let sph = sdfIntoSObject( (sphereSDF ma_sphere3))
        in sObjWithProperty sph Couleur (CouleurValeur({r=0.05;g=1.0;b=0.05}));
        sph
    and littleMirror = 
        let sph = sdfIntoSObject( (sphereSDF ma_sphere4))
        in sObjWithProperty sph Transparence (TransparenceValeur(0.8));
           sObjWithProperty sph IndiceOptique (IndiceOptiqueValeur(1.5));
           sph
    in sObjListToCObj([
        sdfIntoSObject(planSDF planOX);
        sdfIntoSObject(planSDF planOY);
        sdfIntoSObject(planSDF planOZ);
        redSphere;
        greenSphere;
        littleMirror;
        glassSphere
    ]);;           
(*********)
auto_synchronize false;;
render camera sceneObj 40;;
synchronize();;
read_key();;