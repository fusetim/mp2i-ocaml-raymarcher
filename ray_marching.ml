#use "topfind";;
#require "graphics";;
#use "./math.ml";;
#use "./sdf.ml";;
#use "./object.ml";;

(* MODULE RAYMARCHING - Module principale *)

open Graphics;;
open_graph ":0";;

(* Primitives pour représenter caméra et rayon / photon *)
type camera = {cam_origine: point; cam_cible: vecteur; cam_fov: float; hauteur: int; largeur: int};;
type photon = {photon_origine: point; photon_direction: vecteur};;

(* Quelques constantes *)
let x0 = ref 0. 
  and y0 = ref 0.
  and max_depth = 4 (* Limite de profondeur/récursion : 4 rayons de réflection/réfraction de prondeur maximum *)
  and epsilon = 0.005 (* Epsilon du ray-marcher : petite distance à partir de laquelle on considère avoir toucher l'objet *)
  and gradient = 0.02 (* Epsilon du gradient : petite distance autour de l'objet pour déterminer le gradient du SDF (Signed Distance Function *)
  and shadow = 8. (* Constante d'ombre, détermine la douceur des ombres (how soft soft-shadow are) *)
  and background = {r=0.3;g=0.3;b=0.9} (* Détermine la couleur du ciel, càd la couleur par défaut quand on ne touche rien *)
  and world = { (* Base "monde" *)
        u = {vx=1.;vy=0.;vz=0.};
        v = {vx=0.;vy=1.;vz=0.};
        w = {vx=0.;vy=0.;vz=1.};
    };;

(*
 * Détermine à l'aide des propriétés de la caméra et des coordonnées du pixel, le rayon
 * (origine, direction) depuis la caméra à calculer.
 *)
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

(* Estimation de la normale par gradient de la fonction SDF (Signed Distance Function) *)             
let estimeNormale sdf p =
    let x_axis = sdf({vx=p.vx+.gradient;vy=p.vy;vz=p.vz}) -. sdf({vx=p.vx-.gradient;vy=p.vy;vz=p.vz})
    and y_axis = sdf({vx=p.vx;vy=p.vy+.gradient;vz=p.vz}) -. sdf({vx=p.vx;vy=p.vy-.gradient;vz=p.vz})
    and z_axis = sdf({vx=p.vx;vy=p.vy;vz=p.vz+.gradient}) -. sdf({vx=p.vx;vy=p.vy;vz=p.vz-.gradient})
    in unitaire({vx=x_axis;vy=y_axis;vz=z_axis});;

(* Calcule de la direction du rayon réflechi. (un peu de trigonométrie et de Loi de Snell-Descartes en somme) *)
let reflect incident normale = 
    unitaire (somme_vecteur incident (produit_vecteur(-2.0*.(produit_scalaire(incident)(normale)))(normale)))

(* 
 * Calcule de la direction du rayon réfracté.
 * Plus difficile cette fois car il faut résoudre l'équation et savoir si oui ou non il y a une solution
 * (réflexion totale ou non) avant de le calculer avec les lois de Snell-Descartes. 
 *)
let refract iorI_ iorR_ incident normale_ =
    (* Calcul de la direction du rayon refracté s'il existe (réflexion totale) *)
    let cosi = max(-1.)(min(1.0)(produit_scalaire(unitaire(normale_))(unitaire(incident))))
    (* 
     * Selon le signe du cosinus, on peut déterminer si on est actuellement dans ou en dehors de l'objet,
     * et donc si on va avoir affaire avec la réfraction limite ou la réflecion totale 
     *)
    in let iorI = if cosi > 0. then iorI_ else iorR_
    and iorR = if cosi > 0. then iorR_ else iorI_
    and normale = if cosi > 0. then normale_ else  produit_vecteur(-1.)(normale_)
    and cosi_ = if cosi < 0. then -.cosi else cosi
    in 
        let eta = iorI/.iorR
        in let k = 1. -. (eta *. eta *. (1. -. (cosi_ *. cosi_))) 
        in if k < 0. then
            None (* Réfraction limite / Réflexion totale *)
        else 
            (* 
             * Calcul du vecteur directeur du rayon réfracté.
             * Il faudrait que je me penche un peu plus sur cette équation car son calcul change selon les sources
             * avec un résultat néanmoins similaire. Sans doute une factorisation différente.
             *)
            Some(unitaire(somme_vecteur (produit_vecteur(eta)(incident)) (produit_vecteur(eta*.cosi_ -. sqrt(k))(normale))))

(* 
 * Équation de fresnel, permet de calculer le coefficient de réflectivité par rapport à l'angle d'incidence .
 *
 * En effet, un objet est davantage refléchissant lorsque l'angle d'incidence et très grand par rapport à sa normale. 
 * Un phénomène que l'on retrouve lorsque l'on regarde les lacs.
 *
 * Je n'en expliquerais pas plus sur cette fonction.
 *)
let fresnel iorI_ iorR_ incident normale = 
    let cosi = produit_scalaire(unitaire(normale))(unitaire(incident))
    in let iorI = if cosi > 0. then iorI_ else iorR_
    and iorR = if cosi > 0. then iorR_ else iorI_
    in
    let sint = iorI/.iorR*.sqrt(max(0.)(1. -. cosi *.cosi))
        in if sint >= 1. then 
            1. 
        else 
            let cost = sqrt(max(0.)(1. -. sint *. sint))
            in let rs = ((iorR *. cosi) -. (iorI *. cost)) /. ((iorR *. cosi) +. (iorI *. cost))
            and rp = ((iorI *. cosi) -. (iorR *. cost)) /. ((iorI *. cosi) +. (iorR *. cost))
            in (rs *. rs +. rp *. rp) /. 2.

(*
 * Mise en oeuvre des 3 précédentes fonctions, calcule la couleur d'un point en tenant compte
 *  de la réflexion, réfraction et des équations de Fresnel. Elle fait alors un mix qui sera ajouté 
 * à la couleur du pixel final.
 *)
let rec reflectAndRefract p scene lights intersection depth iorI iorR normale =
    let kr = fresnel iorI iorR p normale 
    and dehors = produit_scalaire(p)(normale) > epsilon (* si le point est à l'intérieur ou en dehors de l'objet *)
    and reflDir = reflect (unitaire(p)) (unitaire(normale))
    and bias = produit_vecteur(10.*.epsilon)(normale) (* biais qui permet de s'éloigner ou de rentrer dans l'objet selon le besoin*)
    in let startPointRefl = if dehors then somme_vecteur_point(produit_vecteur(-1.)(bias))(intersection) else somme_vecteur_point(bias)(intersection)
    and startPointRefr = if dehors then somme_vecteur_point(bias)(intersection) else somme_vecteur_point(produit_vecteur(-1.)(bias))(intersection)
    in let reflColor = rayon_couleur scene lights iorI ({photon_origine= startPointRefl; photon_direction=reflDir}) (depth+1)
    and refrDir = refract iorI iorR p normale
    in if kr < 1. && Option.is_some(refrDir) then (* S'il n'y a pas réflexion totale / réfraction limite *)
        let refrColor = rayon_couleur scene lights iorR ({photon_origine=startPointRefr; photon_direction=Option.get(refrDir)}) (depth+1)
        in mixColor(kr)(reflColor)(1.-.kr)(refrColor)
    else (* S'il y a réfraction limite / réflexion totale, alors *)
        reflColor

(*
 * Détermine la couleur d'un rayon en tenant compte de tout: réflexion, réfraction si nécessaire, mais
 * aussi diffusion, spécularité et ombre (soft-shadow), ...
 *)
and rayon_couleur scene lights iorI primaire depth = 
    (* On parcourt notre rayon jusqu'à un objet ou la fin du monde *)
    let (trouve, distance, etapes, _, _) = rayon_parcourt(scene)(primaire)
    in if trouve then 
        (* On tombe sur un objet, on calcule alors, origine, normale, point d'intersection et objet touché, etc.. *)
        let photon_ori = avance_photon(primaire.photon_origine)(primaire.photon_direction)(distance)
        in let photon = vecteur(origine)(photon_ori)
        in let nearest = getNearestSObj(scene)(photon)
        in let normale = estimeNormale(sObjAsSDF(nearest))(photon)
        (* On oublie pas les propriétés de diffusion et de spécularité de nos objets *)
        and (DiffusionValeur(diffColor, diffI), SpeculaireValeur(specColor, specI, specExp)) = (Option.get(sObjGetProperty(nearest)(Diffusion)), Option.get(sObjGetProperty(nearest)(Speculaire)))
        in let calc_lum acc light =  
            (* Calcule les intéractions lumineuses avec l'objet : notamment les coefficients de spécularité et de diffusion *)
            let light_dir = unitaire(vecteur(light.pos_l)(photon_ori))
            and light_dist = norme(vecteur(photon_ori)(light.pos_l))
            in let light_dir_inv = produit_vecteur(-1.)(light_dir)
            and diff, spec = acc
            (* Calcul des ombres également *)
            in let trouve, distance, _, distance_pjpo, distance_pppo  = rayon_parcourt(scene)({photon_origine=light.pos_l; photon_direction=light_dir})
            in let shad = if trouve && distance_pjpo < norme(vecteur(light.pos_l)(photon_ori)) then ((distance_pppo*.shadow)/.distance_pjpo) else 1.
                in let d = max(0.0)(min(1.0)(((produit_scalaire(light_dir_inv)(normale)))*.shad)) (* coefficient de diffusion *)
                in (  
                        diff+.light.intensite_l*.d,
                        (* quant à la spécularité *)
                        spec +. light.intensite_l*.min(1.0)((max(0.0)(d*.produit_scalaire(produit_vecteur(1.)(unitaire(reflect(light_dir_inv)(normale))))(unitaire(produit_vecteur(1.)(photon)))**specExp)))
                )
        (* On calcule donc diffusion et spécularité pour notre objet et on fait le mélange couleur *)
        in let diffuse, specular = List.fold_left(calc_lum)((0.0,0.0))(lights)
        in let k = float_of_int(List.length(lights))
        in let mix = 
            {
                (* Modèle de Phong *)
                r=min(1.0)(max(0.0)(((0.2+.diffuse)*.diffColor.r+.specular*.specColor.r(*+.normale.vx*))/.k));
                g=min(1.0)(max(0.0)(((0.2+.diffuse)*.diffColor.g+.specular*.specColor.g(*+.normale.vy*))/.k));
                b=min(1.0)(max(0.0)(((0.2+.diffuse)*.diffColor.b+.specular*.specColor.b(*+.normale.vz*))/.k))
            }
        in if depth < max_depth then (* Si on peut faire encore des rayons récursifs *)
            match sObjGetProperty(nearest)(Transparence) with (* Si notre objet est transparent *)
            | Some(TransparenceValeur(k)) -> 
                (* On détermine son indice optique *)
                let iorR = match sObjGetProperty(nearest)(IndiceOptique) with
                    | Some(IndiceOptiqueValeur(i)) -> i
                    | _ -> 1.0
                (* Petit mix couleur (Phong) avec la couleur de réflexion/réfraction *)
                in mixColor(k)(reflectAndRefract(photon)(scene)(lights)(photon_ori)(depth)(iorI)(iorR)(normale))(1. -. k)(mix)
            | _ -> mix
        else 
            mix
    else (* Si pas d'objet trouvé, alors on a "touché le ciel" *)
        background

(* Avance notre photon de la distance parcourue. *)
and avance_photon ori dir distance = 
        {
            x = ori.x +. distance *. dir.vx;
            y = ori.y +. distance *. dir.vy;
            z = ori.z +. distance *. dir.vz
        }

(* Le fameux ray-marcher qui parcourt le monde à la recherche des objets à intersecter. *)
and rayon_parcourt scene primaire =
    let etapes = ref 0
    and trouve = ref false
    and sdf = cObjAsSDF(scene)
    and dir = primaire.photon_direction
    and ori = primaire.photon_origine
    and pas = ref (-.1.0)
    and distance = ref 0.0
    and distance_pppo = ref 10000.0 (* Distance du rayon avec le plus proche objet, utile pour les ombres *)
    and distance_pjpo = ref 10000.0 (* Distance parcourue par le rayon jusqu'au plus proche objet, utile pour les ombres *)
    (* tant qu'on n'a pas trouvé ni dépassé le monde ou le nombre d'étapes max *)
    in while ((not !trouve && !etapes < 200) && !distance <= 1000.0) do
        (* Tant que le pas est supérieur à epsilon, càd tant qu'on ne touche rien *)
        if ((!pas > epsilon || !pas < -.epsilon)) then
            (* on avance notre photon le long du rayon *)
            let photon_ori = avance_photon(ori)(dir)(!distance)
            in let photon = vecteur(origine)(photon_ori)
            in begin 
                pas := sdf(photon); (* calcul le plus grand pas possible sans qu'on dépasse un objet *)
                distance := !distance +. !pas;
                etapes := !etapes + 1;
                (* si on croise un objet très proche de nous *)
                if !pas < !distance_pppo then begin
                    distance_pppo := !pas;
                    distance_pjpo := !distance
                end
            end
        else
            trouve := true
    done;
    (!trouve, !distance, !etapes, !distance_pjpo, !distance_pppo);;

(* Fait le rendu d'un bloc de pixels *)
let render_block camera scene lights x0 y0 xf yf=
    for x = x0 to xf do 
        for y = y0 to yf do 
            (* Pour chaque pixel, on construit le rayon primaire, on calcule la couleur du pixel et on l'affiche. *)
            let prim = rayon_primaire(camera)(x)(y)
            in let (r,g,b) = toRGB255(rayon_couleur(scene)(lights)(1.0)(prim)(1))
            in begin
                set_color(rgb(r)(g)(b));
                plot(x)(y)
            end
        done;
    done;
    (* En réalité l'affichage n'arrive qu'en fin de bloc. *)
    synchronize();;

(* Fait le rendu de la fenêtre *)
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
        in while (!blocks < largeur*hauteur) do (* ici une fonction complexe pour faire la petite spirale de rendu :) *)
            lim := !radius;
            while (!lim > 0) do begin
                    render_block camera scene lights (!x) (!y) (!x+block_size) (!y+block_size);
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

(* Mise en place de la caméra, de la scène à rendre, des lumières, etc ... *)
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

(* NOTE: TOUT OBJET DOIT POSSEDER UNE PROPRIETE DE DIFFUSION ET DE SPECULAIRE !!! *)
let sceneObj =
    let glassSphere = 
        let sph = sdfIntoSObject( sphereSDF ma_sphere )
        in sObjWithProperty sph Transparence (TransparenceValeur(0.5));
           sObjWithProperty sph IndiceOptique (IndiceOptiqueValeur(1.5));
           sObjWithProperty sph Diffusion (DiffusionValeur({r=0.3;g=1.;b=0.1}, 0.1));
           sObjWithProperty sph Speculaire (SpeculaireValeur({r=0.1;g=1.;b=0.1}, 0.9, 1425.));
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
        in sObjWithProperty plan Diffusion (DiffusionValeur({r=0.3;g=0.3;b=0.9}, 0.9));
           sObjWithProperty plan Speculaire (SpeculaireValeur({r=0.3;g=0.3;b=0.3}, 0.5, 125.));
           plan
    in sObjListToCObj([
        planZ;
        redSphere;
        greenSphere;
        littleMirror;
        glassSphere
    ]);;           

let lightObj = [
        {pos_l={x= 1.;y= 1.;z= 2.}; intensite_l=1.};
        {pos_l={x= 1.;y= 2.;z= -2.}; intensite_l=0.9};
        {pos_l={x= -2.;y= -2.;z= 4.}; intensite_l=1.0};
        {pos_l={x= 0.;y= -6.;z= 4.}; intensite_l=1.0};
    ];;

(** On lance le rendu etc **)
auto_synchronize false;;
render camera sceneObj lightObj 40;;
synchronize();;

(* Ferme la fenêtre une fois le rendu fini après un appui de touche *)
read_key();;