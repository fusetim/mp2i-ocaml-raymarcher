#use "math.ml"
#use "sdf.ml"
#use "color.ml"

(* * Module Object * *)

(* Type primitif représentant une propriété d'un objet, comme s'il possède un indice optique. *)
type propertyName =
    | IndiceOptique
    | Transparence
    | Diffusion
    | Speculaire;;

(* Type primitif représentant la valeur associée à une propriété d'un objet, comme son indice optique. *)
type property =
    | IndiceOptiqueValeur of float (* Indice optique du milieu *)
    | TransparenceValeur of float
    | DiffusionValeur of color * float (* Couleur et Intensité diffuse de l'objet *)
    | SpeculaireValeur of color * float * float (* Couleur, Intensité et Exposant spéculaire de l'objet *);;
    
(* 
 * Primitive d'un objet "simple", il ne représente qu'un objet au sens qu'il forme un système continue et cohérent.
 *
 * Il contient un SDF (signed distance function) qui pourra utiliser à sa guide les propriétés internes à l'objet.
 * Ces propriétés internes sont stockés dans une HashTable (module OCaml). Elles pourront être récuppérées à travers des
 * accesseurs, transformateurs mais aussi lors du calcul du SDF.
 *)
type simpleObject = {sdf: (propertyName, property) Hashtbl.t -> vecteur -> float; properties: unit -> (propertyName, property) Hashtbl.t};;

(*
 * Un objet "complexe" est composé de sous-objet simple. Il s'agit d'une sorte de groupement d'objet.
 * Il peut servir à représenter une scène entière.
 *)
type complexObject = {inner: simpleObject list};;

(*
 * Primitive d'objet lumière. 
 * Basiquement une source ponctuelle de lumière tri-dimensionelle.
 *
 * Est décrit par un centre et une intensité lumineuse (entre 0 et 1 généralement).
 *)
type lightObject = {pos_l: point; intensite_l: float};;

(*
 * Fonction utilitaire pour transformer un SDF (Signed Distance Function) en objet simple.
 * Il lui ajoute basiquement une liste vide de propriétés. 
 *)
let sdfIntoSObject sdf_ =
    let prop = Hashtbl.create(0)
    in let getprop = fun () -> prop
    in let new_sdf prop = sdf_
    in {sdf=new_sdf;properties=getprop};;

(*
 * Ajoute une propriété particulière à un objet simple.
 * Nécessite un objet simple [simpleObject], une propriété [propertyName] et une valeur [property].
 *
 * Pour remplacer une propriété, utiliser [sObjReplaceProperty].
 *)
let sObjWithProperty sobj name value =
    Hashtbl.add(sobj.properties())(name)(value);;

(*
 * Remplace une propriété particulière à un objet simple.
 * Nécessite un objet simple [simpleObject], une propriété [propertyName] et une valeur [property].
 *
 * Pour ajouter une propriété, utiliser [sObjWithProperty].
 *)
let sObjReplaceProperty sobj name value =
    Hashtbl.replace(sobj.properties())(name)(value);;

(*
 * Enlève une propriété particulière à un objet simple.
 * Nécessite un objet simple [simpleObject] et une propriété [propertyName].
 *)
let sObjRemoveProperty sobj name =
    Hashtbl.remove(sobj.properties())(name);;

(*
 * Récupére une propriété particulière à un objet simple.
 * Nécessite un objet simple [simpleObject] et une propriété [propertyName].
 *
 * La valeur renvoyée est une `property option`.
 *)
let sObjGetProperty sobj name =
    Hashtbl.find_opt(sobj.properties())(name);;

(*
 * Union de deux objets simples en un objet complexe.
 *)
let sObjUnion sobj1 sobj2 =
    {inner=sobj1::sobj2::[]};;

(*
 * Union d'une liste d'objets simples en un objet complexe.
 *)
let sObjListToCObj sobjs =
    {inner=sobjs};;

(*
 * Détermine la fonction SDF (Signed Distance Function) de l'objet simple.
 *
 * Permet au SDF d'accéder aux propriétés de l'objet simple.
 *)
let sObjAsSDF obj =
    obj.sdf(obj.properties());;

(*
 * Ajoute une propriété particulière à un objet complexe.
 * Nécessite un objet complexe [complexObject], une propriété [propertyName] et une valeur [property].
 *
 * Pour remplacer une propriété, utiliser [cObjReplaceProperty].
 *
 * Cette fonction en réalité ajoute la propriété à tous les objets enfants.
 *)
let cObjWithProperty cobj name value =
    List.iter(fun obj -> sObjWithProperty(obj)(name)(value))(cobj.inner);;

(*
 * Remplace une propriété particulière à un objet complexe.
 * Nécessite un objet complexe [complexObject], une propriété [propertyName] et une valeur [property].
 *
 * Pour ajouter une propriété, utiliser [cObjWithProperty].
 *
 * Cette fonction en réalité remplace la propriété à tous les objets enfants. 
 * ATTENTION: la propriété doit déjà existé pour tous les objets enfants.
 *)
let cObjReplaceProperty cobj name value =
    List.iter(fun obj -> sObjReplaceProperty(obj)(name)(value))(cobj.inner);;

(*
 * Enlève une propriété particulière à un objet complexe.
 * Nécessite un objet complexe [complexObject] et une propriété [propertyName].
 *
 * Cette fonction en réalité enlève la propriété à tous les objets enfants. 
 * ATTENTION: la propriété doit déjà existé pour tous les objets enfants.
 *)
let cObjRemoveProperty cobj name =
    List.iter(fun obj -> sObjRemoveProperty(obj)(name))(cobj.inner);;

(*
 * Détermine la fonction SDF (Signed Distance Function) de l'objet complexe.
 *
 * Permet au SDF d'accéder aux propriétés respectifs de l'objet simple associé.
 *)
let cObjAsSDF cobj =
    foldOp opUnion (List.map(fun obj -> obj.sdf(obj.properties()))(cobj.inner));;

(*
 * Détermine l'objet enfant de l'objet complexe le plus proche du rayon.
 *)
let getNearestSObj cobj p = 
    let calc_sdf obj = obj.sdf(obj.properties())(p)
    in let compare obj1 obj2 = Float.compare(calc_sdf(obj1))(calc_sdf(obj2)) 
    in let tries = List.fast_sort(compare)(cobj.inner)
    in List.hd(tries);;
