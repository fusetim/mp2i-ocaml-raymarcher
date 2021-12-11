#use "math.ml"
#use "sdf.ml"
#use "color.ml"

type propertyName =
    | IndiceOptique
    | Transparence
    | Couleur;;

type property =
    | IndiceOptiqueValeur of float
    | TransparenceValeur of float
    | CouleurValeur of color;;
    
type simpleObject = {sdf: (propertyName, property) Hashtbl.t -> vecteur -> float; properties: unit -> (propertyName, property) Hashtbl.t};;
type complexObject = {inner: simpleObject list};;

let sdfIntoSObject sdf_ =
    let prop = Hashtbl.create(0)
    in let getprop = fun () -> prop
    in let new_sdf prop = sdf_
    in {sdf=new_sdf;properties=getprop};;

let sObjWithProperty sobj name value =
    Hashtbl.add(sobj.properties())(name)(value);;

let sObjReplaceProperty sobj name value =
    Hashtbl.replace(sobj.properties())(name)(value);;

let sObjRemoveProperty sobj name =
    Hashtbl.remove(sobj.properties())(name);;

let sObjGetProperty sobj name =
    Hashtbl.find_opt(sobj.properties())(name);;

let sObjUnion sobj1 sobj2 =
    {inner=sobj1::sobj2::[]};;

let sObjListToCObj sobjs =
    {inner=sobjs};;

let cObjWithProperty cobj name value =
    List.iter(fun obj -> sObjWithProperty(obj)(name)(value))(cobj.inner);;

let cObjReplaceProperty cobj name value =
    List.iter(fun obj -> sObjReplaceProperty(obj)(name)(value))(cobj.inner);;

let cObjRemoveProperty cobj name =
    List.iter(fun obj -> sObjRemoveProperty(obj)(name))(cobj.inner);;

let cObjAsSDF cobj =
    foldOp opUnion (List.map(fun obj -> obj.sdf(obj.properties()))(cobj.inner));;

let getNearestSObj cobj p = 
    let calc_sdf obj = obj.sdf(obj.properties())(p)
    in let compare obj1 obj2 = Float.compare(calc_sdf(obj1))(calc_sdf(obj2)) 
    in let tries = List.fast_sort(compare)(cobj.inner)
    in List.hd(tries);;