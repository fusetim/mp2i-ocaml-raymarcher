(* GESTION DES COULEURS *)

(* 
 * Un type assez primitif pour représenter des couleurs dans l'espace RGB linéarisé.
 * Chacun de ses composants prendre un float entre 0 et 1.
 *)
type color = {r: float; g:float; b:float};;

(*
 * Multiplie les coefficients des différents channels entre eux.
 *)
let mixChannel color1 color2 =
    {r=((color1.r)*.color2.r);
    g=((color1.g)*.color2.g);
    b=((color1.b)*.color2.b)};;

(*
 * Multiplie par un facteur toutes les composantes d'une couleur.
 *)
let multColor facteur color = 
    {r=((color.r)*.facteur);
    g=((color.g)*.facteur);
    b=((color.b)*.facteur)};;

(*
 * Prend deux couleurs avec chacun un facteur. 
 * Ce facteur peut être en 0 et 1 néanmoins la somme des deux facteurs devrait
 * rester inférieure à 1. pour éviter les dépassements de capacité (overflow).
 *)
let mixColor f1 color1 f2 color2 =
    {r = (color1.r*.f1 +. color2.r*.f2);
     g = (color1.g*.f1 +. color2.g*.f2);
     b = (color1.b*.f1 +. color2.b*.f2)};;

(*
 * Similaire à mixColor mais prend seulement deux couleurs qui seront
 * mixé à 50% chacune.
 *)
let addColor color1 color2 =
    mixColor(0.5)(color1)(0.5)(color2)


(*
 * Convertit une couleur en un 3-uplet RGB d'entiers 8 bits.
 *)
let toRGB255 color = (
        int_of_float(sqrt((color.r))*.255.),
        int_of_float(sqrt((color.g))*.255.),
        int_of_float(sqrt((color.b))*.255.)
    );;