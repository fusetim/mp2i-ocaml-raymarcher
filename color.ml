type color = {r: float; g:float; b:float};;

let multColor facteur color = 
    {r=((color.r)*.facteur);
    g=((color.g)*.facteur);
    b=((color.b)*.facteur)};;

let mixColor f1 color1 f2 color2 =
    {r = (color1.r*.f1 +. color2.r*.f2);
     g = (color1.g*.f1 +. color2.g*.f2);
     b = (color1.b*.f1 +. color2.b*.f2)};;

let addColor color1 color2 =
    mixColor(0.5)(color1)(0.5)(color2)

let toInvertedGamma color = (
        int_of_float(sqrt((color.r))*.255.),
        int_of_float(sqrt((color.g))*.255.),
        int_of_float(sqrt((color.b))*.255.)
    );;