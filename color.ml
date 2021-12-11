type color = {r: int; g:int; b:int};;

let multColor facteur color = 
    {r=int_of_float(float_of_int(color.r)*.facteur);
    g=int_of_float(float_of_int(color.g)*.facteur);
    b=int_of_float(float_of_int(color.b)*.facteur)};;

let addColor color1 color2 =
    {r = color1.r + color2.r;
     g = color1.g + color2.g;
     b = color1.b + color2.b};;

