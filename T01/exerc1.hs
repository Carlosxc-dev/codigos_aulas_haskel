areac :: Float -> Float
areac r = pi * r * r

perimetroc :: Float -> Float
perimetroc r = 2 * pi * r

hipotenusa :: Float -> Float -> Float
hipotenusa a b = sqrt (a^2 + b^2)

diffarea :: Float -> Float -> Float
diffarea r1 r2 = abs ((areac r1) - (areac r2))
