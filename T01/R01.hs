perimetro :: Float -> Float -> Float

perimetro w h = w*2 + h*2

area :: Float -> Float -> Float
area b h = (b*h)/2

velocidadeMedia :: Float -> Float -> Float
velocidadeMedia s t = s / t

diferencaVelocidade :: Float -> Float -> Float -> Float
diferencaVelocidade s t1 t2 = (s/t1) - (s/t2)

converteDolar :: Float -> Float -> Float 
converteDolar x tx = x / tx

temperatura :: Float  -> Float
temperatura c = c * 1.8 + 32

par :: Int -> Bool
par x = mod x 2 == 0

multiplo :: Int -> Int -> Bool
multiplo i j = j * x == i

dupli :: [Int] -> Int 
dupli 
