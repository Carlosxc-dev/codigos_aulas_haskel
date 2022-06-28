main = do
  putStrLn "Em que ano voce nasceu?"
  ano <- getLine
  let idade = calculaidade 2022 (read ano :: Int)
  putStrLn ("Voce tem " ++ show idade ++ " anos!")

calculaidade :: Int -> Int -> Int
calculaidade anoatual anonasc = anoatual - anonasc
