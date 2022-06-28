main = do
  putStrLn "Entre com o valor da corrente: "
  corrente <- readLn
  putStrLn "Entre com o valor da resistencia: "
  resist <- readLn
  putStrLn ("Tensao = " ++ show (tensao corrente resist))

tensao :: Float -> Float -> Float
tensao r i = r*i
