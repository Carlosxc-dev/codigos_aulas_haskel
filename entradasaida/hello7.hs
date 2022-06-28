main = do
  putStrLn "Entre com o valor da corrente: "
  corrente <- readLn
  putStrLn "Entre com o valor da resistencia: "
  resist <- readLn
  print (tensao corrente resist) -- converte Float para String

tensao :: Float -> Float -> Float
tensao r i = r*i
