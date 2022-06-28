main = do
  putStrLn "Entre com o valor da corrente: "
  corrente <- getLine
  putStrLn "Entre com o valor da resistencia: "
  resist <- getLine
  let resfloat = read resist :: Float
      corfloat = read corrente :: Float
  putStrLn ("Tensao = " ++ show (tensao corfloat resfloat))

tensao :: Float -> Float -> Float
tensao r i = r*i
