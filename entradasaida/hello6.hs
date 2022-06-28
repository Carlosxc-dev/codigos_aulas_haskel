main = do
  menu  -- exibe menu, cujo retorno é IO()
  putStrLn "Digite a opção desejada:"
  opt <- readLn   -- le opção
  putStrLn (carryout opt) -- função que realiza a ação escolhida (opt)
  if opt == 4 then return () else main -- se opt for 4, sair. Senão, loop!

menu :: IO()
menu = do putStrLn "1 - Incluir cliente"
          putStrLn "2 - Excluir cliente"
          putStrLn "3 - Ver clientes"
          putStrLn "4 - Sair"

carryout :: Int -> String
carryout op | op == 1 = "Cliente incluido."
            | op == 2 = "Cliente excluido."
            | op == 3 = "Lista de clientes."
            | op == 4 = "Programa terminado."
            | otherwise = "opcao incorreta."
