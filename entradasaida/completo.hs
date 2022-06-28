type Cliente = (String, Int)
type Database = [Cliente]

inserir :: Database -> Cliente -> Database
inserir [] cl = [cl]
inserir db cl = db ++ [cl]

remover :: Database -> Cliente -> Database
remover [] _ = []
remover (h:t) cl = if h == cl then remover t cl else h : remover t cl

--- Função Principal
main = do
  let db = [] -- Inicializa DB com lista vazia
  menu db -- exibe menu, cujo retorno é IO()

menu :: Database -> IO()
menu db = do
    putStrLn "\n1 - Incluir cliente"
    putStrLn "2 - Excluir cliente"
    putStrLn "3 - Ver clientes"
    putStrLn "4 - Sair"
    putStrLn "Digite a opção desejada:"
    opt <- readLn   -- le opção
    db <- carryout db opt -- função que realiza a ação escolhida (opt)
    if opt == 4 then return () else menu db -- se opt for 4, sair. Senão, loop!

dados :: IO Cliente
dados = do
  putStrLn "Nome do cliente: "
  nome <- getLine
  putStrLn "CPF do cliente: "
  cpf <- readLn
  return (nome, cpf)  -- é uma função de IO que retorna um Cliente.

-- Função para impressão da lista de Clientes
imprime :: Database -> IO ()
imprime [] = do putStrLn ""
imprime (h:t) = do putStrLn (fst h ++ " - " ++ show (snd h))
                   imprime t

carryout :: Database -> Int -> IO Database
carryout db op = do
  if op == 1 then
    do cl <- dados
       return (inserir db cl)
    else if op == 2 then
      do cl <- dados
         return (remover db cl)
      else if op == 3 then
        do putStrLn "Lista de Clientes: "
           imprime db
           return db
        else do
          putStrLn "Terminado."
          return db
