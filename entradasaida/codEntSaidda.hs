type Cliente = (String , Int)
type Database = [Cliente]

inserir :: Database -> Cliente -> Database
inserir [] cl = [cl]
inserir db cl = db ++ [cl]

remover :: Database -> Cliente -> Database
remover [] _ = []
remover (h:t) cl = if h == cl then remover t cl else h : remover t cl

dados :: IO Cliente
dados = do 
    putStrLn "nome do cliente :"
    nome <- getLine
    putStrLn "cpf do cliente "
    cpf <- readLn
    return (nome, cpf)

imprime:: Database -> IO ()
imprime [] = do putStrLn ""
imprime (h:t) = do 
        putStrLn (fst h ++ " - " ++ show (snd h) )
        imprime t


main = do
    let db = []
    menu db

menu :: Database -> IO()
menu db = do
    putStrLn "\n1 - incluir clientes"
    putStrLn "2 - excluir clientes"
    putStrLn "3 - ver clientes"
    putStrLn "4 - sair"
    putStrLn "digite a opcao desejada ? "
    opt <- readLn
    db <- carryout db opt
    if opt == 4 then return () else menu db

carryout :: Database -> Int -> IO Database
carryout db op = do
    if op == 1 then --adicionar 
        do
            cl <- dados
            return (inserir db cl)
        else if op == 2 then -- excluir
            do 
                cl <- dados
                return (remover db cl)
            else if op == 3 then  -- ver 
                do 
                    putStrLn "\n\nlista de clientes"
                    imprime db
                    return db
                else --sair
                    do
                        putStrLn "\nterminado"
                        return db

