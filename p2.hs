--declaracao de tipos
type Titulo = String
type Diretor = String
type Nota = Float
type Lancamento = Int

type Filme = (Titulo, Diretor, Nota, Lancamento)
type Database = [Filme]

-- menu
main = do
    let db = []
    menu db

menu :: Database -> IO()
menu db = do
    putStrLn "\n1. Inserir Novo Filme"
    putStrLn "2. Remover Filme por Título"
    putStrLn "3. Remover Filmes por Diretor"
    putStrLn "4. Buscar Filme por Título"
    putStrLn "5. Buscar Filme por Diretor"
    putStrLn "6. Buscar Filme por Ano"
    putStrLn "7. Exibir Todos os Filmes"
    putStrLn "8. Sair do Programa"
    opt <- readLn
    db <- switch db opt
    if opt == 8 then return () else menu db

-- operacoes do menu
switch :: Database -> Int -> IO Database
switch db op = do
    if op == 1 then --insere novo filme 
        do
            filme <- dados
            return (inserir db filme)
        -- else if op == 2 then -- remover filme por titulo
        --     do
        --     else if op == 3 then  -- remover filme por diretor
        --         do 
        --         else if op == 4 then  -- Buscar Filme por Título
        --             do 
        --             else if op == 5 then  -- Buscar Filme por Diretor
        --                 do 
        --                 else if op == 6 then  -- Buscar Filme por Ano
        --                     do 
        --                     else if op == 7 then  -- Exibir Todos os Filmes
        --                         do 
        --                         else --sair
        --                             do
        --                                 putStrLn "\nsaindo..."
        --                                 return db

--coleta de dados do filme 
dados :: IO Filme
dados = do
            putStrLn "titulo do filme: "
            titulo <- getLine
            putStrLn "diretor do filme: "
            diretor <- getLine
            putStrLn "nota do filme: "
            nota <- readLn
            putStrLn "lancamento do filme: "
            lancamento <- readLn
            return (titulo, diretor, nota, lancamento)


-- funcoes dos menus
inserir :: Database -> Filme -> Database
inserir [] filme = [filme]
inserir db filme = db ++ [filme]

-- remover :: Database -> Filme -> Database
-- remover [] _ = []
-- remover (h,_,_,_:t) filme = if h == filme then remover t cl else h : remover t cl




