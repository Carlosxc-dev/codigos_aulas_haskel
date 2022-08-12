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
        else if op == 2 then -- remover filme por titulo
            do
                putStrLn "digite o titulo: "
                titulo <- getLine
                return (removerTitulo db titulo)
            else if op == 3 then  -- remover filme por diretor
                do 
                    putStrLn "digite o diretor: "
                    diretor <- getLine
                    return (removerDiretor db diretor)
                else if op == 4 then  -- Buscar Filme por Título
                    do 
                        putStrLn "digite o titulo: "
                        titulo <- getLine
                        putStrLn "\n--------FILME--------"
                        imprime(buscaTitulo db titulo)
                        return db
                    else if op == 5 then  -- Buscar Filme por Diretor
                        do 
                            putStrLn "digite o diretor: "
                            diretor <- getLine
                            putStrLn "\n--------FILME--------"
                            imprime(buscaDiretor db diretor)
                            return db
                        else if op == 6 then  -- Buscar Filme por Ano
                            do 
                                putStrLn "digite o lancamento: "
                                lancamento <- readLn
                                putStrLn "\n--------FILME--------"
                                imprime(buscaAno db lancamento)
                                return db
                            else if op == 7 then  -- Exibir Todos os Filmes
                                do 
                                    putStrLn "\n--------FILMES--------"
                                    imprime db
                                    return db
                                else --sair
                                    do
                                        putStrLn "\nsaindo..."
                                        return db

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
imprime:: Database -> IO ()
imprime [] = do putStrLn ""
imprime ((a,b,c,d):t) = do 
        putStrLn ("\ntitulo: " ++ a )
        putStrLn ("Diretor: " ++ b )
        putStrLn ("nota: " ++ show(c) )
        putStrLn ("lancamento: " ++ show(d) )
        imprime t 

inserir :: Database -> Filme -> Database
inserir [] filme = [filme] 
inserir db filme = ordena db filme

ordena :: Database -> Filme -> Database
ordena [] filme = [filme] 
ordena ((a,b,c,d):t) (w,x,y,z) = 
    if z < d then (w,x,y,z) : ordena t (a,b,c,d)
    else (a,b,c,d) : ordena t (w,x,y,z)  

removerTitulo :: Database -> Titulo -> Database
removerTitulo [] _ = []
removerTitulo ((h,a,b,c):t) titulo = if h == titulo 
    then removerTitulo t titulo
    else  (h,a,b,c) : removerTitulo t titulo

removerDiretor :: Database -> Diretor -> Database
removerDiretor [] _ = []
removerDiretor ((a,h,b,c):t) diretor = if h == diretor 
    then removerDiretor t diretor
    else  (a,h,b,c) : removerDiretor t diretor

buscaTitulo:: Database -> Titulo -> Database
buscaTitulo [] _ = []
buscaTitulo ((h,a,b,c):t) titulo = if h == titulo 
    then (h,a,b,c) : buscaTitulo t titulo 
    else buscaTitulo t titulo  

buscaDiretor:: Database -> Diretor -> Database
buscaDiretor [] _ = []
buscaDiretor ((a,h,b,c):t) diretor = if h == diretor 
    then (a,h,b,c) : buscaTitulo t diretor 
    else buscaDiretor t diretor  

buscaAno:: Database -> Lancamento -> Database
buscaAno [] _ = []
buscaAno ((a,b,c,h):t) ano = if h == ano 
    then (a,b,c,h) : buscaAno t ano 
    else buscaAno t ano  
