-- INSTRUÇÕES SOBRE INPUT NO FIM DO CÓDIGO

-- tipos algébricos
type State = Char
type AtomicProgram = Char
type EdgeSet = [(State,State,AtomicProgram)]
type Frame = ([State],EdgeSet,[HistoryEntry])
type HistoryEntry = (Program,[State])
data Program = Composed Char Program Program | Atomic Char deriving Show

-- funções auxiliares de entrada do EdgeSet
getFirst :: (State, State, AtomicProgram) -> State
getFirst (x,_,_) = x

getSecond :: (State, State, AtomicProgram) -> State
getSecond (_,x,_) = x

getThird :: (State, State, AtomicProgram) -> AtomicProgram
getThird (_,_,x) = x

-- funções auxiliares de Frame
getStates :: Frame -> [State]
getStates (x,_,_) = x

getEdgeSet :: Frame -> EdgeSet
getEdgeSet (_,x,_) = x

getHistory :: Frame -> [HistoryEntry]
getHistory (_,_,x) = x

-- funções auxiliares de HistoryEntry
getProgram :: HistoryEntry -> Program
getProgram (x,_) = x

getStatesOfHistory :: HistoryEntry -> [State]
getStatesOfHistory (_,x) = x

-- funções auxiliares de lista
listUnion :: (Eq a) => [a] -> [a] -> [a]
listUnion xs = foldr(\y acc -> if y `notElem` xs then y:acc else acc) xs

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

-- busca da primeira entrada da lista de histórico com estado não-vazio (reverter o histórico antes)
getFirstNonNullState :: [HistoryEntry] -> HistoryEntry
getFirstNonNullState (x:xs)
    | length(getStatesOfHistory(x)) == 0 = getFirstNonNullState xs
    | otherwise = x

-- funções auxiliares do parsing do programa de entrada
getFirstArgumentUnbalanced :: [Char] -> String -> String
getFirstArgumentUnbalanced [] [] = []
getFirstArgumentUnbalanced [] x = []
getFirstArgumentUnbalanced x [] = []
getFirstArgumentUnbalanced (y:ys) (x:xs) 
    | x == ')' = x:(getFirstArgumentUnbalanced ys xs)
    | x == '(' = '(':getFirstArgumentUnbalanced (')':y:ys) xs
    | otherwise = x:getFirstArgumentUnbalanced (y:ys) xs

removeFirstChar :: String -> String
removeFirstChar (x:xs) = xs

getSecondArgumentUnbalanced :: [Char] -> String -> String
getSecondArgumentUnbalanced [] [] = []
getSecondArgumentUnbalanced [] x = []
getSecondArgumentUnbalanced x [] = []
getSecondArgumentUnbalanced (y:ys) (x:xs) 
    | x == '(' = '(':(getSecondArgumentUnbalanced ys xs)
    | x == ')' = ')':getSecondArgumentUnbalanced ('(':y:ys) xs
    | otherwise = x:(getSecondArgumentUnbalanced (y:ys) xs) 

getSecondArgument :: [Char] -> String -> String
getSecondArgument x y = removeFirstChar(reverseList (getSecondArgumentUnbalanced x (removeFirstChar (reverseList y))))

removeLastChar :: String -> String
removeLastChar [] = []
removeLastChar (x:xs) 
    | length(xs) == 1 = [x]
    | otherwise = x : removeLastChar xs 

getFirstArgument :: [Char] -> String -> String
getFirstArgument x y = removeLastChar (getFirstArgumentUnbalanced x y)

parseProgramString :: String -> Program
parseProgramString [x] = Atomic x
parseProgramString (x:y:xs)
    | x == 'U' || x == ';' || x == '*'= Composed x (parseProgramString (getFirstArgument ")" xs)) (parseProgramString (getSecondArgument "(" (x:y:xs)))
    | otherwise = Atomic y

removeElems :: (Integral i) => i -> [a] -> [a] -- remove the first n elements from a list
removeElems _ [] = []
removeElems 0 x = x
removeElems 1 x = tail x
removeElems n xs = removeElems (n-1) (tail xs)

split' :: Char -> String -> [String] -- takes a string and split it by a given character
split' c x
    | notElem c x = [x]
    | otherwise = takeWhile (/= c) x : split' c (removeElems (length (takeWhile (/=c) x) + 1) x)

parseEdgeSetString :: String -> EdgeSet
parseEdgeSetString x = foldl(\acc (w:y:[z]) -> acc ++ [(w,y,z)]) [] splittedInput
    where splittedInput = split' ',' x

-- funções de execução de programa
execAtomicProgram :: AtomicProgram -> Frame -> Frame
execAtomicProgram p frame = (reachedStates,getEdgeSet(frame),getHistory(frame) ++ [(Atomic p,getStates(frame))])
    where
        reachedStates = foldr (\x acc -> if getFirst(x) `elem` getStates(frame) && getThird(x) == p 
             && getSecond(x) `notElem` acc
            then getSecond(x):acc else acc) [] (getEdgeSet(frame))

execCup :: Program -> Program -> Frame -> Frame
execCup x y frame = (listUnion (getStates(executor x frame)) (getStates(executor y frame)),getEdgeSet(frame), 
    getHistory(frame) ++ [((Composed 'U' x y),getStates(frame))])

execSequential :: Program -> Program -> Frame -> Frame
execSequential x y frame = (reachedStates,getEdgeSet(frame),
    getHistory(frame) ++ [(x, getStates(frame)),(y,getStates(executor x frame))])
    where
        reachedStates = getStates(executor y (executor x frame))

execIterative :: Program -> Frame -> Frame
execIterative x frame
    | null (getStates(executor x frame)) = (getStates(frame),getEdgeSet(frame),getHistory(frame)++[((Composed '*' x x),getStates(frame))])
    | otherwise = (listUnion (getStates(frame)) (getStates(executor x frame)),getEdgeSet(frame), getHistory(frame))

executor :: Program -> Frame -> Frame
executor (Composed x y z) frame
    | x == ';' = execSequential y z frame
    | x == 'U' = execCup y z frame
    | x == '*' = execIterative y frame
executor (Atomic x) frame = execAtomicProgram x frame

-- função principal
analyzer :: Program -> Frame -> String
analyzer p frame
    | length(finalStates) /= 0 = "The input program is valid in the input frame. The reachable final states are: {" ++ finalStates ++"}."
    | otherwise = "The input program is not valid in the input frame. The program {" ++ show failProgram ++ "} has no transitions from the states: {" ++ failStates ++ "}."
        where 
            finalFrame = executor p frame
            finalStates = getStates(finalFrame)
            failHistory = getFirstNonNullState (reverseList(getHistory(finalFrame)))
            failProgram = getProgram(failHistory)
            failStates = getStatesOfHistory(failHistory)

-- função que exibe o histórico de execução do programa
executionLog :: Program -> Frame -> [HistoryEntry]
executionLog p frame = getHistory(executor p frame)


main = do
    putStrLn "Type the program: "
    inputProgram <- getLine
    putStrLn "Type the states: "
    inputStates <- getLine
    putStrLn "Type the edges: "
    inputEdges <- getLine
    let edgeSet = parseEdgeSetString inputEdges
    let program = parseProgramString inputProgram
    putStrLn (analyzer program (inputStates,edgeSet,[]))

-- ENTRADA DE DADOS:


-- PROGRAMA: 

-- o programa deve ser informado em notação pré-fixada usando a seguinte gramática:

-- <programa> :: <operador>(<programa>)(<programa>) | (<char>)
-- <operador> :: U | ; | *
-- <char> :: qualquer caracter

-- note que o operador unário '*' receberá dois argumentos
-- o programa executará normalmente com qualquer valor no segundo argumento (apenas o primeiro é usado)
-- para melhorar a visualização do histórico de execução, sugerimos repetir o programa no segundo argumento 

-- por exemplo, o programa  ;(U(;(p)(q))(*(p)(p)))(q) equivale a
-- ((p;q)U(p*));q


-- ESTADOS:

-- cada estado é um caracter
-- os estados devem ser informados juntos em uma string

-- por exemplo, se os estados são a, b e c, então o input deverá ser abc


-- CONJUNTO DE ARESTAS:

-- cada aresta é uma string com 3 caracteres
-- inserir as arestas separadas por vírgulas.

-- por exemplo, se as transições de estados são abp (programa 'p' transita o estado de 'a' para 'b')
-- e bcq (programa 'q' transita o estado de 'b' para 'c'), então a entrada deve ser abp,bcq