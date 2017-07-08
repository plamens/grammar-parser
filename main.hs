import System.Environment
import System.IO
import Control.Monad
import Data.Char
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map

data Grammar = Grammar (Set.Set NonTerminal) (Set.Set Terminal) ProductionRules deriving (Show)
type ProductionRules = Map.Map NonTerminal (Set.Set [Either NonTerminal Terminal])
type NonTerminal = Char
type Terminal = Char

main = do
    (grammarFileName:_) <- getArgs
    definitionStr <- readFile grammarFileName
    let grammar = parseGrammar definitionStr
    putStrLn $ show grammar
    return ()

parseGrammar :: String -> Grammar
parseGrammar definitionStr =
    let ruleLines = lines definitionStr
        rules = Map.fromList $ map parseLine ruleLines
        nonTerminals = Set.fromList (Map.keys rules)
        terminals = extractTerminals rules
    in Grammar nonTerminals terminals rules

extractTerminals :: ProductionRules -> Set.Set Terminal
extractTerminals rules =
    let rightSides = Map.elems rules
        rightSidesSet = foldl1 Set.union rightSides
        joinedRightSide = Set.foldl (++) [] rightSidesSet
        terminals = foldl filterTerminals [] joinedRightSide
    in Set.fromList terminals

filterTerminals :: [a] -> Either b a -> [a]
filterTerminals acc x = case x of
    Left nt -> acc
    Right t -> t:acc

parseLine :: String -> (NonTerminal, (Set.Set [Either NonTerminal Terminal]))
parseLine line =
    let ([nt]:_:rightSide) = words line
        destinations = filter (/="|") rightSide
        parseDestination x = if isUpper x then Left x else Right x
        destinationSet = Set.fromList $ map (map parseDestination) destinations
    in (nt, destinationSet)