import System.Environment
import System.IO
import Control.Monad
import Data.Char
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map

data Grammar = Grammar (Set.Set NonTerminal) (Set.Set Terminal) NonTerminal ProductionRules deriving (Show)
type ProductionRules = Map.Map NonTerminal (Set.Set [Either NonTerminal Terminal])
type NonTerminal = String
type Terminal = String

main = do
    (grammarFileName:_) <- getArgs
    definitionStr <- readFile grammarFileName
    let grammar = parseGrammar definitionStr
    putStrLn $ show grammar
    let nfGrammar = chomskyfy grammar
    putStrLn $ show nfGrammar

chomskyfy :: Grammar -> Grammar
chomskyfy g@(Grammar nonTerminals terminals start rules) =
    let eliminatedStartSymbol = eliminateStartSymbol g
        eliminatedNonSolitaryTerminals = eliminateNonSolitaryTerminals eliminatedStartSymbol
    in eliminatedNonSolitaryTerminals

eliminateStartSymbol :: Grammar -> Grammar
eliminateStartSymbol (Grammar nonTerminals terminals start rules) =
    let newStartSymbol = start ++ "0"
        newNonTerminals = Set.insert newStartSymbol nonTerminals
        newRules = Map.insert newStartSymbol (Set.singleton [Left start]) rules
    in Grammar newNonTerminals terminals newStartSymbol newRules

eliminateNonSolitaryTerminals :: Grammar -> Grammar
eliminateNonSolitaryTerminals g@(Grammar nonTerminals terminals start rules) =
    let terminalReplacementPairs = zip (Set.toList terminals) $ map (("N"++).show) [0..]
        terminalReplacements = Map.fromList terminalReplacementPairs
        newRules = Map.map (Set.map $ replaceTerminals terminalReplacements) rules
        rulesToAdd = Map.fromList $ map (\(t, nt) -> (nt, Set.singleton [Right t])) terminalReplacementPairs
        newRules' = Map.union newRules rulesToAdd
        nonTerminalsToAdd = map snd terminalReplacementPairs
        newNonTerminals = Set.union nonTerminals $ Set.fromList nonTerminalsToAdd
    in Grammar newNonTerminals terminals start newRules'

replaceTerminals :: Map.Map NonTerminal Terminal -> [Either NonTerminal Terminal] -> [Either NonTerminal Terminal]
replaceTerminals replacements = map replace where
    replace x = case x of
        Left n -> Left n
        Right t -> let Just n = Map.lookup t replacements
            in Left n

parseGrammar :: String -> Grammar
parseGrammar definitionStr =
    let ruleLines = lines definitionStr
        parsedRules = map parseLine ruleLines
        rules = Map.fromList $ parsedRules
        nonTerminals = Set.fromList (Map.keys rules)
        terminals = extractTerminals rules
    in Grammar nonTerminals terminals (fst.head $ parsedRules) rules

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
    let (nt:_:rightSide) = words line
        destinations = filter (/="|") rightSide
        destinations' = map parseDestination destinations
        terminalOrNonTerminal s@(x:_) = if isUpper x then Left s else Right s
        destinationSet = Set.fromList $ map (map terminalOrNonTerminal) destinations'
    in (nt, destinationSet)

parseDestination :: String -> [String]
parseDestination "" = []
parseDestination (x:xs) = parseDestination' [x] xs

parseDestination' :: String -> String -> [String]
parseDestination' x [] = [x]
parseDestination' last (x:xs) =
    if isNumber x
        then parseDestination' (last ++ [x]) xs
        else last:parseDestination' [x] xs