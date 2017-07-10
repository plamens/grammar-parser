import System.Environment
import System.IO
import Control.Monad
import Data.Char
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List

data Grammar = Grammar (Set.Set NonTerminal) (Set.Set Terminal) NonTerminal ProductionRules deriving (Show)
type ProductionRules = Map.Map NonTerminal (Set.Set [Either NonTerminal Terminal])
type NonTerminal = String
type Terminal = String

main = do
    (grammarFileName:_) <- getArgs
    definitionStr <- readFile grammarFileName
    let grammar = parseGrammar definitionStr
    putStrLn $ toString grammar
    let nfGrammar = chomskyfy grammar
    putStrLn $ toString nfGrammar

chomskyfy :: Grammar -> Grammar
chomskyfy = eliminateEpsilonRules . eliminateLongRhsRules . eliminateNonSolitaryTerminals . eliminateStartSymbol

eliminateEpsilonRules :: Grammar -> Grammar
eliminateEpsilonRules (Grammar nonTerminals terminals start rules) =
    let eliminatedEpsilonRules = eliminateFirstEpsilonRule rules : map (eliminateFirstEpsilonRule.fst) eliminatedEpsilonRules
        noEpsilonRules = dropWhile ((== True).snd) eliminatedEpsilonRules
    in Grammar nonTerminals terminals start ((fst.head) noEpsilonRules)

eliminateFirstEpsilonRule :: ProductionRules -> (ProductionRules, Bool)
eliminateFirstEpsilonRule rules =
    let epsilonRules = map fst $ filter ((/= False).snd) $ map (\(nt, ruleSet) -> (nt, isEpsilon ruleSet)) $ Map.toList rules
        isEpsilon ruleSet = Set.member [] ruleSet || null ruleSet
        inlineEpsilonNt nt rules =
            let ruleRemoved = removeEpsilonNt nt rules
                ruleSet = Map.lookup nt ruleRemoved
            in case ruleSet of
                Just set -> Map.map (inlineEpsilonNtInSet nt) rules
                Nothing -> Map.map (removeNtInSet nt) rules
        inlineAndRemoveEpsilonNt nt rules = removeEpsilonNt nt $ inlineEpsilonNt nt rules
    in case epsilonRules of
        [] -> (rules, False)
        (epsilonNt:_) -> (inlineAndRemoveEpsilonNt epsilonNt rules, True)

inlineEpsilonNt :: NonTerminal -> ProductionRules -> ProductionRules
inlineEpsilonNt nt rules = Map.map (inlineEpsilonNtInSet nt) rules

inlineEpsilonNtInSet :: NonTerminal -> Set.Set [Either NonTerminal Terminal] -> Set.Set [Either NonTerminal Terminal]
inlineEpsilonNtInSet nt ruleSet =
    let inlinedEpsilonSet = Set.map (\x -> filter (/= Left nt) x) ruleSet
    in Set.union ruleSet inlinedEpsilonSet

removeEpsilonNt :: NonTerminal -> ProductionRules -> ProductionRules
removeEpsilonNt nt rules =
    let Just ruleSet = Map.lookup nt rules
        removedEpsilonRhs = Set.delete [] ruleSet
    in if Set.null ruleSet
        then Map.delete nt rules
        else Map.insert nt removedEpsilonRhs rules

removeNtInSet :: NonTerminal -> Set.Set [Either NonTerminal Terminal] -> Set.Set [Either NonTerminal Terminal]
removeNtInSet nt ruleSet = Set.map (\x -> filter (/= Left nt) x) ruleSet

eliminateLongRhsRules :: Grammar -> Grammar
eliminateLongRhsRules (Grammar nonTerminals terminals start rules) =
    let rulesList = Map.toList rules
        folder (rules, replacements, replacementIndex) (nt, destinationSet) =
            let (newRuleSet, newReplacements, newReplacementIndex) = replaceLongRhsRules destinationSet replacements replacementIndex
                newRules = Map.insert nt newRuleSet rules
            in (newRules, newReplacements, newReplacementIndex)
        (newRules, replacements, _) = foldl folder (Map.empty, [], 0) rulesList
        newRules' = foldl (\rules ((nt1, nt2), newNt) -> Map.insert newNt (Set.singleton [Left nt1, Left nt2]) rules) newRules replacements
        addedNonTerminals = map snd replacements
        newNonTerminals = Set.union nonTerminals $ Set.fromList addedNonTerminals
    in Grammar newNonTerminals terminals start newRules'

replaceLongRhsRules :: Set.Set [Either NonTerminal Terminal] -> [((NonTerminal, NonTerminal), NonTerminal)] -> Int
    -> (Set.Set [Either NonTerminal Terminal], [((NonTerminal, NonTerminal), NonTerminal)], Int)
replaceLongRhsRules ruleSet replacements replacementIndex =
    let folder (rules, replacements, replacementIndex) rhs =
            let (shortenedRhs, newReplacements, newReplacementIndex) = shortenRhs rhs replacements replacementIndex
                newRules = Set.insert shortenedRhs rules
            in (newRules, newReplacements, newReplacementIndex)
    in Set.foldl folder (Set.empty, replacements, replacementIndex) ruleSet

shortenRhs :: [Either NonTerminal Terminal] -> [((NonTerminal, NonTerminal), NonTerminal)] -> Int
    -> ([Either NonTerminal Terminal], [((NonTerminal, NonTerminal), NonTerminal)], Int)
shortenRhs rhs replacements replacementRuleIndex =
    if length rhs <= 2
        then (rhs, replacements, replacementRuleIndex)
        else
            let (Left nt1:Left nt2:remeaningRhs) = rhs
                newNonTerminal = "A" ++ show replacementRuleIndex
                newReplacementIndex = replacementRuleIndex + 1
                newReplacements = ((nt1, nt2), newNonTerminal):replacements
                newRhs = Left newNonTerminal:remeaningRhs
            in shortenRhs newRhs newReplacements newReplacementIndex

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
        stripEpsilon x = case x of
            [Right "e"] -> []
            x -> x
        destinationSet = Set.fromList $ map (stripEpsilon.(map terminalOrNonTerminal)) destinations'
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

toString :: Grammar -> String
toString (Grammar _ _ start rules) =
    let Just startRule = Map.lookup start rules
        remeaningRules = Map.delete start rules
        terminalOrNonTerminalToString x = case x of
            Left nt -> nt
            Right t -> t
        ruleToString nt destinations =
            let destinationStrings = Set.toList $ Set.map destinationToString destinations
                destinationToString dest = if null dest
                    then "e"
                    else foldl1 (++) $ map terminalOrNonTerminalToString dest
            in nt ++ " -> " ++ (List.intercalate " | " destinationStrings)
        remeaningRulePairs = Map.toList remeaningRules
        firstRow = ruleToString start startRule
        otherRows = map (\(nt, destinations) -> ruleToString nt destinations) remeaningRulePairs
    in Data.List.intercalate "\n" $ firstRow:otherRows