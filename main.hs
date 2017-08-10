import System.Environment
import System.IO
import Control.Monad
import Control.Monad.Trans.State
import Data.Char
import Data.List
import Data.Function
import qualified Data.Map as Map

data Grammar = Grammar [NonTerminal] [Terminal] NonTerminal ProductionRules deriving (Show)
type ProductionRules = Map.Map NonTerminal [RightSide]
type NonTerminal = String
type Terminal = String
type RightSide = [Either NonTerminal Terminal]

main = do
    (grammarFileName:_) <- getArgs
    definitionStr <- readFile grammarFileName
    let grammar = parseGrammar definitionStr
    putStrLn $ toString grammar
    let nfGrammar = chomskyfy grammar
    putStrLn $ toString nfGrammar

chomskyfy :: Grammar -> Grammar
chomskyfy = eliminateUnitRules . eliminateEpsilonRules . eliminateLongRhsRules . eliminateNonSolitaryTerminals . eliminateStartSymbol

eliminateUnitRules :: Grammar -> Grammar
eliminateUnitRules (Grammar nonTerminals terminals start rules) =
    let eliminatedUnitRules = eliminateFirstUnitRule rules : map (eliminateFirstUnitRule.fst) eliminatedUnitRules
        noUnitRules = dropWhile ((== True).snd) eliminatedUnitRules
    in Grammar nonTerminals terminals start ((fst.head) noUnitRules)

eliminateFirstUnitRule :: ProductionRules -> (ProductionRules, Bool)
eliminateFirstUnitRule rules =
    let rulesList = rulesToList rules
        unitRule = find (\(_, rhs) -> length rhs == 1 && isNonTerminal (head rhs)) rulesList
        inlineUnitNt lNt rNt rulesList =
            let ruleRemoved = filter (\x -> x /= (lNt, [Left rNt])) rulesList
                rulesToAdd = filter (\(nt, rhs) -> nt == rNt) ruleRemoved
                rulesToAdd' = map (\(_, rhs) -> (lNt, rhs)) rulesToAdd
            in rulesToAdd' ++ ruleRemoved
    in case unitRule of
        Nothing -> (rules, False)
        Just (lNt, [Left rNt]) -> (listToRules $ inlineUnitNt lNt rNt rulesList, True)

isNonTerminal:: Either NonTerminal Terminal -> Bool
isNonTerminal (Left _) = True
isNonTerminal (Right _) = False

rulesToList :: ProductionRules -> [(NonTerminal, RightSide)]
rulesToList rules = foldl (\acc (nt, list) -> foldl (\acc rhs -> (nt, rhs):acc) acc list) [] $ Map.toList rules

listToRules :: [(NonTerminal, RightSide)] -> ProductionRules
listToRules rulesList = foldl (\acc (nt, rhs) -> case Map.lookup nt acc of
                                                        Nothing -> Map.insert nt [rhs] acc
                                                        Just list -> Map.insert nt (rhs:list) acc) Map.empty rulesList

eliminateEpsilonRules :: Grammar -> Grammar
eliminateEpsilonRules (Grammar nonTerminals terminals start rules) =
    Grammar nonTerminals terminals start (eliminateEpsilonRules' rules)

eliminateEpsilonRules' :: ProductionRules -> ProductionRules
eliminateEpsilonRules' = fix eliminateFirstEpsilonRule

eliminateFirstEpsilonRule f rules =
    let epsilonRules = map fst $ filter ((/= False).snd) $ map (\(nt, rulesList) -> (nt, isEpsilon rulesList)) $ Map.toList rules
        inlineAndRemoveEpsilonNt nt rules' = removeEpsilonNt nt $ inlineEpsilonNt nt rules'
    in case epsilonRules of
        [] -> rules
        (epsilonNt:_) -> f $ inlineAndRemoveEpsilonNt epsilonNt rules

isEpsilon :: [RightSide] -> Bool
isEpsilon rightSides = elem [] rightSides || null rightSides

inlineEpsilonNt :: NonTerminal -> ProductionRules -> ProductionRules
inlineEpsilonNt nt rules =
    let ruleRemoved = removeEpsilonNt nt rules
        rightSides = Map.lookup nt ruleRemoved
    in case rightSides of
        Just _ -> Map.map (inlineEpsilonNt' nt) rules
        Nothing -> Map.map (removeNt nt) rules

inlineEpsilonNt' :: NonTerminal -> [RightSide] -> [RightSide]
inlineEpsilonNt' nt rules =
    let inlinedEpsilonRightSides = map (\x -> filter (/= Left nt) x) rules
    in union rules inlinedEpsilonRightSides

removeEpsilonNt :: NonTerminal -> ProductionRules -> ProductionRules
removeEpsilonNt nt rules =
    let Just rightSides = Map.lookup nt rules
        removedEpsilonRhs = delete [] rightSides
    in if null rightSides
        then Map.delete nt rules
        else Map.insert nt removedEpsilonRhs rules

removeNt :: NonTerminal -> [RightSide] -> [RightSide]
removeNt nt rules = map (\x -> filter (/= Left nt) x) rules

eliminateLongRhsRules :: Grammar -> Grammar
eliminateLongRhsRules g =
    fst $ runState (eliminateLongRhsRules' g) 0

eliminateLongRhsRules' :: Grammar -> State Int Grammar
eliminateLongRhsRules' (Grammar nonTerminals terminals start rules) = do
    let rulesList = Map.toList rules
        folder (rules, replacements) (nt, destinations) = do
            (newRightSides, newReplacements) <- replaceLongRhsRules destinations replacements
            let newRules = Map.insert nt newRightSides rules
            return (newRules, newReplacements)
    (newRules, replacements) <- foldM folder (Map.empty, []) rulesList
    let newRules' = foldl (\rules ((nt1, nt2), newNt) -> Map.insert newNt [[Left nt1, Left nt2]] rules) newRules replacements
        addedNonTerminals = map snd replacements
        newNonTerminals = union nonTerminals addedNonTerminals
    return (Grammar newNonTerminals terminals start newRules')

replaceLongRhsRules :: [RightSide] -> [((NonTerminal, NonTerminal), NonTerminal)]
    -> State Int ([RightSide], [((NonTerminal, NonTerminal), NonTerminal)])
replaceLongRhsRules rightSides replacements = do
    let folder (rules, replacements) rhs = do
            (shortenedRhs, newReplacements) <- shortenRhs rhs replacements
            let newRules = shortenedRhs:rules
            return (newRules, newReplacements)
    foldM folder ([], replacements) rightSides

shortenRhs :: RightSide -> [((NonTerminal, NonTerminal), NonTerminal)]
    -> State Int (RightSide, [((NonTerminal, NonTerminal), NonTerminal)])
shortenRhs rhs replacements =
    if length rhs <= 2
        then return (rhs, replacements)
        else do
            let (Left nt1:Left nt2:remeaningRhs) = rhs
            replacementRuleIndex <- get
            let newNonTerminal = "A" ++ show replacementRuleIndex
            put (replacementRuleIndex + 1)
            let newReplacements = ((nt1, nt2), newNonTerminal):replacements
                newRhs = Left newNonTerminal:remeaningRhs
            shortenRhs newRhs newReplacements

eliminateStartSymbol :: Grammar -> Grammar
eliminateStartSymbol (Grammar nonTerminals terminals start rules) =
    let newStartSymbol = start ++ "0"
        newNonTerminals = newStartSymbol:nonTerminals
        newRules = Map.insert newStartSymbol [[Left start]] rules
    in Grammar newNonTerminals terminals newStartSymbol newRules

eliminateNonSolitaryTerminals :: Grammar -> Grammar
eliminateNonSolitaryTerminals g@(Grammar nonTerminals terminals start rules) =
    let terminalReplacementPairs = zip terminals $ map (("N"++).show) [0..]
        terminalReplacements = Map.fromList terminalReplacementPairs
        newRules = Map.map (map $ replaceTerminals terminalReplacements) rules
        rulesToAdd = Map.fromList $ map (\(t, nt) -> (nt, [[Right t]])) terminalReplacementPairs
        newRules' = Map.union newRules rulesToAdd
        nonTerminalsToAdd = map snd terminalReplacementPairs
        newNonTerminals = union nonTerminals nonTerminalsToAdd
    in Grammar newNonTerminals terminals start newRules'

replaceTerminals :: Map.Map NonTerminal Terminal -> RightSide -> RightSide
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
        nonTerminals = Map.keys rules
        terminals = extractTerminals rules
    in Grammar nonTerminals terminals (fst.head $ parsedRules) rules

extractTerminals :: ProductionRules -> [Terminal]
extractTerminals rules =
    let rightSides = Map.elems rules
        joinedRightSide = concat.concat $ rightSides
        terminals = foldl filterTerminals [] joinedRightSide
    in nub terminals

filterTerminals :: [a] -> Either b a -> [a]
filterTerminals acc x = case x of
    Left nt -> acc
    Right t -> t:acc

parseLine :: String -> (NonTerminal, [RightSide])
parseLine line =
    let (nt:_:rightSide) = words line
        destinations = filter (/="|") rightSide
        destinations' = map parseDestination destinations
        terminalOrNonTerminal s@(x:_) = if isUpper x then Left s else Right s
        stripEpsilon x = case x of
            [Right "e"] -> []
            x -> x
        destinationList = map (stripEpsilon.(map terminalOrNonTerminal)) destinations'
    in (nt, destinationList)

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
            let destinationStrings = map destinationToString destinations
                destinationToString dest = if null dest
                    then "e"
                    else foldl1 (++) $ map terminalOrNonTerminalToString dest
            in nt ++ " -> " ++ (intercalate " | " destinationStrings)
        remeaningRulePairs = Map.toList remeaningRules
        firstRow = ruleToString start startRule
        otherRows = map (\(nt, destinations) -> ruleToString nt destinations) remeaningRulePairs
    in intercalate "\n" $ firstRow:otherRows