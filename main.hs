import System.Environment
import System.IO
import Control.Monad
import Control.Monad.Trans.State
import Data.Char
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List

data Grammar = Grammar [NonTerminal] [Terminal] NonTerminal ProductionRules deriving (Show)
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

rulesToList :: ProductionRules -> [(NonTerminal, [Either NonTerminal Terminal])]
rulesToList rules = foldl (\acc (nt, set) -> Set.foldl (\acc rhs -> (nt, rhs):acc) acc set) [] $ Map.toList rules

listToRules :: [(NonTerminal, [Either NonTerminal Terminal])] -> ProductionRules
listToRules rulesList = foldl (\acc (nt, rhs) -> case Map.lookup nt acc of
                                                        Nothing -> Map.insert nt (Set.singleton rhs) acc
                                                        Just set -> Map.insert nt (Set.insert rhs set) acc) Map.empty rulesList

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
eliminateLongRhsRules g =
    fst $ runState (eliminateLongRhsRules' g) 0

eliminateLongRhsRules' :: Grammar -> State Int Grammar
eliminateLongRhsRules' (Grammar nonTerminals terminals start rules) = do
    let rulesList = Map.toList rules
        folder (rules, replacements) (nt, destinationSet) = do
            (newRuleSet, newReplacements) <- replaceLongRhsRules destinationSet replacements
            let newRules = Map.insert nt newRuleSet rules
            return (newRules, newReplacements)
    (newRules, replacements) <- foldM folder (Map.empty, []) rulesList
    let newRules' = foldl (\rules ((nt1, nt2), newNt) -> Map.insert newNt (Set.singleton [Left nt1, Left nt2]) rules) newRules replacements
        addedNonTerminals = map snd replacements
        newNonTerminals = List.union nonTerminals addedNonTerminals
    return (Grammar newNonTerminals terminals start newRules')

replaceLongRhsRules :: Set.Set [Either NonTerminal Terminal] -> [((NonTerminal, NonTerminal), NonTerminal)]
    -> State Int (Set.Set [Either NonTerminal Terminal], [((NonTerminal, NonTerminal), NonTerminal)])
replaceLongRhsRules ruleSet replacements = do
    let folder (rules, replacements) rhs = do
            (shortenedRhs, newReplacements) <- shortenRhs rhs replacements
            let newRules = Set.insert shortenedRhs rules
            return (newRules, newReplacements)
        rulesList = Set.toList ruleSet
    foldM folder (Set.empty, replacements) rulesList

shortenRhs :: [Either NonTerminal Terminal] -> [((NonTerminal, NonTerminal), NonTerminal)]
    -> State Int ([Either NonTerminal Terminal], [((NonTerminal, NonTerminal), NonTerminal)])
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
        newRules = Map.insert newStartSymbol (Set.singleton [Left start]) rules
    in Grammar newNonTerminals terminals newStartSymbol newRules

eliminateNonSolitaryTerminals :: Grammar -> Grammar
eliminateNonSolitaryTerminals g@(Grammar nonTerminals terminals start rules) =
    let terminalReplacementPairs = zip terminals $ map (("N"++).show) [0..]
        terminalReplacements = Map.fromList terminalReplacementPairs
        newRules = Map.map (Set.map $ replaceTerminals terminalReplacements) rules
        rulesToAdd = Map.fromList $ map (\(t, nt) -> (nt, Set.singleton [Right t])) terminalReplacementPairs
        newRules' = Map.union newRules rulesToAdd
        nonTerminalsToAdd = map snd terminalReplacementPairs
        newNonTerminals = List.union nonTerminals nonTerminalsToAdd
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
        nonTerminals = Map.keys rules
        terminals = extractTerminals rules
    in Grammar nonTerminals terminals (fst.head $ parsedRules) rules

extractTerminals :: ProductionRules -> [Terminal]
extractTerminals rules =
    let rightSides = Map.elems rules
        rightSidesSet = foldl1 Set.union rightSides
        joinedRightSide = Set.foldl (++) [] rightSidesSet
        terminals = foldl filterTerminals [] joinedRightSide
    in nub terminals

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