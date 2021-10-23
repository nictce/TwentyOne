-- | This is the file you need to implement to complete the assignment. Remember
-- to comment where appropriate, use generic types and have fun!

module Player where

import           Parser.Parser      -- This is the source for the parser from the course notes
import           Cards              -- Finally, the generic card type(s)

import           TwentyOne.Types    -- Here you will find types used in the game of TwentyOne
import           TwentyOne.Rules    -- Rules of the game

-- You can add more imports if you need them
-- import Debug.Trace
import Data.List
import Parser.Instances
import Data.Char
import TwentyOne.Play (dealerId)



{---------------------------------
Declarations
---------------------------------}

data Memory = Memory {
    currBid :: Int,
    deckState :: [CardFreq],
    lastActions :: [Action],
    lastUpcard :: Maybe Card
}

data CardFreq = CardFreq {
    rank :: Rank,
    freq :: Int
}

instance Show Memory where
    show m = intercalate "," values where
        show_ f = show (f m)
        values = [
            show_ currBid,
            show_ deckState,
            show_ lastActions,
            show_ lastUpcard]

instance Show CardFreq where
    show cf = show (rank cf) ++ ":" ++ show (freq cf)

-- instance Functor CardFreq where
--     fmap f (CardFreq rank freq) = CardFreq rank $ f freq

data Tree a = Node a [Tree a] -- | Leaf a 
    deriving (Show)

data Load = L {
    len :: Int,
    total :: HandValue, -- total value of combination of ranks
    prob :: Double, -- probability of getting to that node from combination of ranks
    ranks :: Hand
}

instance Show Load where
    show l = intercalate "," values where
        show_ f = show (f l)
        values = [
            show_ len,
            show_ total,
            show_ prob,
            show_ ranks]

-- instance Foldable Tree where
--     foldMap f (Node a []) = f a
--     foldMap f (Node a trees) = [f a] ++ (concat (foldMap f <$> trees))

-- instance Functor Load where
--     fmap f l = f $ prob l



{---------------------------------
playCard
---------------------------------}

-- | This function is called once it's your turn, and keeps getting called until your turn ends.
playCard :: PlayFunc
playCard upcard points info pid memo hand
    -- | traceIf (pid == "0") ("\n======================================\n" ++ "id: " ++ show pid ++ " upcard: " ++ show upcard) False = undefined
    -- | traceIf (pid == "0") ("info: " ++ show info ++ " hand: " ++ show hand) False = undefined
    -- | traceIf (pid == "0") ("memo: " ++ show memo) False = undefined

    -- | otherwise
    = let
        newMemo = updateMemoryInfo upcard pid info hand $ deserialise memo
        action = case upcard of
            Nothing -> makeBid pid points newMemo
            Just c -> decideAction c hand pid points newMemo
        finalMemo = updateMemoryAction action newMemo
    in (action, show finalMemo)



{---------------------------------
Bidding
---------------------------------}

makeBid :: PlayerId -> [PlayerPoints] -> Memory -> Action
makeBid pid points memo
-- bid safe borders at 0.3, bid safe' borders at 0.4
    -- | trace ("bid safe " ++ show psafe ++ "\tbid safe' " ++ show psafe') False = Bid maxBid
    | psafe > 4/10 = adjustBid maxBid
    | psafe > 3/10 = adjustBid ((maxBid + minBid) `div` 2)
    | otherwise = adjustBid minBid
    where
        adjustBid = Bid . validBid pid points 
        ptree = makeTree 2 Combo (deckState memo) []
        pbust = jointProbEq Bust ptree -- this is always 0
        pLt18 = jointProbLt (Value 18) ptree
        pLt12 = jointProbLt (Value 12) ptree
        pLt8 = jointProbLt (Value 8) ptree
        psafe = 1 - pbust - pLt18 + pLt12 - pLt8

getPoint :: PlayerId -> [PlayerPoints] -> Points
getPoint pid points = _playerPoints $ head $ filter ((pid ==) . _playerPointsId) points

validBid :: PlayerId -> [PlayerPoints] -> Points -> Points
validBid pid points bid
    | pts < minBid = pts
    | pts > maxBid = min maxBid bid
    | bid > pts = pts
    | otherwise = bid
    where pts = getPoint pid points



{---------------------------------
Actions
---------------------------------}

decideAction :: Card -> [Card] -> PlayerId -> [PlayerPoints] -> Memory -> Action
decideAction upcard hand pid points memo = case take 2 $ lastActions memo of
    [Bid _] -> case upcard of
        Card _ Ace -> if dcombo > 2/3
            then Insurance (validBid pid points $ currBid memo `div` 2)
            else altAction
        _ -> altAction
    [DoubleDown _, _] -> Hit
    [Hit, DoubleDown _] -> Stand
    _ -> altAction
    where
        dtree = makeTree 1 Combo (deckState memo) [upcard]
        dcombo = jointProbEq Combo dtree
        altAction = doubleOrSplit upcard hand pid points memo

doubleOrSplit :: Card -> [Card] -> PlayerId -> [PlayerPoints] -> Memory -> Action
doubleOrSplit upcard hand pid points memo
    | length hand /= 2 = hitOrStand upcard hand memo

    -- Double
    | psafe > 1/3 || handValue hand == Value 11 = DoubleDown bid
    -- Split
    | getRank (head hand) == getRank (last hand) &&
      (getRank (head hand) == Ace || getRank (head hand) == Eight) = Split bid
    
    | otherwise = hitOrStand upcard hand memo
        where 
            bid = validBid pid points $ currBid memo
            ptree = makeTree 1 Combo (deckState memo) hand
            pbust = jointProbEq Bust ptree
            pLt18 = jointProbLt (Value 18) ptree
            psafe = 1 - pbust - pLt18

-- Hit Stand DoubleDown Split
hitOrStand :: Card -> [Card] -> Memory -> Action
hitOrStand upcard hand memo
    -- | trace ("psafe " ++ show psafe ++ "\tdsafe " ++ show dsafe) False = Stand
    | handValue hand <= Value 11 = Hit
    -- | handValue hand <= Value 14 && pbust <= 2/3 = Hit
    | pbust <= 1/2 = Hit
    | dbust > 2/3 && dbust - pbust >= 1/5 = Hit
    | otherwise = Stand
    where
        deckState_ = deckState memo
        ptree = makeTree 1 Combo deckState_ hand
        pbust = jointProbEq Bust ptree
        dtree = makeTree 4 (Value 17) deckState_ [upcard]
        dbust = jointProbEq Bust dtree



{---------------------------------
Memory Maintainance
---------------------------------}

deserialise :: Maybe String -> Memory
deserialise memo = case parse parseMemory <$> memo of
    Just (Result _ m) -> m
    Just (Error _) -> initMemory -- trace "Error occured in deserialising memory!" 
    Nothing -> initMemory

updateMemoryInfo :: Maybe Card -> PlayerId -> [PlayerInfo] -> [Card] -> Memory -> Memory
updateMemoryInfo upcard pid info hand oldMemo =
    oldMemo {
        deckState = updateDeckState newCards (deckState oldMemo),
        lastUpcard = upcard
    }
    where newCards = getNewCards pid upcard info hand oldMemo

updateMemoryAction :: Action -> Memory -> Memory
updateMemoryAction action oldMemo = case action of
    Bid amt -> oldMemo {currBid = amt, lastActions = [action]}
    _ -> oldMemo {lastActions = action : lastActions oldMemo}

updateDeckState :: [Card] -> [CardFreq] -> [CardFreq]
updateDeckState newCards memo = checkDeck (foldr (map . updateFreq) memo newCards)
    where updateFreq card cardFreq
            | getRank card == rank cardFreq = CardFreq (getRank card) (freq cardFreq - 1)
            | otherwise =  cardFreq

checkDeck :: [CardFreq] -> [CardFreq]
-- if all cards reach 0 or any cards reach negative, the deck is replenished
checkDeck deckState = if all ((0 ==) . freq) deckState || any ((0 >) . freq) deckState then
    map (\ v -> CardFreq (rank v) (freq v + numRanks)) deckState else deckState


getNewCards :: PlayerId -> Maybe Card -> [PlayerInfo] -> Hand -> Memory -> [Card]
getNewCards pid upcard info hand memo = let
    lastAction = head (lastActions memo)
    in case upcard of
        -- first turn (always remove dealer's previous upcard)
        Nothing -> removeDealerUpcard (lastUpcard memo) . concat $ (playerInfoHand <$>) $ if lastAction == Hit then
            -- take player head if they hit (took a card) last round
            includePlayerHead pid info else
            -- else dont include the player at all
            filter ((pid /=) . _playerInfoId) info

        Just c -> case lastAction of
            -- second turn    !!! TODO: removes dealers hand - bug?
            Bid _ -> [c] ++ concat (playerInfoHand <$> filter ((dealerId /=) . _playerInfoId) info) ++ hand
            -- >= second turn
            _ -> [head hand]

removeDealerUpcard :: Maybe Card -> [Card] -> [Card]
removeDealerUpcard upcard cards = case upcard of
    (Just c) -> delete c cards
    Nothing -> cards

includePlayerHead :: PlayerId -> [PlayerInfo] -> [PlayerInfo]
includePlayerHead pid info = let
    (hands, phand) = filter' ((pid /=) . _playerInfoId) info in
    hands ++ [PlayerInfo pid [head (playerInfoHand (head phand))]]



{---------------------------------
Tree and Deck Statistics
---------------------------------}

makeTree :: Int -> HandValue -> [CardFreq] -> Hand -> Tree Load
makeTree n maxVal deckState hand = Node (L n (handValue hand) 1.0 hand) $
    makeTree_ (n-1) maxVal 1.0 deckState <$> newHands
    where newHands = possHands deckState hand

makeTree_ :: Int -> HandValue -> Double -> [CardFreq] -> Hand -> Tree Load
makeTree_ 0 _ prob' deckState hand = Node (L 0 newTotal newProb hand) []
    where
        newTotal = handValue hand
        newProb = jointProb prob' hand deckState
makeTree_ n maxVal prob' deckState hand = Node (L n newTotal newProb hand) $
    if terminal then [] 
    else makeTree_ (n-1) maxVal newProb newDeckState <$> newHands
    where
        newTotal = handValue hand
        terminal = newTotal == Bust || newTotal == Combo || newTotal == Charlie || newTotal >= maxVal

        newProb = jointProb prob' hand deckState -- newProb is with new head
        newDeckState = if null hand then deckState else updateDeckState [head hand] deckState
        newHands = possHands newDeckState hand

jointProb :: Double -> [Card] -> [CardFreq] -> Double
jointProb _ [] _ = 1.0 -- base probability starts from 1
jointProb prob' hand deckState = fromIntegral numOfRank / fromIntegral (totalCards deckState) * prob'
    where
        rank' = getRank (head hand)
        numOfRank = rankEq rank' deckState

possHands :: [CardFreq] -> [Card] -> [[Card]]
possHands deckState hand = (:hand) <$> (Card Spade <$> availableRanks)
    where availableRanks = rank <$> filter ((>0) . freq) deckState

sumTree :: Tree Load -> Double
sumTree (Node a []) = prob a -- only take probabilities at the leaves
sumTree (Node _ trees) = sum (sumTree <$> trees)

-- finds probability less than val at every leaf (at any level) of the tree
jointProbLt :: HandValue -> Tree Load -> Double
jointProbLt val (Node a []) = if total a < val then prob a else 0
jointProbLt val (Node _ trees) = sum (jointProbLt val <$> trees)

-- finds probability equal to val at every leaf (at any level) of the tree
jointProbEq :: HandValue -> Tree Load -> Double
jointProbEq val (Node a []) = if total a == val then prob a else 0
jointProbEq val (Node _ trees) = sum (jointProbEq val <$> trees)

-- finds probability equal to val at every leaf (at the bottom level) of the tree
jointProbBottom :: HandValue -> Tree Load -> Double
jointProbBottom val (Node a []) = if total a == val && len a == 0 then prob a else 0
jointProbBottom val (Node _ trees) = sum (jointProbBottom val <$> trees)

rankEq :: Rank -> [CardFreq] -> Int
rankEq rank' = foldr (\v a -> if rank v == rank' then a + freq v else a) 0

totalCards :: [CardFreq] -> Int
totalCards = foldr (\v a -> a + freq v) 0



{---------------------------------
Parser Combinators
---------------------------------}

parseInt :: Parser Int
parseInt = P $ \s -> case readInt s of
    Just (v, r) -> Result r v
    Nothing -> Error $UnexpectedString s

-- parses a Showable into its type according to an input list of possible type values
parseShow :: (Foldable t, Functor t, Show a) => t a -> Parser a
parseShow alist = foldr1 (|||) (parseShow_ <$> alist)

parseShow_ :: Show b => b -> Parser b
parseShow_ a = stringTok (show a) >> pure a

parseList :: Parser a -> Parser [a]
parseList parser = do
    _ <- stringTok "["
    result <- sepby parser commaTok
    _ <- stringTok "]"
    pure result

parseMemory :: Parser Memory
parseMemory = do
    bid <- parseInt
    _ <- commaTok
    deck <- parseList parseCardFreq
    _ <- commaTok
    actions <- parseList parseAction_
    _ <- commaTok
    card <- parseMaybeCard
    pure $ Memory bid deck actions card

parseCardFreq :: Parser CardFreq
parseCardFreq = do
    r <- parseRank
    _ <- stringTok ":"
    f <- parseInt
    pure $ CardFreq r f

parseAction_ :: Parser Action
parseAction_ =  (stringTok "Hit" >> pure Hit) |||
    (stringTok "Stand" >> pure Stand) |||
    (stringTok "DoubleDown " >> parseInt >>= pure . DoubleDown) |||
    (stringTok "Split " >> parseInt >>= pure . Split) |||
    (stringTok "Bid " >> parseInt >>= pure . Bid) |||
    (stringTok "Insurance " >> parseInt >>= pure . Insurance)

parseCard :: Parser Card
parseCard = do
    suit <- parseShow [Spade ..]
    Card suit <$> parseRank

parseRank :: Parser Rank
parseRank = parseShow [Ace ..]

parseMaybeCard :: Parser (Maybe Card)
parseMaybeCard = (stringTok "Just " >> pure <$> parseCard) ||| (stringTok "Nothing" >> pure Nothing)


{---------------------------------
Parser Utility
---------------------------------}

list :: Parser a -> Parser [a]
list p = list1 p ||| pure []

list1 :: Parser a -> Parser [a]
list1 p = p >>= (\p' -> list p >>= (\p''-> pure (p':p'')))

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
  c <- character
  (if f c then pure else unexpectedCharParser) c

spaces :: Parser String
spaces = list (satisfy isSpace)

string :: String -> Parser String
string = traverse is

tok :: Parser a -> Parser a
tok p = do
    r <- p
    _ <- spaces
    pure r

commaTok :: Parser Char
commaTok = tok (is ',')

stringTok :: String -> Parser String
stringTok = tok . string

sepby :: Parser a -> Parser s -> Parser [a]
sepby p1 p2 = sepby1 p1 p2 ||| pure []

sepby1 :: Parser a -> Parser s -> Parser [a]
sepby1 a s = do
    x <- a
    xs <- list $ s >> a
    pure (x:xs)



{---------------------------------
Utility
---------------------------------}

initMemory :: Memory
initMemory = Memory 0 (CardFreq <$> [Ace ..] <*> [numRanks]) [Stand] Nothing

-- traceIf :: Bool -> String -> p -> p
-- traceIf True  s x = trace s x
-- traceIf False _ x = x

filter' :: (a -> Bool) -> [a] -> ([a], [a])
filter' f alist = (filter f alist, filter (not . f) alist)

numRanks :: Int
numRanks = numDecks * 4






{---------------------------------
BNF CFG
---------------------------------}

-- <memory> ::= <currBid> ";" <deckState> ";" <lastActions>
-- <currBid> ::= <int>
-- <int> ::= <digit> | <digit><int>
-- <digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

-- <deckState> ::= "[" <cardFreqs> "]"
-- <cardFreqs> ::= <cardFreq> | <cardFreq> "," <cardFreqs>
-- <cardFreq> ::= <rank> ":" <int> 

-- <lastActions> ::= "[" <actions> "]"
-- <actions> ::= <action> | <action> "," <actions>
-- <action> ::= "H" | "ST" | <doubleDown> | <split> | <bid> | <insurance>
-- <doubleDown> ::= "DD" <int>
-- <split> ::= "SP" <int>
-- <bid> ::= "B" <int>
-- <insurance> ::= "I" <int>


