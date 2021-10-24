-- | This is the file you need to implement to complete the assignment. Remember
-- to comment where appropriate, use generic types and have fun!

module Player where

import Data.List
import Data.Char

import           Parser.Parser      -- This is the source for the parser from the course notes
import Parser.Instances

import           Cards              -- Finally, the generic card type(s)

import           TwentyOne.Types    -- Here you will find types used in the game of TwentyOne
import           TwentyOne.Rules    -- Rules of the game
import TwentyOne.Play

-- You can add more imports if you need them
-- import Debug.Trace
-- import TwentyOne.Types



{---------------------------------
Declarations
---------------------------------}

data Memory = Memory {
    currBid :: Int,
    deckState :: [CardFreq],
    lastActions :: [Action],
    lastUpcard :: Maybe Card,
    hiddenCard :: Int
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
            show_ lastUpcard,
            show_ hiddenCard]

instance Show CardFreq where
    show cf = show (rank cf) ++ ":" ++ show (freq cf)

-- instance Functor CardFreq where
--     fmap f (CardFreq rank freq) = CardFreq rank $ f freq

data Tree a = Node a [Tree a] 
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

instance Eq PlayerPoints where
    (PlayerPoints _ p1) == (PlayerPoints _ p2) = p1 == p2

instance Ord PlayerPoints where
    (PlayerPoints _ p1) `compare` (PlayerPoints _ p2) = p1 `compare` p2

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
playCard upcard points info pid memo hand = (action, show newMemo)
    where
        -- update the old memory with the latest information
        oldMemo = updateMemoryInfo upcard pid info hand $ deserialise memo
        -- choose an action based on the newest information stored in the memory
        action = case upcard of
            Nothing -> makeBid pid points oldMemo
            Just c  -> decideAction c hand pid points oldMemo
        -- update the memory with the action chosen
        newMemo = updateMemoryAction action oldMemo



{---------------------------------
Bidding
---------------------------------}

makeBid :: PlayerId -> [PlayerPoints] -> Memory -> Action
makeBid pid points memo
    | psafe > 4/10 = adjustBid $ min maxBid $ max blw minBid -- maybe just change this to maxBid or blw
    | psafe > 3/10 = adjustBid $ max minBid $ min abv ((maxBid + minBid) `div` 2)
    | otherwise = adjustBid minBid
    where
        adjustBid = Bid . validBid pid points
        ptree = makeTree 2 Combo memo []
        psafe = 1 - jointProbEq Bust ptree - jointProbLt (Value 18) ptree + 
            jointProbLt (Value 12) ptree - jointProbLt (Value 8) ptree
        (blw, abv) = determineBidBounds pid points

validBid :: PlayerId -> [PlayerPoints] -> Points -> Points
validBid pid points bid
    | pts < minBid = pts
    | pts > maxBid = min maxBid bid
    | bid > pts = pts
    | otherwise = bid
    where pts = getPoint pid points

determineBidBounds :: PlayerId -> [PlayerPoints] -> (Points, Points)
determineBidBounds pid points = (point - blw, abv - point)
    where
        sortedPoints = sort points
        point = getPoint pid points
        abv = _playerPoints $ foldr1 (\v a -> if _playerPoints v > point && v < a then v else a) sortedPoints
        blw = _playerPoints $ foldl1 (\a v -> if _playerPoints v < point && v > a then v else a) sortedPoints



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
        dtree = makeTree 1 Combo memo [upcard]
        dcombo = jointProbEq Combo dtree
        altAction = splitOrDouble upcard hand pid points memo

splitOrDouble :: Card -> [Card] -> PlayerId -> [PlayerPoints] -> Memory -> Action
splitOrDouble upcard hand pid points memo
    | length hand /= 2 = hitOrStand upcard hand memo

    -- Split
    | canSplit hand && (headRank == Ace || headRank == Eight) = Split bid
    | canSplit hand && headRank <= Seven = if headRank >= upRank && upRank /= Ace then Split bid else Hit

    -- Double
    | handVal == Value 11 = DoubleDown bid
    | hasAce hand && handVal <= Value 17 = if upRank >= Four && upRank <= Six then DoubleDown bid else Hit
    | psafe > 1/3 && handVal > Value 11 = DoubleDown bid

    | otherwise = hitOrStand upcard hand memo
    where
        bid = validBid pid points $ currBid memo
        ptree = makeTree 1 Combo memo hand
        psafe = 1 - jointProbEq Bust ptree - jointProbLt (Value 18) ptree
        headRank = getRank $ head hand
        handVal = handValue hand
        upRank = getRank upcard

hitOrStand :: Card -> [Card] -> Memory -> Action
hitOrStand upcard hand memo
    | handValue hand <= Value 11 = Hit
    | pbust <= 1/2 = Hit
    -- (|) handValue hand <= Value 14 && pbust <= 2/3 = Hit -- should i add this in
    | dbust > 2/3 && dbust - pbust >= 1/5 = Hit
    | otherwise = Stand
    where
        ptree = makeTree 1 Combo memo hand
        pbust = jointProbEq Bust ptree
        dtree = makeTree 4 (Value 17) memo [upcard]
        dbust = jointProbEq Bust dtree



{---------------------------------
Memory Maintainance
---------------------------------}

deserialise :: Maybe String -> Memory
deserialise memo = case parse parseMemory <$> memo of
    Just (Result _ m) -> m
    _ -> initMemory

updateMemoryInfo :: Maybe Card -> PlayerId -> [PlayerInfo] -> [Card] -> Memory -> Memory
updateMemoryInfo upcard pid info hand oldMemo =
    oldMemo {
        deckState  = updateDeckState newCards (deckState oldMemo),
        lastUpcard = upcard,
        hiddenCard = hiddenCard oldMemo + dealerUnplayedCard info upcard
    }
    where newCards = getNewCards pid upcard info hand oldMemo

-- maps each card onto the deckstate to update it through folding
updateDeckState :: [Card] -> [CardFreq] -> [CardFreq]
updateDeckState newCards deckState = handleRefills (foldr (map . updateFreq) deckState newCards)
    where updateFreq card cardFreq
            | getRank card == rank cardFreq = CardFreq (getRank card) (freq cardFreq - 1)
            | otherwise = cardFreq

-- if all cards reach 0 or any cards reach negative, the deck is replenished
handleRefills :: [CardFreq] -> [CardFreq]
handleRefills deckState = if all ((0 ==) . freq) deckState || any ((0 >) . freq) deckState then
    map (\ v -> CardFreq (rank v) (freq v + numRanks)) deckState else deckState

dealerUnplayedCard :: [PlayerInfo] -> Maybe Card -> Int
dealerUnplayedCard info upcard = case upcard of
    Nothing -> if null dealerList then 0 else 1
    _       -> 0
    where dealerList = filter (\d -> _playerInfoId d == dealerId && length (playerInfoHand d) == 1) info

getNewCards :: PlayerId -> Maybe Card -> [PlayerInfo] -> Hand -> Memory -> [Card]
getNewCards pid upcard info hand memo = let
    lastAction = head (lastActions memo)
    in case upcard of
        -- first turn (always remove dealer's previous upcard)
        Nothing -> removeDealerUpcard (lastUpcard memo) . concat $ (playerInfoHand <$>) $ 
            if lastAction == Hit then
                -- take only player head if they hit (drew a card) last round
                includePlayerHead pid info else
                -- else dont include the player at all
                filter ((pid /=) . _playerInfoId) info

        Just c -> case lastAction of
            -- second turn
            Bid _ -> [c] ++ concat (playerInfoHand <$> filter ((dealerId /=) . _playerInfoId) info) ++ hand
            -- any subsequent turns
            Hit -> [head hand]
            _ -> []

removeDealerUpcard :: Maybe Card -> [Card] -> [Card]
removeDealerUpcard upcard cards = case upcard of
    (Just c) -> delete c cards
    Nothing  -> cards

includePlayerHead :: PlayerId -> [PlayerInfo] -> [PlayerInfo]
includePlayerHead pid info = hands ++ [PlayerInfo pid [playerLatestCard]]
    where 
        (hands, phand)   = filterSplit ((pid /=) . _playerInfoId) info
        playerLatestCard = head (playerInfoHand (head phand))

updateMemoryAction :: Action -> Memory -> Memory
updateMemoryAction action oldMemo = case action of
    Bid amt -> oldMemo {currBid = amt, lastActions = [action]}
    _       -> oldMemo {lastActions = action : lastActions oldMemo}



{---------------------------------
Tree and Deck Statistics
---------------------------------}

makeTree :: Int -> HandValue -> Memory -> Hand -> Tree Load
makeTree n maxVal memo hand = Node (L n (handValue hand) 1.0 hand) $
    makeNodes (n-1) maxVal 1.0 memo <$> newHands
    where newHands = findPossibleHands (deckState memo) hand

makeNodes :: Int -> HandValue -> Double -> Memory -> Hand -> Tree Load
makeNodes 0 _ prob' memo hand = Node (L 0 newTotal newProb hand) []
    where
        newTotal = handValue hand
        newProb = jointProb prob' hand (deckState memo) (hiddenCard memo)
makeNodes n maxVal prob' memo hand = Node (L n newTotal newProb hand) $
    if terminal 
        then []
        else makeNodes (n-1) maxVal newProb (memo {deckState = newDeckState}) <$> newHands
    where
        newTotal = handValue hand
        terminal = newTotal == Bust || newTotal == Combo || newTotal == Charlie || newTotal >= maxVal

        oldDeckState = deckState memo
        newDeckState = if null hand then oldDeckState else updateDeckState [head hand] oldDeckState

        newProb = jointProb prob' hand oldDeckState (hiddenCard memo) -- newProb is with new head
        newHands = findPossibleHands newDeckState hand

jointProb :: Double -> [Card] -> [CardFreq] -> Int -> Double
jointProb _ [] _ _ = 1.0 -- base probability starts from 1
jointProb prob' hand deckState hiddenCard = fromIntegral rankFreq / fromIntegral (totalCards deckState + hiddenCard) * prob'
    where
        newCardRank = getRank (head hand)
        rankFreq = getRankFreq newCardRank deckState

findPossibleHands :: [CardFreq] -> [Card] -> [[Card]]
findPossibleHands deckState hand = (:hand) <$> (Card Spade <$> availableRanks)
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
    _ <- commaTok
    hidden <- parseInt
    pure $ Memory bid deck actions card hidden

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
Utility & Auxiliary Functions
---------------------------------}

initMemory :: Memory
initMemory = Memory 0 (CardFreq <$> [Ace ..] <*> [numRanks]) [Stand] Nothing 0

numRanks :: Int
numRanks = numDecks * 4

filterSplit :: (a -> Bool) -> [a] -> ([a], [a])
filterSplit f alist = (filter f alist, filter (not . f) alist)

getPoint :: PlayerId -> [PlayerPoints] -> Points
getPoint pid points = _playerPoints $ head $ filter ((pid ==) . _playerPointsId) points

canSplit :: [Card] -> Bool
canSplit hand = length hand == 2 && getRank (head hand) == getRank (last hand)

hasAce :: [Card] -> Bool
hasAce = any ((== Ace) . getRank)

getRankFreq :: Rank -> [CardFreq] -> Int
getRankFreq rank' = foldr (\v a -> if rank v == rank' then a + freq v else a) 0

totalCards :: [CardFreq] -> Int
totalCards = foldr (\v a -> a + freq v) 0

-- traceIf :: Bool -> String -> p -> p
-- traceIf True  s x = trace s x
-- traceIf False _ x = x



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


