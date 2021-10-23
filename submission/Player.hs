-- | This is the file you need to implement to complete the assignment. Remember
-- to comment where appropriate, use generic types and have fun!

module Player where

import           Parser.Parser      -- This is the source for the parser from the course notes
import           Cards              -- Finally, the generic card type(s)

import           TwentyOne.Types    -- Here you will find types used in the game of TwentyOne
import           TwentyOne.Rules    -- Rules of the game

-- You can add more imports if you need them
import Debug.Trace
import Data.List
import Parser.Instances
import Data.Char


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



-- | This function is called once it's your turn, and keeps getting called until your turn ends.
playCard :: PlayFunc
playCard upcard points info pid memo hand
    -- | traceIf (pid == "0") ("id: " ++ show pid ++ " upcard: " ++ show upcard) False = undefined
    -- | traceIf (pid == "0") ("info: " ++ show info ++ " hand: " ++ show hand) False = undefined
    -- | traceIf (pid == "0") ("memo: " ++ show memo ++ "\n======================================") False = undefined

    -- | otherwise
    = let
        newMemo = updateMemoryInfo upcard pid info hand $ deserialise memo
        action = case upcard of
            Nothing -> makeBid pid points newMemo
            Just c -> decideAction c hand newMemo
        finalMemo = updateMemoryAction action newMemo
    in (action, show finalMemo)


{---------------------------------
Bidding
---------------------------------}

makeBid :: PlayerId -> [PlayerPoints] -> Memory -> Action
makeBid pid points memo
    | combo > 1/3 = Bid maxBid
    | p > 1/3 = Bid maxBid -- Bid $ max (maxBid * 2 `div` 3) minBid -- $ (maxBid + minBid) `div` 2
    | otherwise = Bid $ min ((maxBid + minBid) `div` 2) $ getPoint pid points
    --  otherwise = Bid maxBid
    where
        p = probValueBelow 8 deckState_
        combo = probValue 1 deckState_ * probValue 10 deckState_
        deckState_ = deckState memo

getPoint :: PlayerId -> [PlayerPoints] -> Points
getPoint pid points = _playerPoints $ head $ filter ((pid ==) . _playerPointsId) points


{---------------------------------
Actions
---------------------------------}

decideAction :: Card -> [Card] -> Memory -> Action
decideAction upcard hand memo = case take 2 $ lastActions memo of
    [Bid _] -> decide2nd upcard hand memo
    [DoubleDown _, _] -> Hit
    [Hit, DoubleDown _] -> Stand
    _ -> playHand upcard hand memo

decide2nd :: Card -> [Card] -> Memory -> Action
decide2nd upcard hand memo
    -- Double
    | handCalc hand == 11 = DoubleDown bid
    -- Split
    | getRank (head hand) == getRank (head (tail hand)) &&
      (getRank (head hand) == Ace || getRank (head hand) == Eight) = Split bid
    -- Insurance -- must 1/2 or put bid??
    | getRank upcard == Ace = if probValue 10 (deckState memo) > 2.0/3.0
        then Insurance bid
        else playHand upcard hand memo

    | otherwise = playHand upcard hand memo
    where bid = max minBid $ currBid memo

-- Hit Stand DoubleDown Split
playHand :: Card -> [Card] -> Memory -> Action
playHand upcard hand memo
    | phand < 11 = Hit
    | trace ("" ++ show treeprob) treeprob > 1/3 = Hit
    | p > 1/2 && length hand == 2 = DoubleDown $ currBid memo
    | p > 1/3 = Hit
    | d < 1/3 && phand >= 19 && phand > dhand = Stand
    | diff > -1/3 = Hit
    | diff <= -1/3 = Stand
    | otherwise = Stand
    where
        p = probValueBelow (targetValue - phand) (deckState memo)
        d = probValueBelow (targetValue - dhand) (deckState memo)
        phand = handCalc hand
        dhand = handCalc [upcard]
        tree = makeTree (5 - length hand) Combo (deckState memo) hand
        diff = p - d
        treeprob = jointProbEq Charlie tree + jointProbEq (Value 21) tree

{---------------------------------
Deck Statistics
---------------------------------}

probValueBelow :: Int -> [CardFreq] -> Double
probValueBelow val deckState = fromIntegral (cardsBelow val deckState) / fromIntegral (totalCards deckState)

probValue :: Int -> [CardFreq] -> Double
probValue val deckState = fromIntegral (foldr (\v a -> if rankValue (rank v) == val then freq v else a) 0 deckState) / fromIntegral (totalCards deckState)

cardsBelow :: Int -> [CardFreq] -> Int
cardsBelow val = foldr (\v a -> if rankValue (rank v) <= val then a + freq v else a) 0

cardsEq :: Int -> [CardFreq] -> Int
cardsEq val = foldr (\v a -> if rankValue (rank v) == val then a + freq v else a) 0

totalCards :: [CardFreq] -> Int
totalCards = foldr (\v a -> a + freq v) 0

rankValue :: Rank -> Int
rankValue rank
    --  rank == Ace = 11
    | rank < Jack = fromEnum rank + 1
    | otherwise   = 10

-- calculates the probability of the dealer's hidden card (below 17?)
-- dlrHcProb :: [CardFreq] -> Int -> Int -> Float
-- dlrHcProb deckState players upcardVal = if dlrNewDeck deckState players then 
--     probValueBelow upcardVal (map (\x -> if (freq x - numRanks) > 0 then CardFreq (rank x) (freq x - 12) else CardFreq (rank x) 0) deckState) else 
--     probValueBelow upcardVal deckState

-- finds whether the dealer's hidden card is from a new deck
-- dlrNewDeck :: [CardFreq] -> Int -> Bool -- numPlayers or next players before dealer
-- dlrNewDeck deckState players = startingNumCards * players >= totalCards deckState

-- probWin :: [Card] -> Card -> [CardFreq] -> Double
-- probWin hand _ deckState = let
--     p = handCalc hand
--     -- d = handCalc [upcard]
--     -- probPNotBust = 
--     in probValueBelow p deckState


{---------------------------------
Memory Maintainance
---------------------------------}

deserialise :: Maybe String -> Memory
deserialise memo = case parse parseMemory <$> memo of
    Just (Result _ m) -> m
    Just (Error _) -> trace "Error occured in deserialising memory!" initMemory
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
            Bid _ -> [c] ++ concat (playerInfoHand <$> filter (("dealer" /=) . _playerInfoId) info) ++ hand
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
Tree
---------------------------------}

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

makeTree :: Int -> HandValue -> [CardFreq] -> Hand -> Tree Load
makeTree n maxVal deckState hand = Node (L n (handValue hand) 1.0 hand) $
    makeTree_ (n-1) maxVal 1.0 deckState <$> newHand
    where
        newHand = possHands maxVal deckState hand
        -- availRanks = rank <$> filter ((>0) . freq) deckState
        -- newHand = (:hand) <$> filter ((maxVal >=) . handValue . (:hand)) (Card Spade <$> availRanks)

makeTree_ :: Int -> HandValue -> Double -> [CardFreq] -> Hand -> Tree Load
makeTree_ 0 _ prob' deckState hand = Node (L 0 newTotal newProb hand) []
    where
        newTotal = handValue hand
        newProb = jointProb prob' hand deckState
makeTree_ n maxVal prob' deckState hand = Node (L n newTotal newProb hand) $
    if terminal then [] 
    else makeTree_ (n-1) maxVal newProb newDeckState <$> newHand
        where
            newTotal = handValue hand
            terminal = newTotal == Bust || newTotal == Combo || newTotal == Charlie || newTotal >= maxVal

            newProb = jointProb prob' hand deckState -- newProb is with new head
            newDeckState = if null hand then deckState else updateDeckState [head hand] deckState
            newHand = possHands maxVal newDeckState hand

possHands :: HandValue -> [CardFreq] -> [Card] -> [[Card]]
possHands maxVal deckState hand = handsLTmaxVal
    where
        availableRanks = rank <$> filter ((>0) . freq) deckState
        -- handsLTmaxVal = foldr (\v a -> if handValue (v:hand) <= maxVal then (v:hand):a else a) [] (Card Spade <$> availableRanks)
        handsLTmaxVal = (:hand) <$> (Card Spade <$> availableRanks)

jointProb :: Double -> [Card] -> [CardFreq] -> Double
jointProb _ [] _ = 1.0 -- base probability starts from 1
jointProb prob' hand deckState = fromIntegral numOfRank / fromIntegral (totalCards deckState) * prob'
    where
        rank' = getRank (head hand)
        numOfRank = rankEq rank' deckState

rankEq :: Rank -> [CardFreq] -> Int
rankEq rank' = foldr (\v a -> if rank v == rank' then a + freq v else a) 0

sumTree :: Tree Load -> Double
sumTree (Node a []) = prob a -- only take probabilities at the leaves
sumTree (Node _ trees) = sum (sumTree <$> trees)

-- finds probability of val at every node of the tree
-- jointProbEq :: HandValue -> Tree Load -> Double
-- jointProbEq val (Node a trees)
--     | total a == val = prob a
--     | otherwise = sum (jointProbEq val <$> trees)

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

-- makeTree 4 (Value 16) deckState [upcard] -- dealer
-- makeTree (5 - length hand) Combo deckState hand -- player

-- makeTree 5 (Value 5) (CardFreq <$> [Ace ..] <*> [numRanks]) []
-- makeTree 5 (Combo) (CardFreq <$> [Ace ..] <*> [numRanks]) []
-- makeTree 4 (Combo) (CardFreq <$> [Ace ..] <*> [numRanks]) []
-- makeTree 5 Combo (CardFreq <$> [Ace ..] <*> [numRanks]) []
-- makeTree 2 (Combo) [CardFreq Ace 1, CardFreq Ten 1] []
-- makeTree 3 (Value 18) [CardFreq Six 1, CardFreq Eight 1, CardFreq King 2] []
-- makeTree 3 (Combo) [CardFreq Six 2, CardFreq Four 2, CardFreq Six 2] [Card Spade Two]

-- jointProbBelow :: HandValue -> Tree Load -> Double
-- jointProbBelow val (Node a trees)
--     | total a < val = prob a + sum (valTree val <$> trees)
--     | total a == val = prob a
--     | otherwise = 0



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
    CardFreq r <$> parseInt

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

traceIf :: Bool -> String -> p -> p
traceIf True  s x = trace s x
traceIf False _ x = x

filter' :: (a -> Bool) -> [a] -> ([a], [a])
filter' f alist = (filter f alist, filter (not . f) alist)

numRanks :: Int
numRanks = numDecks * 4

-- powerset :: [a] -> [[a]]
-- powerset [] = [[]]
-- powerset (x:xs) = (x:) <$> (((x:) <$> powerset xs) ++ powerset xs)






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


