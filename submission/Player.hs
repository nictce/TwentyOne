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
    | traceIf (pid == "0") ("id: " ++ show pid ++ " upcard: " ++ show upcard) False = undefined
    | traceIf (pid == "0") ("info: " ++ show info ++ " hand: " ++ show hand) False = undefined
    | traceIf (pid == "0") ("memo: " ++ show memo ++ "\n======================================") False = undefined

    | otherwise
    = let
        newMemo = updateMemoryInfo upcard pid info hand $ deserialise memo
        action = case upcard of
            Nothing -> makeBid pid points newMemo
            Just c -> decideAction c hand newMemo
        finalMemo = updateMemoryAction action newMemo
        in (action, show finalMemo)


{---------------------------------
Bidding & Actions?
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
    where bid = currBid memo

-- Hit Stand DoubleDown Split
playHand :: Card -> [Card] -> Memory -> Action
playHand upcard hand memo
    | phand < 11 = Hit
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
        diff = p - d

-- probValueBelow 10 [CardFreq Ace 4, CardFreq Two 7, CardFreq Three 6, CardFreq Four 5, CardFreq Five 8, CardFreq Six 3, CardFreq Seven 8, CardFreq Eight 4, CardFreq Nine 3, CardFreq Ten 8, CardFreq Jack 5, CardFreq Queen 4, CardFreq King 7]

-- calculates the probability of the dealer's hidden card (below 17?)
-- dlrHcProb :: [CardFreq] -> Int -> Int -> Float
-- dlrHcProb deckState players upcardVal = if dlrNewDeck deckState players then 
--     probValueBelow upcardVal (map (\x -> if (freq x - numRanks) > 0 then CardFreq (rank x) (freq x - 12) else CardFreq (rank x) 0) deckState) else 
--     probValueBelow upcardVal deckState

-- finds whether the dealer's hidden card is from a new deck
-- dlrNewDeck :: [CardFreq] -> Int -> Bool -- numPlayers or next players before dealer
-- dlrNewDeck deckState players = startingNumCards * players >= totalCards deckState

probWin :: [Card] -> Card -> [CardFreq] -> Double
probWin hand _ deckState = let
    p = handCalc hand
    -- d = handCalc [upcard]
    -- probPNotBust = 
    in probValueBelow p deckState

probValueBelow :: Int -> [CardFreq] -> Double
probValueBelow val deckState = fromIntegral (cardsBelow val deckState) / fromIntegral (totalCards deckState)

-- probValueBelow' :: Int -> [CardFreq] -> Int
-- probValueBelow' val deckState = cardsBelow (targetValue - val) deckState * 100 `div` totalCards deckState

probValue :: Int -> [CardFreq] -> Double
probValue val deckState = fromIntegral (foldr (\v a -> if rankValue (rank v) == val then freq v else a) 0 deckState) / fromIntegral (totalCards deckState)

-- probValue' :: Int -> [CardFreq] -> Int
-- probValue' val deckState = foldr (\v a -> if rankValue (rank v) == val then freq v else a) 0 deckState * 100 `div` totalCards deckState

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


{---------------------------------
Memory Maintainance
---------------------------------}

deserialise :: Maybe String -> Memory
deserialise memo = case parse parseMemory <$> memo of
    Just (Result _ m) -> m
    Just (Error _) -> trace "Error occured in deserialising memory!" initMemory
    Nothing -> initMemory

-- (getNewCards pid upcard info hand newMemo)
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

-- search (Node a []) _ = a
-- search (Node (Load ))

makeTree :: Int -> HandValue -> Double -> [CardFreq] -> Hand -> Tree Load
makeTree 0 _ prob' deckState hand = Node (L 0 (handValue hand) (jointProb prob' hand deckState) hand) []
makeTree n val prob' deckState hand = Node (L n val' prob'' hand) $
    if terminal then [] else makeTree (n-1) val prob'' deckState <$> hand'
        where
            hand' = (:hand) <$> filter ((val >=) . handValue . (:hand)) (Card Spade <$> availRanks)
            availRanks = rank <$> filter ((>0) . freq) (updateDeckState hand deckState)
            prob'' = jointProb prob' hand deckState
            val' = handValue hand
            terminal = val' == Bust || val' == Combo || val' == Charlie

-- makeTree 5 (Value 5) 1.0 (CardFreq <$> [Ace ..] <*> [numRanks]) []
-- makeTree 2 (Combo) 1.0 [CardFreq Ace 1, CardFreq Ten 1] []
-- makeTree 3 (Value 18) 1.0 [CardFreq Six 1, CardFreq Eight 1, CardFreq King 2] []

-- Node 2,0P,1.0,[] [
    -- Node 1,11P,0.5,[SA] 
    --     [
    --         Node 0,Combo,0.5,[ST,SA] []
    --     ],
    -- Node 1,10P,0.5,[ST] [
    --     Node 0,Combo,0.5,[SA,ST] []]]

-- Node 3,0P,1.0,[] [
--     Node 2,6P,0.25,[S6] [
--         Node 1,14P,8.333333333333333e-2,[S8,S6] [
--             Node 0,Bust,8.333333333333333e-2,[SK,S8,S6] []
--         ],
--         Node 1,16P,0.16666666666666666,[SK,S6] [
--             Node 0,Bust,8.333333333333333e-2,[S8,SK,S6] [],
--             Node 0,Bust,0.0,[SK,SK,S6] []
--         ]
--     ],
--     Node 2,8P,0.25,[S8] [
--         Node 1,14P,8.333333333333333e-2,[S6,S8] [
--             Node 0,Bust,8.333333333333333e-2,[SK,S6,S8] []
--         ],
--         Node 1,18P,0.16666666666666666,[SK,S8] [
--             Node 0,Bust,8.333333333333333e-2,[S6,SK,S8] [],
--             Node 0,Bust,0.0,[SK,SK,S8] []
--         ]
--     ],
--     Node 2,10P,0.5,[SK] [
--         Node 1,16P,0.16666666666666666,[S6,SK] [
--             Node 0,Bust,8.333333333333333e-2,[S8,S6,SK] [],
--             Node 0,Bust,0.0,[SK,S6,SK] []
--         ],
--         Node 1,18P,0.16666666666666666,[S8,SK] [
--             Node 0,Bust,8.333333333333333e-2,[S6,S8,SK] [],
--             Node 0,Bust,0.0,[SK,S8,SK] []
--         ]
--     ]
-- ]

jointProb :: Double -> [Card] -> [CardFreq] -> Double
jointProb _ [] _ = 1.0
jointProb prob' hand deckState = fromIntegral newRankFreq / fromIntegral (totalCards deckState') * prob'
    where
        rank' = getRank (head hand)
        newRankFreq = rankEq rank' deckState' - length (filter ((==rank') . getRank) hand) + 1
        deckState' = updateDeckState (tail hand) deckState -- deck with hand without new card

rankEq :: Rank -> [CardFreq] -> Int
rankEq rank' = foldr (\v a -> if rank v == rank' then a + freq v else a) 0

-- instance Foldable Tree where
--     foldMap f (Node a []) = f a
--     foldMap f (Node a trees) = [f a] ++ (concat (foldMap f <$> trees))

-- instance Functor Load where
--     fmap f l = f $ prob l

sumTree :: Tree Load -> Double
sumTree (Node a []) = prob a -- only take probabilities at the leaves
sumTree (Node _ trees) = sum (sumTree <$> trees) -- + (if total a /= Value 0 then prob a else 1 - prob a) 

-- valTree :: HandValue -> Int -> Tree Load -> Double
-- valTree val _ (Node a []) = if total a == val then prob a else 0 -- only take probabilities at the leaves
valTree :: HandValue -> Tree Load -> Double
valTree val (Node a trees) = if total a == val then prob a else sum (valTree val <$> trees)

-- resultTable tree = 

-- jointProbBelow turns val deckState hand = sum 
--     where tree = makeTree turns val 1.0 deckState hand

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
numRanks = numDecks *4

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = (x:) <$> (((x:) <$> powerset xs) ++ powerset xs)



{---------------------------------
Parsers
---------------------------------}

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

parseRank :: Parser Rank
parseRank = parseShow [Ace ..]

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

-- isNot :: Char -> Parser Char
-- isNot c = satisfy (/= c)

-- digit :: Parser Char
-- digit = satisfy isDigit

space :: Parser Char
space = satisfy isSpace

spaces :: Parser String
spaces = list space

-- sequenceParser :: [Parser a] -> Parser [a]
-- sequenceParser = sequence

-- thisMany :: Int -> Parser a -> Parser [a]
-- thisMany n l = sequenceParser (replicate n l)

string :: String -> Parser String
string = traverse is

tok :: Parser a -> Parser a
tok p = do
    r <- p
    _ <- spaces
    pure r

-- charTok :: Char -> Parser Char
-- charTok c = tok (is c)

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

-- parse a character and see if it is part of the string s
-- oneof :: String -> Parser Char
-- oneof s = satisfy (`elem` s)

-- noneof :: String -> Parser Char
-- noneof s = satisfy (`notElem` s)

parseInt :: Parser Int
parseInt = P $ \s -> case readInt s of
    Just (v, r) -> Result r v
    Nothing -> Error $UnexpectedString s

parseList :: Parser a -> Parser [a]
parseList parser = do
    _ <- stringTok "["
    result <- sepby parser commaTok
    _ <- stringTok "]"
    pure result

-- parseShow :: (Functor f, Show a) => f a -> f (Parser a)
-- parseShow = fmap $ (>>) . stringTok . show <*> pure
-- parseShow = fmap (((>>) . stringTok . show) <*> pure)
-- parseShow = fmap (((>>) . stringTok . show) <*> pure)

parseShow_ :: Show b => b -> Parser b
parseShow_ a = stringTok (show a) >> pure a

parseShow :: (Foldable t, Functor t, Show a) => t a -> Parser a
parseShow alist = foldr1 (|||) (parseShow_ <$> alist)

-- parseShow :: Show b => b -> Parser b
-- parseShow alist = P $ \s -> case parse (stringTok s) s of
--     Result r str -> foldr1 (\a v -> if show v == str then (pure v) else a) (Error (UnexpectedString s)) alist
--     _ -> Error (UnexpectedString s)

-- parseShow :: (Foldable t, Show a, Functor t) => t a -> Parser a
-- parseShow alist = P $ \s -> case parse (stringTok s) s of
--     Result r str -> foldr (\a v@(Result _ n) -> if show n == str then v else a) (Error (UnexpectedString s)) (Result r <$> alist)






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


