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



-- | This function is called once it's your turn, and keeps getting called until your turn ends.
playCard :: PlayFunc
playCard upcard points info pid memo hand
    | trace ("id: " ++ show pid ++ " upcard: " ++ show upcard) False = undefined
    | trace ("info: " ++ show info ++ " hand: " ++ show hand) False = undefined
    | trace ("memo: " ++ show memo ++ "\n======================================") False = undefined

    | otherwise = let
        newMemo = updateMemoryInfo (getNewCards pid upcard info hand newMemo) upcard (deserialise memo)
        action = case upcard of
            Nothing -> makeBid points newMemo
            Just c -> decideAction c hand newMemo
            --  case take 2 $ lastActions finalMemo of
            --     [Bid _] -> Hit 
            --     [DoubleDown _, Bid _] -> Hit
            --     [Hit, DoubleDown _] -> Stand
            --     _ -> Hit 
        finalMemo = updateMemoryAction action newMemo
        in (action, show finalMemo)


        -- action = decideAction upcard points hand finalMemo
                -- case head (lastActions finalMemo) of
                -- (Bid _) -> Hit -- decide2nd upcard hand memo
                -- (DoubleDown _) -> Hit
                -- -- [Hit, DoubleDown _, Bid _] -> Stand
                -- _ -> Hit -- playHand upcard hand memo
            -- Just Ace -> Insurance 50
            -- Just c -> trace ("decideAction " ++ show upcard ++ show hand ++ show memo) decideAction c hand finalMemo

-- playCard (Just (Card Heart Nine)) [PlayerPoints "0" 900, PlayerPoints "2" 800] [PlayerInfo "2" [],PlayerInfo "0" []] "0" (Just "100,[A:12,2:12,3:12,4:12,5:12,6:12,7:12,8:12,9:12,T:12,J:12,Q:12,K:12],[Bid 100],Nothing") [Card Spade Ace, Card Heart Two]


{---------------------------------
Bidding & Actions?
---------------------------------}

makeBid :: [PlayerPoints] -> Memory -> Action
makeBid _ _ = Bid maxBid

decideAction :: Card -> [Card] -> Memory -> Action
decideAction upcard hand memo = case trace ("decideAction " ++ show upcard ++ show hand ++ show memo) lastActions memo of
    [Bid _] -> decide2nd upcard hand memo
    [DoubleDown _, Bid _] -> Hit
    [Hit, DoubleDown _, Bid _] -> Stand
    _ -> playHand upcard hand memo
-- decideAction upcard hand memo = case trace ("decideAction " ++ show upcard ++ show hand ++ show memo) lastActions memo of
--     [Bid 100] -> Hit -- decide2nd upcard hand memo
--     [DoubleDown 100, Bid 100] -> Hit
--     [Hit, DoubleDown 100, Bid 100] -> Stand
--     _ -> Hit -- playHand upcard hand memo

-- decideAction (Card Spade Ace) [Card Heart Two, Card Club Three] (Memory 100 [CardFreq Ace 3, CardFreq Two 3, CardFreq Three 3] [Bid 100] Nothing)

decide2nd :: Card -> [Card] -> Memory -> Action
decide2nd upcard hand memo
    | trace "decide2nd" False = undefined
    -- Insurance
    | getRank upcard == Ace = if probValue' 10 (deckState memo) > 67 then Insurance (bid `div` 2) else playHand upcard hand memo
    -- Split
    | getRank (head hand) == getRank (head (tail hand)) &&
      (getRank (head hand) == Ace || getRank (head hand) == Eight) = Split bid
    -- Double
    | handCalc hand <= 11 = DoubleDown bid

    | otherwise = playHand upcard hand memo
    where bid = currBid memo

-- Hit Stand DoubleDown Split
playHand :: Card -> [Card] -> Memory -> Action
playHand upcard hand memo
    | trace ("playHand " ++ show p ++ " " ++ show d) False = undefined
    | diff > 0 = Hit
    | diff == 0 = Hit
    | diff < 0 = Stand
    | otherwise = Stand
    where
        p = probValueBelow' (21 - handCalc hand) (deckState memo)
        d = probValueBelow' (21 - handCalc [upcard]) (deckState memo)
        diff = p - d

-- calculates the probability of the dealer's hidden card (below 17?)
-- dlrHcProb :: [CardFreq] -> Int -> Int -> Float
-- dlrHcProb deckState players upcardVal = if dlrNewDeck deckState players then 
--     probValueBelow upcardVal (map (\x -> if (freq x - numRanks) > 0 then CardFreq (rank x) (freq x - 12) else CardFreq (rank x) 0) deckState) else 
--     probValueBelow upcardVal deckState

-- finds whether the dealer's hidden card is from a new deck
-- dlrNewDeck :: [CardFreq] -> Int -> Bool -- numPlayers or next players before dealer
-- dlrNewDeck deckState players = startingNumCards * players >= totalCards deckState

probWin :: [Card] -> Card -> [CardFreq] -> Float
probWin hand _ deckState = let
    p = handCalc hand
    -- d = handCalc [upcard]
    -- probPNotBust = 
    in probValueBelow p deckState

probValueBelow :: Fractional a => Int -> [CardFreq] -> a
probValueBelow val deckState = trace "probValueBelow" fromIntegral (cardsBelow (targetValue - val) deckState) / fromIntegral (totalCards deckState)

probValueBelow' :: Int -> [CardFreq] -> Int
probValueBelow' val deckState = trace "probValueBelow'" (cardsBelow (targetValue - val) deckState) * 100 `div` totalCards deckState

probValue :: Fractional a => Int -> [CardFreq] -> a
probValue val deckState = fromIntegral (foldr (\v a -> if rankValue (rank v) == val then freq v else a) 0 deckState) / fromIntegral (totalCards deckState)

probValue' :: Int -> [CardFreq] -> Int
probValue' val deckState = foldr (\v a -> if rankValue (rank v) == val then freq v else a) 0 deckState * 100 `div` totalCards deckState

cardsBelow :: Int -> [CardFreq] -> Int
cardsBelow val = foldr (\v a -> if rankValue (rank v) <= val then a + freq v else a) 0

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
    Just (Error _) -> initMemory -- trace (Error "") ""
    Nothing -> initMemory

updateMemoryInfo :: [Card] -> Maybe Card -> Memory -> Memory
updateMemoryInfo newCards upcard oldMemo = oldMemo {deckState = updateDeckState newCards (deckState oldMemo), lastUpcard = upcard}
    -- in case action of
    --     Bid amt -> Memory amt (updateDeckState newCards deckState_) [action] upcard
    --     _ -> Memory (currBid oldMemo) (updateDeckState newCards deckState_) (action : lastActions_) upcard

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


