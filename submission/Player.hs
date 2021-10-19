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
import TwentyOne.Types
import Text.ParserCombinators.ReadP (sepBy)

traceIf :: Bool -> String -> p -> p
traceIf True  s x = trace s x
traceIf False _ x = x

-- | This function is called once it's your turn, and keeps getting called until your turn ends.
playCard :: PlayFunc
playCard upcard points info pid memo hand
    | traceIf (upcard == Nothing) "====================" False = undefined
    | trace "***" False = undefined
    | trace ("id: " ++ show pid ++ " points: " ++ show points) False = undefined
    | trace ("upcard: " ++ show upcard ++ " hand: " ++ show hand ++ " memo: " ++ show memo) False = undefined
    | trace ("info: " ++ show info) False = undefined
    | trace ("newMemo: " ++ show ()) False = undefined


    | otherwise = let
        action = case getRank <$> upcard of
        -- Just Ace -> if length (read hand) <= 3 then (Insurance 50, "") else (Hit, "")
            Just Ace -> Insurance 50
            Just _ -> Hit
            Nothing -> Bid 100
        -- newMemo = updateMemory 100 
        --     (if length hand <= 2 then hand else [head hand]) 
        --     action
        --     (case parse parseMemory <$> memo of
        --         Just (Result _ m) -> updateMemory 100 (concat (playerInfoHand <$> info)) action m
        --         _ -> Memory 100 [] []
        --             where )
        -- in (action, show newMemo)
        newMemo = case parse parseMemory <$> memo of
            Just (Result _ m) -> show $ updateMemory 100 (concat (playerInfoHand <$> info)) action m
            Nothing -> show $ Memory 100 (zipWith CardFreq [Ace ..] (replicate 13 12)) []
            Just e@(Error _) -> trace (show e) ""
        in (action, newMemo)



-- <memory> ::= <currBid> ";" <deckState> ";" <lastActions>
-- <currBid> ::= <int>
-- <int> ::= <digit> | <digit><int>
-- <digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

data Memory = Memory {
    currBid :: Int,
    deckState :: [CardFreq],
    lastActions :: [Action]
}

instance Show Memory where
    show m = intercalate "," values where
        show_ f = show (f m)
        values = [
            show_ currBid,
            show_ deckState,
            show_ lastActions]

parseMemory :: Parser Memory
parseMemory = do
    bid <- parseInt
    _ <- commaTok
    deck <- parseList parseCardFreq
    _ <- commaTok
    actions <- parseList parseAction_
    pure $ Memory bid deck actions

updateMemory :: Int -> [Card] -> Action -> Memory -> Memory
updateMemory bid newCards newAction oldMemory = 
    Memory bid (updateFreq <$> newCards <*> deckState oldMemory) (newAction : lastActions oldMemory)
    -- Memory bid (map (updateFreq <$> newCards) (deckState oldMemory)) (newAction : lastActions oldMemory)

updateFreq :: Card -> CardFreq -> CardFreq
updateFreq card cardFreq = if getRank card == rank cardFreq then CardFreq (getRank card) (freq cardFreq - 1) else cardFreq

-- updateFreq_ cardFreq = traverse (\card -> if getRank card == rank cardFreq then pure (freq cardFreq - 1) else cardFreq)
-- updateFreq_ :: [CardFreq] -> [CardFreq]
-- updateFreq_ cardFreq = foldr (\a v -> map (\x -> if rank v == rank x then CardFreq (rank x) (min (freq v) (freq x)) else v) a) []
-- updateFreq_ newCards oldMemory = foldr (\a v -> a) oldMemory

-- <deckState> ::= "[" <cardFreqs> "]"
-- <cardFreqs> ::= <cardFreq> | <cardFreq> "," <cardFreqs>
-- <cardFreq> ::= <rank> ":" <int> 

data DeckState = DeckState {ace, two, three, four, five, six, seven, eight, nine, tens :: Int}
    -- deriving (Show)

-- instance Show DeckState where
--     show d = "A" ++ show (ace d) ++ "A" ++ show ace ++ "A" ++ show ace ++ "A" ++ show ace ++ "A" ++ show ace ++ "A" ++ show ace ++ "A" ++ show ace ++ "A" ++ show ace ++ "A" ++ show ace ++ "A" ++ show ace ++ "A" ++ show ace

instance Show DeckState where
    show m = intercalate "," values where
        show_ f = show (f m)
        values = show_ <$> [ace, two, three, four, five, six, seven, eight, nine, tens]

data CardFreq = CardFreq {
    rank :: Rank,
    freq :: Int
}

parseDeckState :: Parser [Int]
parseDeckState = (sepby parseInt commaTok)

instance Show CardFreq where
    show cf = show (rank cf) ++ ":" ++ show (freq cf)

parseCardFreq :: Parser CardFreq
parseCardFreq = do
    r <- parseRank
    _ <- stringTok ":"
    CardFreq r <$> parseInt

parseRank :: Parser Rank
parseRank = (stringTok "A" >> pure Ace) ||| 
    (stringTok "2" >> pure Two) ||| 
    (stringTok "3" >> pure Three) ||| 
    (stringTok "4" >> pure Four) ||| 
    (stringTok "5" >> pure Five) ||| 
    (stringTok "6" >> pure Six) ||| 
    (stringTok "7" >> pure Seven) ||| 
    (stringTok "8" >> pure Eight) ||| 
    (stringTok "9" >> pure Nine) ||| 
    (stringTok "T" >> pure Ten) ||| 
    (stringTok "J" >> pure Jack) ||| 
    (stringTok "Q" >> pure Queen) ||| 
    (stringTok "K" >> pure King)

-- <lastActions> ::= "[" <actions> "]"
-- <actions> ::= <action> | <action> "," <actions>
-- <action> ::= "H" | "ST" | <doubleDown> | <split> | <bid> | <insurance>
-- <doubleDown> ::= "DD" <int>
-- <split> ::= "SP" <int>
-- <bid> ::= "B" <int>
-- <insurance> ::= "I" <int>

parseAction_ :: Parser Action
parseAction_ =  (string "Hit" >> pure Hit) |||
    (string "Stand" >> pure Stand) |||
    (string "DoubleDown " >> parseInt >>= pure . DoubleDown) |||
    (string "Split " >> parseInt >>= pure . Split) |||
    (string "Bid " >> parseInt >>= pure . Bid) |||
    (string "Insurance " >> parseInt >>= pure . Insurance)

list :: Parser a -> Parser [a]
list p = list1 p ||| pure []

list1 :: Parser a -> Parser [a]
list1 p = p >>= (\p' -> list p >>= (\p''-> pure (p':p'')))

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
  c <- character
  (if f c then pure else unexpectedCharParser) c

isNot :: Char -> Parser Char
isNot c = satisfy (/= c)

digit :: Parser Char
digit = satisfy isDigit

space :: Parser Char
space = satisfy isSpace

spaces :: Parser String
spaces = list space

sequenceParser :: [Parser a] -> Parser [a]
sequenceParser = sequence

thisMany :: Int -> Parser a -> Parser [a]
thisMany n l = sequenceParser (replicate n l)

string :: String -> Parser String
string = traverse is

tok :: Parser a -> Parser a
tok p = do
    r <- p
    _ <- spaces
    pure r

charTok :: Char -> Parser Char
charTok c = tok (is c)

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

oneof :: String -> Parser Char
oneof s = satisfy (`elem` s)

noneof :: String -> Parser Char
noneof s = satisfy (`notElem` s)

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

-- parseShow list shown = P $ \s -> 






