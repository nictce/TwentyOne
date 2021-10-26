-- | This is the file you need to implement to complete the assignment. Remember
-- to comment where appropriate, use generic types and have fun!

module Player where

-- Parsers
import Parser.Parser
import Parser.Instances

-- Cards
import Cards

-- Blackjack Game Logic
import TwentyOne.Types
import TwentyOne.Rules
import TwentyOne.Play

-- Other Modules
import Data.List
import Data.Char


{-------------------------------------------------------------------------------
Declarations
-------------------------------------------------------------------------------}

-- The data type that details the game 'state', i.e. the player's memory. 
-- Every value in the Memory is important to the workings of the player, 
-- and no attribute is redundant.
data Memory = Memory {
    currBid       :: Int,
    deckState     :: [CardFreq],
    lastActions   :: [Action],
    lastUpcard    :: Maybe Card,
    unplayedCards :: Int
}

-- The data type that is used to store the number of cards of a given rank left 
-- in the deck. 
data CardFreq = CardFreq {
    rank :: Rank,
    freq :: Int
}

-- Serialises the Memory to convert it into a String. 
instance Show Memory where
    show m = intercalate "," values where
        show_ f = show (f m)
        values = [
            show_ currBid,
            show_ deckState,
            show_ lastActions,
            show_ lastUpcard,
            show_ unplayedCards]

-- Serialises a CardFreq to convert it into a String. 
instance Show CardFreq where
    show cf = show (rank cf) ++ ":" ++ show (freq cf)

-- The data type that is used for creating a tree with a payload and any number 
-- of subtrees as its children.
data Tree a = Node a [Tree a] 
    deriving (Show)

-- The data type that is used in a Tree to form a Probability Tree. 
-- This includes vital attributes like the ranks (represented as cards)
-- associated with the Node, the total value of that combination of ranks, 
-- and the probability of getting that rank combination. 
data Load = L {
    height :: Int,
    total  :: HandValue,
    prob   :: Double,
    ranks  :: Hand
}

-- An instance of showing a Load (mostly for debugging purposes). 
instance Show Load where
    show l = intercalate "," values where
        show_ f = show (f l)
        values = [
            show_ height,
            show_ total,
            show_ prob,
            show_ ranks]

-- Instances for comparing PlayerPoints so that the players can be sorted 
-- according to the amount of points they possess. 
instance Eq PlayerPoints where
    (PlayerPoints _ p1) == (PlayerPoints _ p2) = p1 == p2
instance Ord PlayerPoints where
    (PlayerPoints _ p1) `compare` (PlayerPoints _ p2) = p1 `compare` p2



{-------------------------------------------------------------------------------
playCard
-------------------------------------------------------------------------------}

-- General Algorithm: 
-- 1. Deserialise memory and update it with the latest information.
-- 2. Choose an action based on the newest information stored in the memory
--    This would make a bid if it is currently the first turn of the round
--    or choose other types of actions for subsequent rounds.
-- 3. Update the memory with the chosen action.

playCard :: PlayFunc
playCard upcard points info pid memo hand = (action, show finalMemo)
    where
        newMemo = updateMemoryInfo upcard pid info hand $ deserialise memo
        action = case upcard of
            Nothing -> makeBid pid points newMemo
            Just c  -> decideAction c hand pid points newMemo
        finalMemo = updateMemoryAction action newMemo



{-------------------------------------------------------------------------------
Parser Combinators for Deserialising the Memory
-------------------------------------------------------------------------------}

-- Deserialises a Maybe String into a Memory through the use of parsers.
deserialise :: Maybe String -> Memory
deserialise memo = case parse parseMemory <$> memo of
    Just (Result _ m) -> m
    _ -> initMemory

-- Parses a String into its components to be create a Memory.
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
    unplayed <- parseInt
    pure $ Memory bid deck actions card unplayed

-- Parses a Showable according to an input list of its possible type values.
parseShow :: (Foldable t, Functor t, Show a) => t a -> Parser a
parseShow = foldr1 (|||) . (<$>) parseShow_

-- Parses a Showable according to an inputted possible type value.
parseShow_ :: Show b => b -> Parser b
parseShow_ a = stringTok (show a) >> pure a

-- Parses an Integer.
parseInt :: Parser Int
parseInt = P $ \s -> case readInt s of
    Just (v, r) -> Result r v
    Nothing -> Error $UnexpectedString s

-- Parses a List that is enclosed by [] with items separated by commas.
parseList :: Parser a -> Parser [a]
parseList parser = do
    _ <- stringTok "["
    result <- sepby parser commaTok
    _ <- stringTok "]"
    pure result

-- Parse a CardFreq
parseCardFreq :: Parser CardFreq
parseCardFreq = do
    r <- parseRank
    _ <- stringTok ":"
    f <- parseInt
    pure $ CardFreq r f

-- Parses an Action.
parseAction_ :: Parser Action
parseAction_ =  parseShow [Hit, Stand] |||
    (stringTok "DoubleDown" >> parseInt >>= pure . DoubleDown) |||
    (stringTok "Split" >> parseInt >>= pure . Split) |||
    (stringTok "Bid" >> parseInt >>= pure . Bid) |||
    (stringTok "Insurance" >> parseInt >>= pure . Insurance)

-- Parses a Card wrapped in a Maybe context, or a Nothing.
parseMaybeCard :: Parser (Maybe Card)
parseMaybeCard = (stringTok "Just" >> pure <$> parseCard) ||| 
    (stringTok "Nothing" >> pure Nothing)

-- Parses a Card.
parseCard :: Parser Card
parseCard = do
    suit <- parseShow [Spade ..]
    rank_ <- parseRank
    pure $ Card suit rank_

-- Parses a Rank (using parseShow on the list of possible Rank values).
parseRank :: Parser Rank
parseRank = parseShow [Ace ..]

-- Returns a parser that continually parses a String and places the results in 
-- a List. Inspired from Parser.hs in Tutorial 11. 
list :: Parser a -> Parser [a]
list p = list1 p ||| pure []
    where list1 s = do
            s' <- s
            s'' <- list s
            pure (s':s'')

-- Parses any amount of spaces in a String. 
-- Inspired from Parser.hs in Tutorial 11. 
spaces :: Parser String
spaces = list (satisfy isSpace)
    where satisfy f = do
            c <- character
            (if f c then pure else unexpectedCharParser) c

-- Applies the given parser and ignores any trailing whitespaces. 
-- Inspired from Parser.hs in Tutorial 11. 
tok :: Parser a -> Parser a
tok p = do
    r <- p
    _ <- spaces
    pure r

-- Parses a comma and ignores any trailing whitespaces. 
-- Inspired from Parser.hs in Tutorial 11. 
commaTok :: Parser Char
commaTok = tok (is ',')

-- Parses the input string and ignores any trailing whitespaces. 
-- Inspired from Parser.hs in Tutorial 11. 
stringTok :: String -> Parser String
stringTok = tok . string
    where string = traverse is

-- Parses a list of values from applying the first parser, separated by values 
-- from the second parser. Inspired from Parser.hs in Tutorial 11. 
sepby :: Parser a -> Parser s -> Parser [a]
sepby p1 p2 = sepby1 p1 p2 ||| pure []
    where sepby1 a s = do
            x <- a
            xs <- list $ s >> a
            pure (x:xs)



{-------------------------------------------------------------------------------
Memory Maintainance
-------------------------------------------------------------------------------}

-- General Algorithm:
-- 1. Compile all new cards together according to the turn of the round. 
-- 2. Update the deck state with the new cards, handling the case when the deck 
--    is replenished. 
-- 3. Update the memory with the latest upcard from the dealer. 
-- 4. Find out if the dealer's hidden card was not played in the last round, 
--    and update accordingly. 
-- 5. Update the memory with the latest action once it has been decided. 

-- Returns a new Memory from updating an old Memory with the latest information.
updateMemoryInfo :: Maybe Card -> PlayerId -> [PlayerInfo] -> Hand -> Memory -> Memory
updateMemoryInfo upcard pid info hand oldMemo =
    oldMemo {
        deckState  = updateDeckState newCards (deckState oldMemo),
        lastUpcard = upcard,
        unplayedCards = unplayedCards oldMemo + dealerUnplayedCard info upcard
    }
    where newCards = getNewCards pid upcard info hand oldMemo

-- Maps each card to update the deck state via folding, refilling it if necessary.
updateDeckState :: Hand -> [CardFreq] -> [CardFreq]
updateDeckState newCards deckState = handleRefills (foldr (map . updateFreq) deckState newCards)
    where updateFreq card cardFreq
            | getRank card == rank cardFreq = CardFreq (getRank card) (freq cardFreq - 1)
            | otherwise = cardFreq

-- Replenishes the card frequencies if the deck is refilled, which happens 
-- if all frequencies are zero, or if any frequencies are negative.
handleRefills :: [CardFreq] -> [CardFreq]
handleRefills deckState = 
    if all ((0 ==) . freq) deckState || any ((0 >) . freq) deckState 
        then map (\cf -> CardFreq (rank cf) (freq cf + numRanks)) deckState 
        else deckState

-- Determines whether the dealer's hidden card was unplayed in the last round.
dealerUnplayedCard :: [PlayerInfo] -> Maybe Card -> Int
dealerUnplayedCard info upcard = case upcard of
    Nothing -> if null dealerList then 0 else 1
    _       -> 0
    where dealerList = filter (\d -> _playerInfoId d == dealerId && length (playerInfoHand d) == 1) info

-- Obtains all new card information based on the turn the player is in.
getNewCards :: PlayerId -> Maybe Card -> [PlayerInfo] -> Hand -> Memory -> Hand
getNewCards pid upcard info hand memo = let
    lastAction = head (lastActions memo)
    in case upcard of
        -- First turn (always remove the dealer's previous upcard)
        Nothing -> removeCard (lastUpcard memo) . concat $ (playerInfoHand <$>) $ 
            if lastAction == Hit 
                -- Only take the head of the player's hand if they Hit previously
                then includePlayerHead pid info 
                -- Else dont include the player at all
                else filter ((pid /=) . _playerInfoId) info

        Just c -> case lastAction of
            -- Second turn (take all new information except dealer's last hand)
            Bid _ -> [c] ++ playersInfo ++ hand
                where playersInfo = concat (playerInfoHand <$> filter ((dealerId /=) . _playerInfoId) info)
            -- Any subsequent turns
            Hit -> [head hand]
            _ -> []

-- Removes a Card (in a Maybe context) from a Hand.
removeCard :: Maybe Card -> Hand -> Hand
removeCard card hand = case card of
    (Just c) -> delete c hand
    Nothing  -> hand

-- Searches the player's info and only includes the head (the new card) while 
-- discarding the rest.
includePlayerHead :: PlayerId -> [PlayerInfo] -> [PlayerInfo]
includePlayerHead pid info = hands ++ [PlayerInfo pid [playerLatestCard]]
    where 
        (hands, phand)   = filterSplit ((pid /=) . _playerInfoId) info
        playerLatestCard = head (playerInfoHand (head phand))

-- Updates a Memory with an Action.
updateMemoryAction :: Action -> Memory -> Memory
updateMemoryAction action oldMemo = case action of
    Bid amt -> oldMemo {currBid = amt, lastActions = [action]}
    _       -> oldMemo {lastActions = action : lastActions oldMemo}



{-------------------------------------------------------------------------------
Bidding
-------------------------------------------------------------------------------}

-- General Algorithm:
-- 1. Find the ranking of the player among all players. 
-- 2. If the probability of getting a good hand is:
--    - High: Bid with the maximum bid, so that if you win you have a chance of 
--      overtaking the rank of the player above you. 
--    - Moderate: Bid with the difference against the player below you, so that 
--      if you lose, your rank won't drop too much, as the dealer likely won. 
--    - Low: Bid with the minimum bid. 
-- 3. Validate each bid to ensure that they fall within the maximum and minimum 
--    bids as well as the player's points, before returning the action. 

-- Makes a Bid based on the probability that the new hand would be favourable
-- The amount of the bid is based on the difference in points of adjacent ranks.
makeBid :: PlayerId -> [PlayerPoints] -> Memory -> Action
makeBid pid points memo
    | psafe > 4/10 = adjustBid maxBid
    | psafe > 3/10 = adjustBid $ min blw ((maxBid + minBid) `div` 2)
    | otherwise = adjustBid minBid
    where
        adjustBid = Bid . validAmt pid points
        ptree = makeTree 2 Combo memo []
        psafe = 1 - jointProbEq Bust ptree - jointProbLt (Value 18) ptree + 
            jointProbLt (Value 12) ptree - jointProbLt (Value 8) ptree
        blw = bidLowerBound pid points

-- Ensures that the amount being bid is valid, according to the player's points.
validAmt :: PlayerId -> [PlayerPoints] -> Points -> Points
validAmt pid points bid
    | pts < minBid = pts
    | bid > pts && pts <= maxBid = pts
    | otherwise = max minBid $ min maxBid bid
    where pts = getPoint pid points

-- Determines the upper and lower bounds of points that the player should bid.
-- We bid as much as the difference with the player below us and as low as the 
-- difference with the player above us to minimise losses and maximise profit.
bidLowerBound :: PlayerId -> [PlayerPoints] -> Points
bidLowerBound pid points = point - blw
    where
        sortedPoints = sort points
        point = getPoint pid points
        blw = _playerPoints $ foldl1 (\a v -> if _playerPoints v < point && v > a then v else a) sortedPoints



{-------------------------------------------------------------------------------
Actions
-------------------------------------------------------------------------------}

-- General Algorithm:
-- 1. Determine if the player should get Insurance, based on the probability of 
--    the dealer possessing a Ten, or process the required Actions following a 
--    DoubleDown.
-- 2. Determine if the player should Split or DoubleDown if the player has 
--    exactly two cards. This incorporates a bit of basic strategy. 
-- 3. Determine if the player should Hit or Stand, based on Probability Trees
--    that calculate the chances of going Bust for both the player and dealer. 

-- Decides the player's next action based on the given information. 
-- This function mostly deals with default Actions that must happen only after 
-- certain specific Actions. 
decideAction :: Card -> Hand -> PlayerId -> [PlayerPoints] -> Memory -> Action
decideAction upcard hand pid points memo = case take 2 $ lastActions memo of
    [Bid _] -> case upcard of
        Card _ Ace -> if dcombo > 2/3
            then Insurance (validAmt pid points $ currBid memo `div` 2)
            else altAction
        _ -> altAction
    [DoubleDown _, _] -> Hit
    [Hit, DoubleDown _] -> Stand
    _ -> altAction
    where
        dtree = makeTree 1 Combo memo [upcard]
        dcombo = jointProbEq Combo dtree
        altAction = splitOrDouble upcard hand pid points memo

-- Decides whether to Split or DoubleDown if the player has exactly 2 cards.
-- This incorporates some Basic Strategy with the use of Probability Trees, 
-- inspired by https://www.blackjackapprenticeship.com/blackjack-strategy-charts/.  
splitOrDouble :: Card -> Hand -> PlayerId -> [PlayerPoints] -> Memory -> Action
splitOrDouble upcard hand pid points memo
    | length hand /= 2 = hitOrStand upcard hand memo

    -- Split
    | canSplit hand && (headRank == Ace || headRank == Eight) = Split bid
    | canSplit hand && headRank <= Seven = 
        if headRank >= upRank && upRank /= Ace 
            then Split bid 
            else Hit

    -- Double
    | handVal == Value 11 = DoubleDown bid
    | hasAce hand && handVal <= Value 17 = 
        if upRank >= Four && upRank <= Six 
            then DoubleDown bid 
            else Hit
    | psafe > 1/3 && handVal > Value 11 = DoubleDown bid

    | otherwise = hitOrStand upcard hand memo
    where
        bid = validAmt pid points $ currBid memo
        upRank = getRank upcard
        headRank = getRank $ head hand
        handVal = handValue hand
        ptree = makeTree 1 Combo memo hand
        psafe = 1 - jointProbEq Bust ptree - jointProbLt (Value 18) ptree

-- Decides whether to hit or stand based on creating Probability Trees for both
-- the player and dealer to see whether the conditions are favourable for the 
-- player to Hit, otherwise the player would Stand. 
hitOrStand :: Card -> Hand -> Memory -> Action
hitOrStand upcard hand memo
    | handValue hand <= Value 11 = Hit
    | pbust <= 1/2 = Hit
    | handValue hand <= Value 14 && pbust <= 3/5 = Hit
    | dbust > 2/3 && dbust - pbust >= 1/5 = Hit
    | otherwise = Stand
    where
        ptree = makeTree 1 Combo memo hand
        pbust = jointProbEq Bust ptree
        -- We make a tree of 4 levels (for the dealer drawing at most 4 cards), 
        -- stopping any branching when the dealer gets a hand value of at least
        -- 17, to see all probabilities of the dealer going bust. 
        dtree = makeTree 4 (Value 17) memo [upcard]
        dbust = jointProbEq Bust dtree



{-------------------------------------------------------------------------------
Tree and Deck Statistics
-------------------------------------------------------------------------------}

-- Makes a Probability Tree with configuration for height and a value constraint.
makeTree :: Int -> HandValue -> Memory -> Hand -> Tree Load
makeTree n minVal memo hand = Node (L n (handValue hand) 1.0 hand) $
    makeNodes (n-1) minVal 1.0 memo <$> newHands
    where newHands = findPossibleHands (deckState memo) hand

-- The auxiliary function that creates each of the nodes within the Probability 
-- Tree, keeping its constraints by stopping any branching for terminal nodes. 
makeNodes :: Int -> HandValue -> Double -> Memory -> Hand -> Tree Load
-- Creates a leaf, since its height is 0. 
makeNodes 0 _ prob' memo hand = Node (L 0 newTotal newProb hand) []
    where
        newTotal = handValue hand
        newProb = jointProb prob' hand (deckState memo) (unplayedCards memo)
-- Creates all inner nodes of the tree. 
makeNodes n minVal prob' memo hand = Node (L n newTotal newProb hand) $
    if terminal 
        then []
        -- Create new nodes for each possible card that can be drawn
        else makeNodes (n-1) minVal newProb (memo {deckState = newDeckState}) <$> newHands
    where
        newTotal = handValue hand
        terminal = newTotal == Bust || newTotal == Combo || newTotal == Charlie || newTotal >= minVal

        oldDeckState = deckState memo
        -- Updates the deck state with the newest card from the current hand
        newDeckState = if null hand then oldDeckState else updateDeckState [head hand] oldDeckState

        -- Finds the probability of getting the hand given the newest card in 
        -- the head of the current hand
        newProb = jointProb prob' hand oldDeckState (unplayedCards memo)
        newHands = findPossibleHands newDeckState hand

-- Calculates the joint probability of a hand, given its current probability 
-- without the newest card in the head of the hand, offset by the number of 
-- unplayed dealer cards for an unbiased probability. 
jointProb :: Double -> Hand -> [CardFreq] -> Int -> Double
jointProb _ [] _ _ = 1.0 -- Base probability starts from 1
jointProb prob' hand deckState unplayedCards = rankFreq / totalFreq * prob'
    where
        newCardRank = getRank (head hand)
        rankFreq = fromIntegral $ getRankFreq newCardRank deckState
        totalFreq = fromIntegral (totalCards deckState + unplayedCards)

-- Finds all possible hands with new cards from each rank, given that there is 
-- at least one card of that rank in the deck. 
findPossibleHands :: [CardFreq] -> Hand -> [Hand]
findPossibleHands deckState hand = (:hand) <$> (Card Spade <$> availableRanks)
    where availableRanks = rank <$> filter ((>0) . freq) deckState

-- Sums up all probability values in the tree at the leaf nodes.  
sumTree :: Tree Load -> Double
sumTree (Node a []) = prob a
sumTree (Node _ trees) = sum (sumTree <$> trees)

-- Finds all probabilities less than the specified value at every leaf 
-- (at any level) of the tree. 
jointProbLt :: HandValue -> Tree Load -> Double
jointProbLt val (Node a []) = if total a < val then prob a else 0
jointProbLt val (Node _ trees) = sum (jointProbLt val <$> trees)

-- Finds all probabilities equal to the specified value at every leaf 
-- (at any level) of the tree
jointProbEq :: HandValue -> Tree Load -> Double
jointProbEq val (Node a []) = if total a == val then prob a else 0
jointProbEq val (Node _ trees) = sum (jointProbEq val <$> trees)

-- Finds all probabilities equal to the specified value at every leaf 
-- (at the bottom level) of the tree
jointProbBottom :: HandValue -> Tree Load -> Double
jointProbBottom val (Node a []) = if total a == val && height a == 0 then prob a else 0
jointProbBottom val (Node _ trees) = sum (jointProbBottom val <$> trees)



{-------------------------------------------------------------------------------
Utility & Auxiliary Functions
-------------------------------------------------------------------------------}

-- Creates the initial Memory at the start of a new game. 
initMemory :: Memory
initMemory = Memory 0 (CardFreq <$> [Ace ..] <*> [numRanks]) [Stand] Nothing 0

-- Contains the number of ranks for card frequencies, using the number of decks.
numRanks :: Int
numRanks = numDecks * 4

-- Filters a list and splits it into two lists that are mutually exclusive. 
filterSplit :: (a -> Bool) -> [a] -> ([a], [a])
filterSplit f alist = (filter f alist, filter (not . f) alist)

-- Obtains the points of a player by searching the input player id. 
getPoint :: PlayerId -> [PlayerPoints] -> Points
getPoint pid points = _playerPoints $ head $ filter ((pid ==) . _playerPointsId) points

-- Checks whether the current hand can be Split. 
canSplit :: Hand -> Bool
canSplit hand = length hand == 2 && getRank (head hand) == getRank (last hand)

-- Checks whether the current hand has an Ace. 
hasAce :: Hand -> Bool
hasAce = any ((== Ace) . getRank)

-- Gets the frequency of a rank from the deck state. 
getRankFreq :: Rank -> [CardFreq] -> Int
getRankFreq rank' = foldr (\v a -> if rank v == rank' then a + freq v else a) 0

-- Gets the total number of cards in the deck from the deck state. 
totalCards :: [CardFreq] -> Int
totalCards = foldr (\v a -> a + freq v) 0



{-------------------------------------------------------------------------------
BNF CFG
-------------------------------------------------------------------------------}

-- <memory> ::= <int> "," <deckState> "," <lastActions> "," <lastUpcard> "," <int>
-- <int> ::= <digit> | <digit><int>
-- <digit> ::= "0" | "1" | <twoToNine>
-- <twoToNine> ::= "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
-- <empty> ::= ""

-- <deckState> ::= "[" <empty> "]" | "[" <cardFreqs> "]"
-- <cardFreqs> ::= <cardFreq> | <cardFreq> "," <cardFreqs>
-- <cardFreq> ::= <rank> ":" <int> 

-- <lastActions> ::= "[" <empty> "]" | "[" <actions> "]"
-- <actions> ::= <action> | <action> "," <actions>
-- <action> ::= "Hit" | "Stand" | "DoubleDown" <int> | "Split" <int> | "Bid" <int> | "Insurance" <int>

-- <lastUpcard> ::= "Just" <card> | "Nothing"
-- <card> ::= <suit> <rank>
-- <suit> ::= "D" | "C" | "H" | "S"
-- <rank> ::= "A" | <twoToNine> | "T" | "J" | "Q" | "K"

