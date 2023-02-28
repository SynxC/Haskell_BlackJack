-- | This is the file you need to implement to complete the assignment. Remember
-- to comment where appropriate, use generic types and have fun!

module Player where

import           Parser.Parser      -- This is the source for the parser from the course notes
import           Cards              -- Finally, the generic card type(s)

import           TwentyOne.Types    -- Here you will find types used in the game of TwentyOne
import           TwentyOne.Rules    -- Rules of the game
-- Rules of the game
import           Data.Maybe
import           Debug.Trace
import           Parser.Instances
import           Data.Char
import           Control.Monad

-- You can add more imports if you need them
-- Strategy to be ran if the remaining deck is only 1.
singleDeckStrategy:: String -> Points -> Int -> Maybe Card -> Hand -> Action
singleDeckStrategy parsedAction currentPoints correctBid dealerUpCard playerHand = action
    where
         -- Double Down Conditions
        doDoubleDown
            | checkCards playerHand && handValue playerHand == Value 10 && justCardRank dealerUpCard < 10 = True
            | Ace `elem` map getRank playerHand && handValue playerHand == Value 13 && length playerHand == 2 && justCardRank dealerUpCard < 7 && justCardRank dealerUpCard >= 4 = True
            | Ace `elem` map getRank playerHand && handValue playerHand == Value 14 && length playerHand == 2 && justCardRank dealerUpCard < 7 && justCardRank dealerUpCard >= 4 = True
            | Ace `elem` map getRank playerHand && handValue playerHand == Value 15 && length playerHand == 2 && justCardRank dealerUpCard < 7 && justCardRank dealerUpCard >= 4 = True
            | Ace `elem` map getRank playerHand && handValue playerHand == Value 16 && length playerHand == 2 && justCardRank dealerUpCard < 7 && justCardRank dealerUpCard >= 4 = True
            | Ace `elem` map getRank playerHand && handValue playerHand == Value 17 && length playerHand == 2 && justCardRank dealerUpCard < 7 = True
            | Ace `elem` map getRank playerHand && handValue playerHand == Value 18 && length playerHand == 2 && justCardRank dealerUpCard < 7 && justCardRank dealerUpCard >= 3 = True
            | Ace `elem` map getRank playerHand && handValue playerHand == Value 19 && length playerHand == 2 && justCardRank dealerUpCard == 6 = True
            | handValue playerHand == Value 11 && length playerHand == 2 && currentPoints > 2000 = True
            | handValue playerHand == Value 10 && justCardRank dealerUpCard < 10 && length playerHand == 2 && currentPoints > 2000 = True
            | handValue playerHand == Value 9 && justCardRank dealerUpCard < 7 && length playerHand == 2 && currentPoints > 2000 = True
            | handValue playerHand == Value 8 && justCardRank dealerUpCard >= 4 && justCardRank dealerUpCard < 7 && length playerHand == 2 && currentPoints > 2000 = True
            | otherwise = False

        -- Stand Conditions
        doStand
            | handValue playerHand == Combo = True
            | handValue playerHand == Charlie = True
            | Ace `elem` map getRank playerHand && handValue playerHand > Value 17 && length playerHand == 2 && justCardRank dealerUpCard /= 9 = True
            | Ace `elem` map getRank playerHand && handValue playerHand > Value 17 && length playerHand == 2 && justCardRank dealerUpCard /= 10 = True
            | handValue playerHand > Value 16 = True
            | handValue playerHand >= Value 12 && justCardRank dealerUpCard >= 4 && justCardRank dealerUpCard < 7 = True
            | handValue playerHand >= Value 13 && justCardRank dealerUpCard < 7 = True
            | otherwise = False

        -- Split Conditions
        doSplit
            | handValue playerHand == Value 8
                || handValue playerHand == Value 10
                || handValue playerHand == Value 20
                    = False
            | handValue playerHand == Value 18 && justCardRank dealerUpCard == 7
                || handValue playerHand == Value 18 && justCardRank dealerUpCard == 10
                || handValue playerHand == Value 18 && justCardRank dealerUpCard == 11
                    = False
            | checkAces playerHand = True
            | handValue playerHand == Value 14 && justCardRank dealerUpCard >= 8 = False
            | not(checkAces playerHand) && handValue playerHand == Value 12 && justCardRank dealerUpCard >= 7 = False
            | handValue playerHand <= Value 6 && justCardRank dealerUpCard >= 8 = False
            | otherwise = True

        -- Returns the correct action
        action
            | justCardRank dealerUpCard == 11 && parsedAction == "Bid" = Insurance (correctBid`div`2)
            | parsedAction == "DoubleDown" = Hit
            | parsedAction == "DoubleHit"  = Stand
            | isNothing dealerUpCard = Bid correctBid
            | checkCards playerHand && doSplit && parsedAction /= "Split" = Split correctBid
            | doDoubleDown = DoubleDown correctBid
            | doStand = Stand
            | otherwise = Hit

-- Strategy to be ran if the remaining deck is only 2.
doubleDeckStrategy:: String -> Points -> Int -> Maybe Card -> Hand -> Action
doubleDeckStrategy parsedAction currentPoints correctBid dealerUpCard playerHand = action
    where
         -- Double Down Conditions
         -- Double Down Conditions
        doDoubleDown
            | Ace `elem` map getRank playerHand && handValue playerHand == Value 13 && length playerHand == 2 && justCardRank dealerUpCard < 7 && justCardRank dealerUpCard >= 5 = True
            | Ace `elem` map getRank playerHand && handValue playerHand == Value 14 && length playerHand == 2 && justCardRank dealerUpCard < 7 && justCardRank dealerUpCard >= 4 = True
            | Ace `elem` map getRank playerHand && handValue playerHand == Value 15 && length playerHand == 2 && justCardRank dealerUpCard < 7 && justCardRank dealerUpCard >= 4 = True
            | Ace `elem` map getRank playerHand && handValue playerHand == Value 16 && length playerHand == 2 && justCardRank dealerUpCard < 7 && justCardRank dealerUpCard >= 4 = True
            | Ace `elem` map getRank playerHand && handValue playerHand == Value 17 && length playerHand == 2 && justCardRank dealerUpCard < 7 && justCardRank dealerUpCard >= 3 = True
            | Ace `elem` map getRank playerHand && handValue playerHand == Value 18 && length playerHand == 2 && justCardRank dealerUpCard < 7 = True
            | Ace `elem` map getRank playerHand && handValue playerHand == Value 19 && length playerHand == 2 && justCardRank dealerUpCard == 6 = True
            | handValue playerHand == Value 11 && length playerHand == 2 && currentPoints > 2000 = True
            | handValue playerHand == Value 10 && justCardRank dealerUpCard < 10 && length playerHand == 2 && currentPoints > 2000 = True
            | handValue playerHand == Value 9 && justCardRank dealerUpCard < 7 && length playerHand == 2 && currentPoints > 2000 = True
            | otherwise = False

        -- Stand Conditions
        doStand
            | handValue playerHand == Combo = True
            | handValue playerHand == Charlie = True
            | Ace `elem` map getRank playerHand && handValue playerHand > Value 18 && length playerHand == 2 && justCardRank dealerUpCard >= 7 && justCardRank dealerUpCard < 9 = True
            | Ace `elem` map getRank playerHand && handValue playerHand > Value 19 && length playerHand == 2 && justCardRank dealerUpCard /= 6 = True
            | handValue playerHand > Value 16 = True
            | handValue playerHand >= Value 12 && justCardRank dealerUpCard >= 4 && justCardRank dealerUpCard < 7 = True
            | handValue playerHand >= Value 13 && justCardRank dealerUpCard < 7 = True
            | otherwise = False

        -- Split Conditions
        doSplit
            | handValue playerHand == Value 8
                || handValue playerHand == Value 10
                || handValue playerHand == Value 20
                    = False
            | handValue playerHand == Value 18 && justCardRank dealerUpCard == 7
                || handValue playerHand == Value 18 && justCardRank dealerUpCard == 10
                || handValue playerHand == Value 18 && justCardRank dealerUpCard == 11
                    = False
            | checkAces playerHand = True
            | handValue playerHand == Value 14 && justCardRank dealerUpCard >= 8 = False
            | not(checkAces playerHand) && handValue playerHand == Value 12 && justCardRank dealerUpCard >= 7 = False
            | handValue playerHand <= Value 6 && justCardRank dealerUpCard >= 8 = False
            | otherwise = True

        -- Returns the correct action
        action
            | justCardRank dealerUpCard == 11 && parsedAction == "Bid" = Insurance (correctBid`div`2)
            | parsedAction == "DoubleDown" = Hit
            | parsedAction == "DoubleHit"  = Stand
            | isNothing dealerUpCard = Bid correctBid
            | checkCards playerHand && doSplit && parsedAction /= "Split" = Split correctBid
            | doDoubleDown = DoubleDown correctBid
            | doStand = Stand
            | otherwise = Hit

-- Strategy to be ran if the remaining deck is only 3 or more.
tripleDeckStrategy:: String -> Points -> Int -> Maybe Card -> Hand -> Action
tripleDeckStrategy parsedAction currentPoints correctBid dealerUpCard playerHand = action
    where
         -- Double Down Conditions
        doDoubleDown
            | checkCards playerHand && handValue playerHand == Value 10 && justCardRank dealerUpCard < 10 = True
            | Ace `elem` map getRank playerHand && handValue playerHand == Value 13 && length playerHand == 2 && justCardRank dealerUpCard == 6 = True
            | Ace `elem` map getRank playerHand && handValue playerHand == Value 14 && length playerHand == 2 && justCardRank dealerUpCard < 7 && justCardRank dealerUpCard >= 5= True
            | Ace `elem` map getRank playerHand && handValue playerHand == Value 15 && length playerHand == 2 && justCardRank dealerUpCard < 7 && justCardRank dealerUpCard >= 4= True
            | Ace `elem` map getRank playerHand && handValue playerHand == Value 16 && length playerHand == 2 && justCardRank dealerUpCard < 7 && justCardRank dealerUpCard >= 4= True
            | Ace `elem` map getRank playerHand && handValue playerHand == Value 17 && length playerHand == 2 && justCardRank dealerUpCard < 7 && justCardRank dealerUpCard >= 3= True
            | Ace `elem` map getRank playerHand && handValue playerHand == Value 18 && length playerHand == 2 && justCardRank dealerUpCard < 7 = True
            | Ace `elem` map getRank playerHand && handValue playerHand == Value 19 && length playerHand == 2 && justCardRank dealerUpCard == 6 = True
            | handValue playerHand == Value 11 && length playerHand == 2 && currentPoints > 2000 = True
            | handValue playerHand == Value 10 && justCardRank dealerUpCard < 10 && length playerHand == 2 && currentPoints > 2000 = True
            | handValue playerHand == Value 9 && justCardRank dealerUpCard >= 3 && justCardRank dealerUpCard < 7 && length playerHand == 2 && currentPoints > 2000 = True
            | otherwise = False

        -- Stand Conditions
        doStand
            | handValue playerHand == Combo = True
            | handValue playerHand == Charlie = True
            | handValue playerHand > Value 16 = True
            | handValue playerHand >= Value 12 && justCardRank dealerUpCard >= 4 && justCardRank dealerUpCard < 7 = True
            | handValue playerHand >= Value 13 && justCardRank dealerUpCard < 7 = True
            | otherwise = False

        -- Split Conditions
        doSplit
            | handValue playerHand == Value 8
                || handValue playerHand == Value 10
                || handValue playerHand == Value 20
                    = False
            | handValue playerHand == Value 18 && justCardRank dealerUpCard == 7
                || handValue playerHand == Value 18 && justCardRank dealerUpCard == 10
                || handValue playerHand == Value 18 && justCardRank dealerUpCard == 11
                    = False
            | checkAces playerHand = True
            | handValue playerHand == Value 14 && justCardRank dealerUpCard >= 8 = False
            | not(checkAces playerHand) && handValue playerHand == Value 12 && justCardRank dealerUpCard >= 7 = False
            | handValue playerHand <= Value 6 && justCardRank dealerUpCard >= 8 = False
            | otherwise = True

        -- Returns the correct action
        action
            | justCardRank dealerUpCard == 11 && parsedAction == "Bid" = Insurance (correctBid`div`2)
            | parsedAction == "DoubleDown" = Hit
            | parsedAction == "DoubleHit"  = Stand
            | isNothing dealerUpCard = Bid correctBid
            | checkCards playerHand && doSplit && parsedAction /= "Split" = Split correctBid
            | doDoubleDown = DoubleDown correctBid
            | doStand = Stand
            | otherwise = Hit

-- | This function is called once it's your turn, and keeps getting called until your turn ends.
playCard :: PlayFunc
playCard dealerUpCard playerPoints playerInfo playerId playerMemory playerHand = (p_action, memory)
    where
        -- Updating certain variables
        currentPoints = find' getPoints playerId getId playerPoints

        ----- AI Probability Odds Calculation -----
        -- Keeps track of the remaining decks
        decksRemaining
            | parsedRemaining <= 52 = 3
            | parsedRemaining <= 104 && parsedRemaining > 52 = 2
            | otherwise = 1

        -- Determining the odds of winning the dealer using information from the table
        countingOdds = (calcRunningCount(singleHand playerInfo) + calcRunningCount playerHand) `div` decksRemaining

        -- Adjusting the bid according to the odds of winning
        adjustedBid
            ---- Player favored odds -----
            | countingOdds >= 4 = maxBid
            | countingOdds >= 3 && countingOdds < 4 = maxBid*85`div`100
            | countingOdds >= 2 && countingOdds < 3 = maxBid*75`div`100
            | countingOdds >= 1 && countingOdds < 2 = maxBid*65`div`100
            ---- Dealer favored odds -----
            | countingOdds <= -1 && countingOdds > -2 = maxBid*40`div`100
            | countingOdds <= -2 && countingOdds > -3 = maxBid*30`div`100
            | countingOdds <= -3 && countingOdds > -4 = maxBid*15`div`100
            | countingOdds <= -4 = minBid
            | otherwise = maxBid `div` 2

        -- Keeping the bid stable
        correctBid
            | null playerHand = adjustedBid
            | otherwise = parsedBidAmount
        -- correctBid
        --     | currentPoints < 6000 = maxBid*65`div`100
        --     | currentPoints < 7000 = maxBid*70`div`100
        --     | currentPoints < 8000 = maxBid*80`div`100
        --     | currentPoints < 9000 = maxBid*90`div`100
        --     | otherwise = maxBid

        --- Choosing the correct action based off available information ---
        p_action
            | decksRemaining == 1 = singleDeckStrategy parsedAction currentPoints correctBid dealerUpCard playerHand
            | decksRemaining == 1 = doubleDeckStrategy parsedAction currentPoints correctBid dealerUpCard playerHand
            | otherwise = tripleDeckStrategy parsedAction currentPoints correctBid dealerUpCard playerHand

        -- parsing the neccessary data
        parsedAction = action $ parseString (removePunc(justString playerMemory))
        parsedRemaining = remainingCards $ parseString (removePunc(justString playerMemory))
        parsedRunningCount = runningCount $ parseString (removePunc(justString playerMemory))
        parsedBidAmount = bidAmount $ parseString (removePunc(justString playerMemory))
        parsedAces = aces $ parseString (removePunc(justString playerMemory))
        parsedTwos= twos $ parseString (removePunc(justString playerMemory))
        parsedThrees = threes $ parseString (removePunc(justString playerMemory))
        parsedFours= fours $ parseString (removePunc(justString playerMemory))
        parsedFives = fives $ parseString (removePunc(justString playerMemory))
        parsedSixes = sixes $ parseString (removePunc(justString playerMemory))
        parsedSevens = sevens $ parseString (removePunc(justString playerMemory))
        parsedEights = eights $ parseString (removePunc(justString playerMemory))
        parsedNines = nines $ parseString (removePunc(justString playerMemory))
        parsedTens = tens $ parseString (removePunc(justString playerMemory))
        parsedJacks = jacks $ parseString (removePunc(justString playerMemory))
        parsedQueens = queens $ parseString (removePunc(justString playerMemory))
        parsedKings = kings $ parseString (removePunc(justString playerMemory))

        -- updating data --
        updatedAction
            | parsedAction == "DoubleDown" = "DoubleHit"
            | otherwise = show p_action
        currentRC = parsedRunningCount
        updatedBit = correctBid
        -- Update all instances of on the table to reflect the played cards within a deck.
        updatedAces = updateStringCards parsedAces parsedAction dealerUpCard Ace playerId playerInfo playerHand
        updatedTwos = updateStringCards parsedTwos parsedAction dealerUpCard Two playerId playerInfo playerHand
        updatedThrees = updateStringCards parsedThrees parsedAction dealerUpCard Three playerId playerInfo playerHand
        updatedFours = updateStringCards parsedFours parsedAction dealerUpCard Four playerId playerInfo playerHand
        updatedFives = updateStringCards parsedFives parsedAction dealerUpCard Five playerId playerInfo playerHand
        updatedSixes = updateStringCards parsedSixes parsedAction dealerUpCard Six playerId playerInfo playerHand
        updatedSevens = updateStringCards parsedSevens parsedAction dealerUpCard Seven playerId playerInfo playerHand
        updatedEights = updateStringCards parsedEights parsedAction dealerUpCard Eight playerId playerInfo playerHand
        updatedNines = updateStringCards parsedNines parsedAction dealerUpCard Nine playerId playerInfo playerHand
        updatedTens = updateStringCards parsedTens parsedAction dealerUpCard Ten playerId playerInfo playerHand
        updatedJacks = updateStringCards parsedJacks parsedAction dealerUpCard Jack playerId playerInfo playerHand
        updatedQueens = updateStringCards parsedQueens parsedAction dealerUpCard Queen playerId playerInfo playerHand
        updatedKings = updateStringCards parsedKings parsedAction dealerUpCard King playerId playerInfo playerHand
        -- Updating the played cards as the turn ends.
        currentTrueCount = updatedAces + updatedTwos + updatedThrees + updatedFours +
                             updatedFives + updatedSixes + updatedSevens + updatedEights +
                                updatedNines + updatedTens + updatedJacks + updatedQueens + updatedKings
        -- Keeping the card count stable.
        updatedCards = currentTrueCount `mod` 156

        memory = convertCountingData $ createCountingData updatedAction updatedCards currentRC updatedBit
                 updatedAces updatedTwos updatedThrees updatedFours updatedFives updatedSixes updatedSevens
                 updatedEights updatedNines updatedTens updatedJacks updatedQueens updatedKings

-- Counts the total occurance of items within an list. Bool condition specifies the condition to be checked against.
countCards :: (a -> Bool) -> [a] -> Int
countCards p = foldl (\n x -> if p x then n+1 else n) 0

justCardRank :: Maybe Card -> Int
justCardRank (Just card) = cardToInt card
justCardRank Nothing = 0

updateStringCards :: Int -> String -> Maybe Card -> Rank -> PlayerId -> [PlayerInfo] -> Hand -> Int
updateStringCards update paction mc c pid pinfo currHand = total
    where
        total
            | isNothing mc = update + countCards(==c)(map getRank (singleExcludePlayerHand pid pinfo))
            | paction == "Bid" || paction == "Insurance" = update + countCards(==c)(map getRank (singleExcludeDealer pinfo ++ currHand))
            | length currHand >2 && getRank (head currHand) == c = update + 1
            | otherwise = update

cardToInt :: Card -> Int
cardToInt card
    | getRank card == Ace = 11
    | getRank card == Two = 2
    | getRank card == Three = 3
    | getRank card == Four = 4
    | getRank card == Five = 5
    | getRank card == Six = 6
    | getRank card == Seven = 7
    | getRank card == Eight = 8
    | getRank card == Nine = 9
    | getRank card == Ten = 10
    | getRank card == Jack = 10
    | getRank card == Queen = 10
    | getRank card == King = 10
    | otherwise = 0

-- Calculates the current odds to of all accumulative hands on the table.
calcRunningCount :: Hand -> Int
calcRunningCount cards = value
    where
        value = sum(map runningCountIntFixture cards)

-- Used to represent each rank of card to be weighted as a positive gain or negative lost
runningCountIntFixture:: Card -> Int
runningCountIntFixture (Card _ rank) | rank == Two||rank == Three||rank == Four||rank == Five ||rank == Six = 1
                                     | rank == Seven||rank == Eight||rank == Nine = 0
                                     | otherwise = -1

-- Extracts the Just and Nothing from a Maybe String
justString :: Maybe String -> String
justString (Just s) = s
justString Nothing = "Null"

-- Extended trace function that can do conditioning processing
traceIf :: Bool -> String -> a -> a
traceIf True  s x = trace s x
traceIf False _ x = x

-- Checks if the cards in the hand are the same. Only applicable for 2 card situations.
checkCards :: Hand -> Bool
checkCards [c1, c2] | getRank c1 == getRank c2 = True
checkCards _ = False

-- Checks if the cards in the hand are the same. Only applicable for 2 card situations.
checkAces :: Hand -> Bool
checkAces [c1, c2] | getRank c1 == Ace && getRank c2 == Ace = True
checkAces _ = False

-- Returns the hand from PlayerInfo data
getHand :: PlayerInfo -> Hand
getHand (PlayerInfo _ hand) = hand

-- Extracts all the hands from the players and combines them into a single array
extractHands :: [PlayerInfo] -> [Hand]
extractHands = map getHand

-- Combine all hand lists into a single list to be parsed
combineHand :: [Hand] -> Hand
combineHand = join

-- A higher order function that returns a single hand
singleHand::[PlayerInfo] -> Hand
singleHand p = hand
    where
        hand = combineHand(extractHands p)

-- Get dealer's hand
getDealerHand :: PlayerInfo -> Hand
getDealerHand (PlayerInfo i h) = hand
    where
        hand = if i == "dealer" then h
                else []

-- Converts dealer's hand
extractDealerHand::[PlayerInfo] -> [Hand]
extractDealerHand = map getDealerHand

-- Return's dealer's hand
singleDealerHand::[PlayerInfo] -> Hand
singleDealerHand p = hand
    where
        hand = combineHand(extractDealerHand p)

-- Exclude dealer's hand
getNotDealerHand :: PlayerInfo -> Hand
getNotDealerHand (PlayerInfo i h) = hand
    where
        hand = if i /= "dealer" then h
                else []

-- Converts dealer's hand
extractNotDealerHand::[PlayerInfo] -> [Hand]
extractNotDealerHand = map getNotDealerHand

-- Return's a hand of all other cards on the table excluding the previous dealer cards
singleExcludeDealer::[PlayerInfo] -> Hand
singleExcludeDealer p = hand
    where
        hand = combineHand(extractNotDealerHand p)


-- Exclude dealer's hand
getPlayerHand :: PlayerId -> PlayerInfo -> Hand
getPlayerHand a (PlayerInfo i h) = hand
    where
        hand
            |i == a = []
            |otherwise = h

extractExPlayerHand:: PlayerId -> [PlayerInfo] -> [Hand]
extractExPlayerHand a = map (getPlayerHand a)

-- Return's a hand of all other cards on the table excluding the previous dealer cards
singleExcludePlayerHand:: PlayerId -> [PlayerInfo] -> Hand
singleExcludePlayerHand a p = hand
    where
        hand = combineHand(extractExPlayerHand a p)

-- Manual card extraction
getAnyHand :: PlayerId -> PlayerInfo -> Hand
getAnyHand a (PlayerInfo i h) = hand
    where
        hand
            |i == "dealer" = []
            |i == a = []
            |otherwise = h

extractAnyHand:: PlayerId -> [PlayerInfo] -> [Hand]
extractAnyHand a = map (getAnyHand a)

-- Return's a hand of all other cards on the table excluding the previous dealer cards
singleAnyHand:: PlayerId -> [PlayerInfo] -> Hand
singleAnyHand a p = hand
    where
        hand = combineHand(extractAnyHand a p)

--------------------------------- Parser section -------------------------------------
data CountingData =
    CountingData {  action::String,
                    remainingCards,
                    runningCount,
                    bidAmount,
                    aces, twos, threes, fours, fives, sixes, sevens, eights, nines, tens, jacks, queens, kings::Int
                    }

createCountingData :: String -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> CountingData
createCountingData a rd rc ba ca c2 c3 c4 c5 c6 c7 c8 c9 c10 cj cq ck =
    CountingData {
        action=a, remainingCards=rd, runningCount=rc, bidAmount=ba,
        aces=ca, twos=c2, threes=c3, fours=c4, fives=c5, sixes=c6, sevens=c7, eights=c8, nines=c9, tens=c10, jacks=cj, queens=cq, kings=ck}

convertCountingData :: CountingData -> String
convertCountingData CountingData {action, remainingCards, runningCount, bidAmount, aces, twos, threes, fours, fives, sixes, sevens, eights, nines, tens, jacks, queens, kings} =
    show action ++ ":" ++ show remainingCards ++ ":" ++ show runningCount ++ ":" ++ show bidAmount
    ++ ":" ++ show aces ++ "|" ++ show twos ++ "|" ++ show threes ++ "|" ++ show fours
    ++ "|" ++ show fives ++ "|" ++ show sixes ++ "|" ++ show sevens ++ "|" ++ show eights
    ++ "|" ++ show nines ++ "|" ++ show tens ++ "|" ++ show jacks ++ "|" ++ show queens
    ++ "|" ++ show kings

instance Show CountingData where
    show CountingData { action, remainingCards, runningCount, bidAmount, aces, twos, threes, fours, fives, sixes, sevens, eights, nines, tens, jacks, queens, kings } =
        "("
            ++show action++","++ show remainingCards++ ","++ show runningCount++ ","++ show bidAmount++ ","++ show aces++ ","++ show twos++ ","++ show threes++ ","
            ++show fours++ ","++ show fives++ ","++ show sixes++ ","++ show sevens++ ","++ show eights++ ","++ show nines++ ","++ show tens++ ","++ show jacks
            ++","++ show queens++","++ show kings++ ")"

--- Additional Parsers (FIT2102 Week 11 Parser)
isRemove :: Char -> Parser Char
isRemove char = satisfy (== char)

string:: String -> Parser String
string = traverse is

spaces :: Parser ()
spaces = (is ' ' >> spaces) ||| pure ()

colon :: Parser ()
colon = (is ':' >> colon) ||| pure ()

line :: Parser ()
line = (is '|' >> colon) ||| pure ()

-- | Return a parser that produces a character but fails if:
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
  c <- character
  if f c then return c else unexpectedCharParser c

-- | Return a parser that produces any character but fails if:
isNot :: Char -> Parser Char
isNot char = satisfy (/= char)

-- Digits parsing
digit1 :: Parser String
digit1 = do
    d1 <- satisfy isDigit
    return [d1]

digit2 :: Parser String
digit2 = do
    d1 <- satisfy isDigit
    d2 <- satisfy isDigit
    return [d1,d2]

digit3 :: Parser String
digit3 = do
    d1 <- satisfy isDigit
    d2 <- satisfy isDigit
    d3 <- satisfy isDigit
    return [d1,d2,d3]

-- | Return a parser that produces a character between '0' and '9'
digits :: Parser String
digits = digit3 ||| digit2 ||| digit1

-- | Return a parser that continues producing a list of values from the given
-- parser.
list :: Parser a -> Parser [a]
list p = P $ \i -> case parse p i of
  Error _ -> Result i []
  Result rest x -> (x:) <$> parse (list p) rest

list1 :: Parser a -> Parser [a]
list1 x = do
  x' <- x
  x'' <- list x
  return (x' : x'')

-- | parseUntil
parseUntil :: Char -> Parser String
parseUntil c = do
    result <- list $ isNot c
    _ <- is c
    return result

getMemFromParser :: ParseResult a -> a
getMemFromParser (Result _ cs) = cs
getMemFromParser (Error _) = error "Error while parsing"

removePunc :: String -> String
removePunc xs = [ x | x <- xs, x `notElem` ".?!-;\"\'" ]

parseActionWithDigit :: Parser String
parseActionWithDigit = do
    s <- parseUntil ' '
    _ <- digits
    _ <- is ':'
    pure s

parseActionWithoutDigit :: Parser String
parseActionWithoutDigit =
    parseUntil ':'

parseCleanAction:: Parser String
parseCleanAction = parseActionWithDigit ||| parseActionWithoutDigit

countingData :: Parser CountingData
countingData = do
    act <- parseCleanAction
    remain <- digits
    _ <- is ':'
    count <- digits
    _ <- is ':'
    bid <- digits
    _ <- is ':'
    aces <- digits
    _ <- is '|'
    twos <- digits
    _ <- is '|'
    three <- digits
    _ <- is '|'
    four <- digits
    _ <- is '|'
    fives <- digits
    _ <- is '|'
    sixes <- digits
    _ <- is '|'
    sevens <- digits
    _ <- is '|'
    eight <- digits
    _ <- is '|'
    nine <- digits
    _ <- is '|'
    ten <- digits
    _ <- is '|'
    jacks <- digits
    _ <- is '|'
    queens <- digits
    _ <- is '|'
    kings <- digits
    return $ CountingData act (read remain::Int) (read count::Int) (read bid::Int)
            (read aces::Int) (read twos::Int) (read three::Int) (read four::Int)
            (read fives::Int) (read sixes::Int) (read sevens::Int) (read eight::Int)
            (read nine::Int) (read ten::Int) (read jacks::Int) (read queens::Int)
            (read kings::Int)

countingDataNull :: Parser CountingData
countingDataNull = string "" >> pure (CountingData "" 156 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)

parseCountingData :: Parser CountingData
parseCountingData = countingData ||| countingDataNull

parseString :: String -> CountingData
parseString s = getMemFromParser $ parse parseCountingData s
