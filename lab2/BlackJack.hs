module BlackJack where
import           Cards
import           RunGame
import           System.Random
import           Test.QuickCheck

implementation = Interface {
  iFullDeck = fullDeck,
  iValue = value,
  iDisplay = display,
  iGameOver = gameOver,
  iWinner = winner,
  iDraw = draw,
  iPlayBank = playBank,
  iShuffle = shuffleDeck
}

main :: IO ()
-- main = do
--   quickCheck prop_size_shuffle
--   quickCheck prop_shuffle_sameCards
--   quickCheck prop_onTopOf_assoc
--   quickCheck prop_size_onTopOf
main = runGame implementation

-- A0 ------------------------
{-
size hand2
= size (Add (Card (Numeric 2) Hearts)
(Add (Card Jack Spades) Empty))
= ...
= 2
-}
hand2 = Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty)
sizeSteps :: [Integer]
sizeSteps = [ size hand2, size (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty)), size (Add (Card Jack Spades) Empty) + 1, 1 + 1, 2 ]
-- A1 ------------------------
-- | Converts a rank to a short diplay representation
displayRank :: Rank -> String
displayRank (Numeric n) = show n
displayRank Ace         = "A"
displayRank King        = "K"
displayRank Queen       = "Q"
displayRank Jack        = "J"
-- | Converts the suit of a card to a short unicode representation
displaySuit :: Suit -> String
displaySuit Hearts   = "\9829"
displaySuit Spades   = "\9824"
displaySuit Diamonds = "\9830"
displaySuit Clubs    = "\9827"
-- | Display a card in short representation
displayCard :: Card -> String
displayCard (Card rank suit) = "[" ++ displayRank rank ++ displaySuit suit ++ "]"
-- | Display of a hand of cards.
display :: Hand -> String
display Empty           = ""
display (Add card hand) = displayCard card ++ " " ++ display hand

-- A2 ------------------------
-- | Converts a rank to a value
valueRank :: Rank -> Integer
valueRank (Numeric n) = n
valueRank Ace         = 11
valueRank _           = 10
-- | Calculates the Number of Aces in a hand
numberOfAces :: Hand -> Integer
numberOfAces Empty                   = 0
numberOfAces (Add (Card Ace _) hand) = 1 + numberOfAces hand
numberOfAces (Add _ hand)            = numberOfAces hand
-- | Calculates the value of a hand not taking into account the number of Aces
initialValue :: Hand -> Integer
initialValue Empty                    = 0
initialValue (Add (Card rank _) hand) = valueRank rank + initialValue hand
-- | Lower the value of aces in the hand until the value is less than 21
lowerAces :: Hand -> Integer -> Integer
lowerAces hand value
  | value > 21 && numberOfAces hand > 0 = lowerAces hand (value - 10)
  | otherwise = value
-- | Takes a hand and returns the value of the hand
value :: Hand -> Integer
value hand = lowerAces hand (initialValue hand)

-- A3 ------------------------
-- | Check if the hand is a bust
gameOver :: Hand -> Bool
gameOver hand = value hand > 21
-- | Check if the hand is a winner

-- A4 ------------------------
-- | Given two hands, returns the winner
winner :: Hand -> Hand -> Player
winner guest bank
  | gameOver guest = Bank
  | gameOver bank = Guest
  | value guest > value bank = Guest
  | value guest < value bank = Bank
  | otherwise = Bank

-- B1 ------------------------
-- | Given two hands, <+ puts the first one on top of the second one
(<+) :: Hand -> Hand -> Hand
(<+) Empty hand            = hand
(<+) (Add card hand) hand2 = Add card (hand <+ hand2)
-- | <+ must satisfy the following laws using QuickCheck
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
  p1<+(p2<+p3) == (p1<+p2)<+p3
-- | <+ must satisfy the following laws using QuickCheck
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf p1 p2 =
  size p1 + size p2 == size (p1<+p2)

-- B2 ------------------------
-- | Returns a fullSuit of cards
fullSuit :: Suit -> Hand
fullSuit suit = foldr Add Empty [Card rank suit | rank <- [Numeric n | n <- [2..10]] ++ [Jack, Queen, King, Ace]]

-- | Returns FullDeck of cards
fullDeck :: Hand
fullDeck = foldr (<+) Empty [fullSuit suit | suit <- [Hearts, Spades, Diamonds, Clubs]]

-- B3 ------------------------
{-
Given a deck and a hand, draw one card from the deck
and put on the hand. Return both the deck and the hand
(in that order).
-}
removeCard :: Hand -> (Card, Hand)
removeCard Empty           = error "empty deck"
removeCard (Add card hand) = (card, hand)

draw :: Hand -> Hand -> (Hand, Hand)
draw Empty hand = error "draw: The deck is empty."
draw deck hand = (deck', hand')
  where
    (card, deck') = removeCard deck
    hand' = Add card hand

-- B4 ------------------------
playBankHelper :: Hand -> Hand -> Hand
playBankHelper deck hand
  | value hand >= 16 = hand
  | otherwise = playBankHelper deck' hand'
  where
    (deck', hand') = draw deck hand
-- | Given a deck, play for the bank (starting with an empty hand), and return the bank’s final hand:
playBank :: Hand -> Hand
playBank deck = playBankHelper deck Empty
-- B5 ------------------------
-- c `belongsTo` Empty = False
-- c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h
belongsTo :: Card -> Hand -> Bool
belongsTo _ Empty = False
belongsTo card (Add card' hand)
  | card == card' = True
  | otherwise = card `belongsTo` hand
removeCardAt :: Hand -> Int -> (Card, Hand)
removeCardAt Empty _ = error "empty deck"
removeCardAt (Add card hand) 0 = (card, hand)
removeCardAt (Add card hand) n = (card', Add card hand')
  where
    (card', hand') = removeCardAt hand (n-1)
-- | Given a StdGen and a hand of cards, shuffle the cards and return the shuffled hand
shuffleDeck :: StdGen -> Hand -> Hand
shuffleDeck _ Empty = Empty
shuffleDeck gen deck = do
  -- take a random card from the deck and add it to the shuffled deck
  -- repeat until the deck is empty
  let (n, gen') = randomR (0, size deck - 1) gen
  let (card, deck') = removeCardAt deck n
  Add card (shuffleDeck gen' deck')

prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
  c `belongsTo` h == c `belongsTo` shuffleDeck g h
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h = size h == size (shuffleDeck g h)
