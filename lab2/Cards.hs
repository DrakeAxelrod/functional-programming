-- | Data types for card games
module Cards where

import Test.QuickCheck
import System.Random

-- | A card has a rank and belongs to a suit.
data Card = Card Rank Suit
      deriving (Eq, Show)

-- | rank and suit give the respective parts of a card
rank :: Card -> Rank
rank (Card r _) = r

suit :: Card -> Suit
suit (Card _ s) = s


-- | A rank is either a numeric card, a face card, or an ace. The
-- numeric cards range from two to ten.
data Rank = Numeric Integer | Jack | Queen | King | Ace
            deriving (Eq, Show)

-- | All the different suits.
data Suit = Hearts | Spades | Diamonds | Clubs
            deriving (Eq, Show)

-- | A hand of cards. This data type can also be used to represent a
-- deck of cards.
data Hand = Empty | Add Card Hand
            deriving (Eq, Show)
            
-- | The size of a hand.
size :: Num a => Hand -> a
size Empty            = 0
size (Add card hand)  = 1 + size hand

--------------------------------------------------------------------
-- Functions below are to tell QuickCheck how to generate random cards
-- We will see how to do this in week 4.
           
-- | Generate a random Card
instance Arbitrary Card where
  arbitrary = Card <$> arbitrary <*> arbitrary

-- | Random generator for Suit
instance Arbitrary Suit where
  arbitrary = elements [Hearts, Spades, Diamonds, Clubs]

-- | Random generator for Rank
instance Arbitrary Rank where
  arbitrary = frequency [ (4, elements [Jack,Queen,King,Ace])
                        , (9, Numeric <$> choose (2, 10))
                        ]

-- | Random generator for Hand
instance Arbitrary Hand where
  arbitrary = frequency [  (1,  return Empty)
                        ,  (7, Add <$> arbitrary <*> arbitrary)
                        ]
  shrink Empty = []
  shrink (Add c h) = Empty : h : [Add c h' | h'<-shrink h]


-- We also need to be able to generate random number generators. (This
-- does not really belong in this file, but is placed here to reduce
-- the number of files needed.)
instance Arbitrary StdGen where
  arbitrary = mkStdGen <$> arbitrary
