module RunGame where

import Data.Char
import System.Random
import Cards

-- | The interface to the students' implementation.
data Interface = Interface
  { iFullDeck :: Hand
  , iValue    :: Hand -> Integer
  , iDisplay  :: Hand -> String
  , iGameOver :: Hand -> Bool
  , iWinner   :: Hand -> Hand -> Player
  , iDraw     :: Hand -> Hand -> (Hand, Hand)
  , iPlayBank :: Hand -> Hand
  , iShuffle  :: StdGen -> Hand -> Hand
  }

-- | A type of players.
data Player = Guest | Bank
              deriving (Show, Eq)

-- | Runs a game given an implementation of the interface.
runGame :: Interface -> IO ()
runGame i =
  do putStrLn "Welcome to the game."
     g <- newStdGen
     gameLoop i (iShuffle i g (iFullDeck i)) Empty

-- | Play until the guest player is bust or chooses to stop.
gameLoop :: Interface -> Hand -> Hand -> IO ()
gameLoop i deck guest = do 
     putStrLn $ "Your current score: " ++ displayHand i guest ++ "\n"
     if iGameOver i guest
       then finish i deck guest
       else do putStr ("Draw "
                       ++ (if guest == Empty then "a " else "another ")
                       ++ "card? [y] ")
               yn <- getLine
               if null yn || not (map toLower yn == "n")
                 then do let (deck', guest') = iDraw i deck guest
                         gameLoop i deck' guest'
                 else finish i deck guest


-- | Display the bank's final score and the winner.
finish :: Interface -> Hand -> Hand -> IO ()
finish i deck guest = do
  putStrLn $ "Your final score: " ++ displayHand i guest
  putStrLn $ "The bank's final score: " ++ displayHand i bank
  putStrLn $ "Winner: " ++ show (iWinner i guest bank)
   where
     bank = iPlayBank i deck

displayHand :: Interface -> Hand -> String
displayHand i hand = show (iValue i hand)
                  ++ if hand == Empty then ""
                     else " with cards: " ++ iDisplay i hand