{- Lab 1
   Date: November 3, 2022
   Authors: Drake Axelrod, Vernita Gouws
   Lab group: vrochophobia
 -}
import Test.QuickCheck
-- MAIN ----------------------
main :: IO ()
main = 
  quickCheck prop_powers'
--------------------------------------------
power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)

-- A ------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute
stepsPower :: Integer -> Integer -> Integer
stepsPower n k
   | k < 0 = error "stepsPower: negative argument"
-- not recursive
stepsPower n k = k + 1

-- B -------------------------
power1 :: Integer -> Integer -> Integer
power1 n k
   | k < 0 = error "stepsPower: negative argument"
   -- use replicate and fromInteger to create a list
power1 n k = product (replicate (fromInteger k) n)

-- C -------------------------
power2 :: Integer -> Integer -> Integer
-- base case
power2 n 0 = 1
-- check if even or odd
power2 n k
   | even k = power2 (n*n) (k `div` 2)
   | odd k = n * power2 (n*n) ((k-1) `div` 2)

-- D -------------------------
{-
<Describe your test cases here>
check 0 for both params at same time becuase 0^0 should be 1
case 1: 0, 0 -> 1
check second param as 0 because we want to see if the fall back case works
case 2: 1, 0 -> 1
check all equation get the same result
case 3: 1, 1 -> 1
case 4: 2, 3 -> 8
check first param as negative number and second param as 0 because the result should always be -1
case 5: -1, 0 -> -1
case 6: -2, 0 -> -1
check whether stop condition is reached
case 7: 0, -1 -> 0
case 8: 1, -1 -> 0
case 9: -1, -1 -> 0
check very large numbers to check the efficiency of the algorithm
case 10: 2, 100000 -> 100000
-}

-- 
prop_powers :: Integer -> Integer -> Bool
prop_powers n k = power n k == power1 n k && power n k == power2 n k

--
powerTest :: Bool
-- check all well-behaved cases
powerTest = prop_powers 0 0 && prop_powers 1 0 && prop_powers 1 1 && prop_powers 2 3 && prop_powers 2 100000

--
prop_powers' :: Integer -> Integer -> Property
-- handle all test cases
prop_powers' n k = (n >= 0 && k >= 0) ==> prop_powers n k
