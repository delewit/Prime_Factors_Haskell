{- The "makeChangeSolver" that I wrote last night can be adapted (I think?) to the problem of finding the
   prime factors of any integer.  This is cool shit!  Since I didn't have much discrete math when I was a
   math major, the algorithms of computer science are sometimes a little challenging and mysterious to me,
   but I think I'm beginning to "see the Light"!  Of course... this is probably just the tip of the
   proverbial iceberg. -}

module Prime_Factors ( findPrimeFactors ) where 

import qualified Data.List (sort)

makeChange :: [Int] -> [Int] -> Int -> [Int] -> [Int]
makeChange coins infList amount values =
  let h = head infList
      t = tail infList 
  in if amount <= 1 then coins
     else if mod amount (values!!h) /= 0 then makeChange coins t amount values
          else makeChange (values!!h : coins) infList (div amount $ values!!h) values


makeChangeSolver :: Int -> [Int] -> [Int]
makeChangeSolver amount values =
  let len     = length values - 1 
      infList = myCycleFunction [0 .. len]
      values' = (reverse . Data.List.sort) values 
  in makeChange [] infList amount values'


myCycleFunction :: [a] -> [a]
myCycleFunction xs = xs ++ myCycleFunction xs


{- Not a very efficient function for testing to see if an Int is
   prime or not, but... I'm in a hurry this morning!!! -}
isPrime' :: Int -> Int -> Bool
isPrime' start finish | start == finish = True
isPrime' start finish | mod finish start == 0 = False
isPrime' start finish = isPrime' (1+start) finish

isPrime :: Int -> Bool
isPrime num  = if num < 2 then False else isPrime' 2 num 


generatePrimes :: Int -> [Int]
generatePrimes n =
  if isPrime n then n : generatePrimes (1+n) else generatePrimes (1+n)


findPrimeFactors :: Int -> [Int]
findPrimeFactors y | y < 2 = error "Bad argument! Please try again."
findPrimeFactors y =
  if isPrime y then [1, y]
  else makeChangeSolver y factors
  where factors = takeWhile (\x -> x < y) $ generatePrimes 1 



