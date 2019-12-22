import Prime_Factors
import System.IO
import Data.Char 


main = do
  hSetBuffering stdout NoBuffering
  putStrLn ""
  num <- (putStr "Enter an integer greater than or equal to 2: ")
             >> (\s -> read s :: Int) <$> getLine 
  let factorsList = findPrimeFactors num
  putStrLn ""
  let initFactorsList = map show $ init factorsList
  let lastFactorsList = show $ last factorsList
  mapM_ (\x -> putStr $ x ++ ", ") initFactorsList
  putStrLn lastFactorsList
  putStrLn ""
  repeat <-
    (putStr "Would you like to enter another integer to see its factors? ") >> getLine 
  let yesOrNo = map (toLower) repeat
  if yesOrNo == "yes" then main else putStrLn "" 



