import Data.Char (toLower)
import Data.Set (fromList, isSubsetOf)

leap :: Integer -> Bool
leap n
	  | n `mod` 400 == 0 =  True
	  | n `mod` 4 == 0 && n `mod` 100 /= 0 = True
	  | otherwise		= False

pangram :: String -> Bool
pangram text = isSubsetOf alphabet (lowerSet text)
 where
  lowerSet = fromList . toLower'
  toLower' = map toLower
  alphabet = fromList "abcdefghijklmnopqrstuvwxyz"