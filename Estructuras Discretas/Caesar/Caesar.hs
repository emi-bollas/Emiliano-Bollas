-- Caesar Encryption (English, 26 letters)--
              -- E. Bollás --
import Data.Char (toUpper,chr,ord)
--Aux function that removes non alphabetical characters from the String
clean :: String -> String
clean [] = []
clean xs = [toUpper x | x <- xs, toUpper x `elem` ['A'..'Z']]
--True for encrypting, false for decrypting
caesar :: String -> Int -> Bool -> String 
caesar [] n b = []
caesar xs n b = if b == True then [chr(((ord x - ord 'A' + n) `mod` 26) + ord 'A')| x <- clean xs ]
                                  else [chr(((ord x - ord 'A' - n) `mod` 26) + ord 'A')| x <- clean xs ]
--------------
-- Caesar Encryption (Spanish, 27 letters)--
              -- E. Bollás --
                                       
-- Dictionary with the added Ñ                                       
dict :: String
dict = "ABCDEFGHIJKLMNÑOPQRSTUVWXYZ"
--Adapted clean function
putzen :: String -> String
putzen [] = []
putzen xs = [toUpper x | x <- xs, toUpper x `elem` dict || x `elem` ['a'..'z'] ++ "ñ"]
-- Aux function that returns the position of a character
pos :: String -> Char -> Int
pos [] a = -1
pos (x:xs) a = if a /= x then 1+ pos xs a
                              else 1 + pos[] a
-- Aux function that returns a character given its position
chh :: Int -> Char
chh n = dict !! n
-- Same as the last one, only that there are 27 letters now. True for encrypting, False for decrypting
shiza :: String -> Int -> Bool -> String
shiza [] n b = []
shiza xs n b = if b == True then [chh((pos dict x + n) `mod` 27)| x<- putzen xs]
                                 else [chh((pos dict x - n) `mod` 27)| x<- putzen xs]


