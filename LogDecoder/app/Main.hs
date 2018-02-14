module Main where

import Data.Text
import qualified Text.Regex.Posix as Regex
import Data.Attoparsec.Text
import Data.Char
import Control.Applicative
import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)
import qualified Codec.Binary.UTF8.String as C
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import Data.String
import qualified Data.Text.Encoding as Encoding
import Data.Word8

oneNumber9 :: Parser Int
oneNumber9 = do
  char '\\'
  char 'M'
  char '-'
  result <- anyChar
  return $ (ord result) + 9

oneNumber4 :: Parser Int
oneNumber4 = do
  char '\\'
  char 'M'
  char '^'
  result <- anyChar
  return $ (ord result) + 4

oneNumber :: Parser Int
oneNumber = oneNumber9 <|> oneNumber4

oneChar :: Parser String
oneChar = do
  num'16 <- oneNumber
  num'8  <- oneNumber
  num'1  <- oneNumber
  return $ convertToBS num'16 num'8 num'1


changeToBS :: String -> BS.ByteString
changeToBS s = fromString s

changeToBS' :: String -> UTF8.ByteString
changeToBS' s = fromString s

convertToBS :: Int -> Int -> Int -> String
convertToBS a b c = C.decode [fromIntegral a, fromIntegral b, fromIntegral c]

main :: IO ()
main = do
  let input = "\\M-c\\M^A\\M->"
  putStrLn$  "input = " ++ input
  putStrLn $ show $ testFunc input
  let result = input Regex.=~ "\^A" :: (String, String, String, [String])
  putStrLn $ show $ result
  putStrLn "!!!"
  print $ parse oneNumber (pack "\\M-a\\M^A")
  print $ parse oneChar (pack input)
  putStrLn $ convertToBS 227 129 130
  print $ C.encodeChar 'あ'
  print $ C.decode $ C.encodeChar 'あ'

testFunc :: String -> Bool
testFunc s = s Regex.=~ "\\M-c" :: Bool
