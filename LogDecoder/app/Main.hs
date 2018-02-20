module Main where

import System.Environment (getArgs)
import Data.String
import Data.Word8
import Data.Char
import Data.Text (Text, pack)
import Data.Attoparsec.Text
import Control.Applicative
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import Control.Monad

oneNumber8 :: Parser Int
oneNumber8 = do
  char '\\'
  char 'M'
  char '-'
  result <- anyChar
  return $ (ord result) + 0x80

oneNumber4 :: Parser Int
oneNumber4 = do
  char '\\'
  char 'M'
  char '^'
  result <- anyChar
  return $ (ord result) + 0x40

oneNumber7 :: Parser Int
oneNumber7 = do
  char '\\'
  char '2'
  char '4'
  result <- anyChar
  return $ (ord result) + 0x70

oneNumber :: Parser Int
oneNumber = oneNumber8 <|> oneNumber4 <|> oneNumber7

oneChar :: Parser Char
oneChar = do
  num'16 <- oneNumber
  num'8  <- oneNumber
  num'1  <- oneNumber
  return $ convertToChar num'16 num'8 num'1

decodedChar :: Parser Char
decodedChar = oneChar <|> anyChar

decodedLine :: Parser String
decodedLine = manyTill decodedChar (endOfLine <|> endOfInput)

decodedLines :: Parser [String]
decodedLines = many decodedLine

convertToChar :: Int -> Int -> Int -> Char
convertToChar a b c = decodeUTF8 $ BS.pack [fromIntegral a, fromIntegral b, fromIntegral c]

decodeUTF8 :: UTF8.ByteString -> Char
decodeUTF8 bs = case (UTF8.decode bs) of
  Just (c, _) -> c
  otherwise -> '×'

main :: IO ()
main = do
  args <- getArgs
  case args of
    [x] -> do
      input <- readFile x
      forM_ (lines input) $ \line -> do
        let result = case (parse decodedLine (pack (line ++ "\n"))) of
                     Done _ result -> result
                     _             -> "×"
        putStrLn $ result
    otherwise -> usage

usage :: IO ()
usage = do
  putStrLn "usage: "
  putStrLn "  ./decodeLog <input log file path> > <redirect file path>"
  putStrLn "example: "
  putStrLn "  ./decodeLog ./log.txt > log_decoded.txt"
