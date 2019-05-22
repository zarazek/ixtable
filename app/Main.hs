{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Main where

import qualified Data.Set as S
import           Data.Set (Set)
import           Data.Foldable (toList)

import           Control.Monad.Except

import qualified Data.IxTable.Table   as Tbl
import           Data.IxTable.Table   (Table)
import qualified Data.IxTable.Indices as Idc
import qualified Data.IxTable.Index   as Idx

data Example = Example { primKey       :: Word
                       , maybeInt      :: Maybe Int
                       , listOfStrings :: [String]
                       , double        :: Double }
  deriving (Eq, Show)

type ExampleKeys = '[Int, String, Double]

type ExampleTable = Table Word ExampleKeys Example

instance Tbl.PrimaryKey Word Example where
  extractPkey = primKey

instance Tbl.Indexable Word ExampleKeys Example where
  emptyIndices = Idc.mkIndices (Idx.ixFun (toList . maybeInt))
                               (Idx.ixFun listOfStrings)
                               (Idx.ixFun (singleton . double))

singleton :: a -> [a]
singleton x = [x]

example1 :: Example
example1 = Example{ primKey       = 1
                  , maybeInt      = Nothing
                  , listOfStrings = ["aa", "bb", "cc"]
                  , double        = 13.0 }

example2 :: Example
example2 = Example{ primKey       = 2
                  , maybeInt      = Just 7
                  , listOfStrings = ["aa"]
                  , double        = 13.1 }

example3 :: Example
example3 = Example{ primKey       = 3
                  , maybeInt      = Nothing
                  , listOfStrings = ["bb", "dd"]
                  , double        = 12.9 }

example4 :: Example
example4 = Example{ primKey       = 4
                  , maybeInt      = Just 5
                  , listOfStrings = ["aa", "cc"]
                  , double        = 12.8 }

examples :: [Example]
examples = [ example1, example2, example3, example4 ]

runTests :: ExceptT String IO () -> IO ()
runTests act = do
  res <- runExceptT act
  case res of
    Left msg -> putStrLn msg
    Right _  -> pure ()

printLn :: String -> ExceptT String IO ()
printLn = lift . putStrLn

assertEq :: (Eq a, Show a) => String -> a -> a -> ExceptT String IO ()
assertEq msg actual expected = do
  printLn msg
  case actual == expected of
    True  -> printLn $ "\tOK, " <> show actual
    False -> throwError errorMsg
      where
        errorMsg = "\tFAILURE: expected: " <> show expected <> ", actual: " <> show actual

fromRight :: Either a b -> b
fromRight = \case
  Left _  -> error "Left!"
  Right x -> x

main :: IO ()
main = runTests $ do
  printLn "Constructing from list"
  t :: ExampleTable <- case Tbl.fromList examples of
    Right tbl -> pure tbl
    Left pkey -> throwError $ "duplicate pkey " ++ show pkey

  printLn "Filtering by pkey (Word)"
  let table1 = Tbl.getEQ (3 :: Word) t
  assertEq "table" table1 (fromRight $ Tbl.fromList [example3])
  assertEq "pkey set" (Tbl.keysSet table1) (S.fromList [3 :: Word])
  assertEq "Int key set" (Tbl.keysSet table1) (S.empty :: Set Int)
  assertEq "String key set" (Tbl.keysSet table1) (S.fromList ["bb", "dd"])
  assertEq "Double key set" (Tbl.keysSet table1) (S.fromList [12.9 :: Double])

  printLn "Filtering by Int"
  let table2 = Tbl.getEQ (1 :: Int) t
  assertEq "table" table2 Tbl.empty
  assertEq "pkey set" (Tbl.keysSet table2) (S.empty :: Set Word)
  assertEq "Int key set" (Tbl.keysSet table2) (S.empty :: Set Int)
  assertEq "String key set" (Tbl.keysSet table2) (S.empty :: Set String)
  assertEq "Double key set" (Tbl.keysSet table2) (S.empty :: Set Double)

  printLn "Filtering by String"
  let table3 = Tbl.getEQ "aa" t
  assertEq "table" table3 (fromRight $ Tbl.fromList [example1, example2, example4])
  assertEq "pkey set" (Tbl.keysSet table3) (S.fromList [1 :: Word, 2, 4])
  assertEq "Int key set" (Tbl.keysSet table3) (S.fromList [5 :: Int, 7])
  assertEq "String key set" (Tbl.keysSet table3) (S.fromList ["aa", "bb", "cc"])
  assertEq "Double key set" (Tbl.keysSet table3) (S.fromList [12.8 :: Double, 13.0, 13.1])

  printLn "Filtering by Double"
  let table4 = Tbl.getLTE (13.0 :: Double) t
  assertEq "table" table4 (fromRight $ Tbl.fromList [example1, example3, example4])
  assertEq "pkey set" (Tbl.keysSet table4) (S.fromList [1 :: Word, 3, 4])
  assertEq "Int key set" (Tbl.keysSet table4) (S.fromList [5 :: Int])
  assertEq "String key set" (Tbl.keysSet table4) (S.fromList ["aa", "bb", "cc", "dd"])
  assertEq "Double key set" (Tbl.keysSet table4) (S.fromList [12.8 :: Double, 12.9, 13.0])
