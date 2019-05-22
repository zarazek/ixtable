{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
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

examples :: [Example]
examples = [ Example{ primKey       = 1
                    , maybeInt      = Nothing
                    , listOfStrings = ["aa", "bb", "cc"]
                    , double        = 13.0 }
           , Example{ primKey       = 2
                    , maybeInt      = Just 7
                    , listOfStrings = ["aa"]
                    , double        = 13.1 }
           , Example{ primKey       = 3
                    , maybeInt      = Nothing
                    , listOfStrings = ["bb", "dd"]
                    , double        = 12.9 }
           , Example{ primKey       = 4
                    , maybeInt      = Just 5
                    , listOfStrings = ["aa", "cc"]
                    , double        = 12.8 }
           ]

runTests :: ExceptT String IO () -> IO ()
runTests act = do
  res <- runExceptT act
  case res of
    Left msg -> putStrLn $ '\t' : msg
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

main :: IO ()
main = runTests $ do
  printLn "Constructing from list"
  t :: ExampleTable <- case Tbl.fromList examples of
    Right tbl -> pure tbl
    Left pkey -> throwError $ "duplicate pkey " ++ show pkey

  printLn "Filtering by pkey (Word)"
  let table1 = Tbl.getEQ (3 :: Word) t
  assertEq "pkey set" (Tbl.keysSet table1) (S.fromList [3 :: Word])
  assertEq "Int key set" (Tbl.keysSet table1) (S.empty :: Set Int)
  assertEq "String key set" (Tbl.keysSet table1) (S.fromList ["bb", "dd"])
  assertEq "Double key set" (Tbl.keysSet table1) (S.fromList [12.9 :: Double])

  printLn "Filtering by Int"
  let table2 = Tbl.getEQ (1 :: Int) t
  assertEq "pkey set" (Tbl.keysSet table2) (S.empty :: Set Word)
  assertEq "Int key set" (Tbl.keysSet table2) (S.empty :: Set Int)
  assertEq "String key set" (Tbl.keysSet table2) (S.empty :: Set String)
  assertEq "Double key set" (Tbl.keysSet table2) (S.empty :: Set Double)

  printLn "Filtering by String"
  let table3 = Tbl.getEQ "aa" t
  assertEq "pkey set" (Tbl.keysSet table3) (S.fromList [1 :: Word, 2, 4])
  assertEq "Int key set" (Tbl.keysSet table3) (S.fromList [5 :: Int, 7])
  assertEq "String key set" (Tbl.keysSet table3) (S.fromList ["aa", "bb", "cc"])
  assertEq "Double key set" (Tbl.keysSet table3) (S.fromList [12.8 :: Double, 13.0, 13.1])

  printLn "Filtering by Double"
  let table4 = Tbl.getEQ (13.0 :: Double) t
  assertEq "pkey set" (Tbl.keysSet table4) (S.fromList [1 :: Word, 3, 4])
  assertEq "Int key set" (Tbl.keysSet table4) (S.fromList [5 :: Int])
  assertEq "String key set" (Tbl.keysSet table4) (S.fromList ["aa", "bb", "cc", "dd"])
  assertEq "Double key set" (Tbl.keysSet table4) (S.fromList [12.8 :: Double, 12.9, 13.0])
