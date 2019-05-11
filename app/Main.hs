{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Main where

import qualified Data.Set as S
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

printTbl :: (Show pkey) => Table pkey keys elt -> ExceptT String IO ()
printTbl = printLn . show . Tbl.pkeysSet

main :: IO ()
main = runTests $ do
  printLn "Constructing from list"
  t :: ExampleTable <- case Tbl.fromList examples of
    Right tbl -> pure tbl
    Left pkey -> throwError $ "duplicate pkey " ++ show pkey
  printLn "Looking up by pkey (Word)"
  let s1 = Tbl.pkeysSet $ Tbl.getEQ (3 :: Word) t
  case s1 == S.fromList [3] of
    False -> throwError $ show s1
    True  -> printLn $ show s1
  printLn "Looking up by Int"
  let s2 = Tbl.pkeysSet $ Tbl.getEQ (1 :: Int) t
  case s2 == S.empty of
    False -> throwError $ show s2
    True  -> printLn $ show s2
  printLn "Looking up by String"
  let s3 = Tbl.pkeysSet $ Tbl.getEQ "aa" t
  case s3 == S.fromList [1, 2, 4] of
    False -> throwError $ show s3
    True  -> printLn $ show s3
  printLn "Looking up by Double"
  let s4 = Tbl.pkeysSet $ Tbl.getLTE (13.0 :: Double) t
  case s4 == S.fromList [1, 3, 4] of
    False -> throwError $ show s4
    True  -> printLn $ show s4
