module Data.IxTable.RangeQuery
  ( RangeQuery(..) ) where

data RangeQuery key = RangeQuery { key :: key
                                 , lt  :: Bool
                                 , eq  :: Bool
                                 , gt  :: Bool }
