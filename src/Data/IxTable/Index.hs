{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.IxTable.Index
  ( Index
  , ixFun, ixField
  , insert, delete, update
  , restrictPkeys
  , member
  , lookup
  , keysSet
  ) where

import           Prelude         hiding (lookup)
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import qualified Data.Set        as S
import           Data.Set        (Set)

import           Data.Bool       (bool)
import           Data.Foldable   (foldl')
import           Data.Maybe      (fromMaybe)

import           Data.IxTable.RangeQuery

data Index pkey key elt = Index { extractKeys :: !(elt -> [key])
                                , index       :: !(Map key (Set pkey)) }

ixFun :: (elt -> [key]) -> Index pkey key elt
ixFun extractKeys = Index { extractKeys
                          , index = M.empty }

ixField :: (elt -> key) -> Index pkey key elt
ixField extractField = Index { extractKeys = singleton . extractField
                             , index = M.empty }
  where
    singleton x = [x]

insert :: (Ord pkey, Ord key) => pkey -> elt -> Index pkey key elt -> Index pkey key elt
insert pkey elt idx@Index{ extractKeys, index } = idx { index = index' }
  where
    index' = foldl' insertIntoMap index $ extractKeys elt
    insertIntoMap m key = M.alter insertIntoSet key m
    insertIntoSet = \case
      Nothing  -> Just $ S.singleton pkey
      Just set -> Just $ S.insert pkey set

delete :: (Ord pkey, Ord key) => pkey -> elt -> Index pkey key elt -> Index pkey key elt
delete pkey elt idx@Index{ extractKeys, index } = idx { index = index' }
  where
    index' = foldl' deleteFromMap index $ extractKeys elt
    deleteFromMap m key = M.update deleteFromSet key m
    deleteFromSet set =
      case S.null set' of
        False -> Just set'
        True  -> Nothing
      where
        set' = S.delete pkey set

update :: (Ord pkey, Ord key)
       => pkey
       -> elt
       -> elt
       -> Index pkey key elt
       -> Index pkey key elt
update pkey old new = insert pkey new . delete pkey old

restrictPkeys :: Ord pkey
              => Set pkey
              -> Index pkey key elt
              -> Index pkey key elt
restrictPkeys pkeys idx@Index{ index } = idx { index = index' }
  where
    index' = M.mapMaybe subtractPkeys index
    subtractPkeys set =
      case S.null set' of
        False -> Just set'
        True  -> Nothing
      where
        set' = S.intersection set pkeys

member :: (Ord key) => key -> Index pkey key elt -> Bool
member key Index{ index } = M.member key index

lookup :: (Ord pkey, Ord key) => RangeQuery key -> Index pkey key elt -> Set pkey
lookup RangeQuery{ key, lt, eq, gt } Index{ index } =
  S.unions [ ltSet `maskedBy` lt, eqSet `maskedBy` eq, gtSet `maskedBy` gt ]
  where
    set `maskedBy` flag = bool S.empty set flag
    ltSet = S.unions $ M.elems ltPart
    eqSet = fromMaybe S.empty eqPart
    gtSet = S.unions $ M.elems gtPart
    (ltPart, eqPart, gtPart) = M.splitLookup key index

keysSet :: Index pkey key elt -> Set key
keysSet Index{ index } = M.keysSet index
