{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.IxTable.Table
  ( Table
  , Indexable(..), PrimaryKey(..)
  , empty
  , insert, delete, update, upsert
  , lookupByPkey
  , Lookup, member, lookup, keysSet
  , getLT, getLTE, getEQ, getGTE, getGT
  , null, size
  , fromList
  , toMap
  , TableDiff(..), emptyDiff, tableDiff
  ) where
import           Prelude                hiding (lookup, null)

import qualified Data.Map.Strict        as M
import           Data.Map.Strict        (Map)
import           Data.Set               (Set)
import           Data.These             (These(..))

import           Data.Bool              (bool)
import           Data.Foldable          (foldl', foldlM)

import qualified Data.IxTable.Indices   as Idc
import           Data.IxTable.Indices   (Indices)
import           Data.IxTable.RangeQuery
import           Data.IxTable.TypeLevel

data Table (pkey :: *) (keys :: [*]) (elt :: *) =
  Table { elts    :: !(Map pkey elt)
        , indices :: !(Indices pkey keys elt) }

instance (Eq elt) => Eq (Table pkey keys elt) where
  Table{ elts = elts1 } == Table{ elts = elts2 } = M.elems elts1 == M.elems elts2

instance (Show elt) => Show (Table pkey keys elt) where
  show Table{ elts } = "fromList " ++ show (M.elems elts)

instance Foldable (Table pkey keys) where
  foldr f x Table{ elts } = foldr f x elts

class PrimaryKey pkey elt where
  extractPkey  :: elt -> pkey

class PrimaryKey pkey elt => Indexable pkey keys elt where
  emptyIndices :: Indices pkey keys elt

empty :: (Indexable pkey keys elt) => Table pkey keys elt
empty = Table { elts = M.empty, indices = emptyIndices }

insert :: (Ord pkey, All Ord keys, PrimaryKey pkey elt)
       => elt
       -> Table pkey keys elt
       -> Either pkey (Table pkey keys elt)
insert elt Table{ elts, indices } =
  mkTable <$> M.alterF checkedInsert pkey elts
  where
    mkTable elts' = Table { elts = elts', indices = indices' }
    indices' = Idc.insert pkey elt indices
    checkedInsert = \case
      Nothing -> Right $ Just elt
      Just _  -> Left pkey
    pkey = extractPkey elt

delete :: (Ord pkey, All Ord keys)
       => pkey
       -> Table pkey keys elt
       -> (Table pkey keys elt, Maybe elt)
delete pkey Table{ elts, indices } = (table', maybeRemoved)
  where
    table' = Table{ elts = elts', indices = indices' }
    (maybeRemoved, elts') = M.alterF deleteAndSave pkey elts
    deleteAndSave x = (x, Nothing)
    indices' = case maybeRemoved of
      Nothing  -> indices
      Just elt -> Idc.delete pkey elt indices

update :: (Ord pkey, All Ord keys, PrimaryKey pkey elt)
       => elt
       -> Table pkey keys elt
       -> Maybe (Table pkey keys elt, elt)
update new Table{ elts, indices } =
  case unMaybePair $ M.alterF checkedUpdateAndSave pkey elts of
    Nothing           -> Nothing
    Just (old, elts') -> Just (table', old)
      where
        table' = Table{ elts = elts', indices = indices' }
        indices' = Idc.update pkey old new indices
  where
    pkey = extractPkey new
    checkedUpdateAndSave = \case
      Nothing  -> MaybePair Nothing
      Just old -> MaybePair $ Just (old, Just new)

newtype MaybePair b a = MaybePair { unMaybePair :: Maybe (b, a) }
  deriving (Functor)

upsert :: (Ord pkey, All Ord keys, PrimaryKey pkey elt)
       => elt
       -> Table pkey keys elt
       -> (Table pkey keys elt, Maybe elt)
upsert new Table{ elts, indices } = (table', maybeOld)
  where
    table' = Table{ elts = elts', indices = indices' }
    (maybeOld, elts') = M.alterF upsertAndSave pkey elts
    upsertAndSave x = (x, Just new)
    indices' = case maybeOld of
      Nothing  -> indices
      Just old -> Idc.update pkey old new indices
    pkey = extractPkey new

lookupByPkey :: Ord pkey => pkey -> Table pkey keys elt -> Maybe elt
lookupByPkey pkey Table{ elts } = M.lookup pkey elts

class Lookup key table where
  member :: key -> table -> Bool
  lookup :: RangeQuery key -> table -> table
  keysSet :: table -> Set key

instance {-# OVERLAPPING #-} (Ord pkey, All Ord keys) => Lookup pkey (Table pkey keys elt) where
  member pkey Table{ elts } = M.member pkey elts

  lookup RangeQuery{ key = pkey, lt, eq, gt }
             Table{ elts, indices } =
    Table{ elts = elts', indices = indices' }
    where
      elts' = M.unions [ ltMap `maskedBy` lt
                       , eqMap `maskedBy` eq
                       , gtMap `maskedBy` gt ]
      m `maskedBy` flag = bool M.empty m flag
      eqMap = maybe M.empty (M.singleton pkey) maybeEqElt
      (ltMap, maybeEqElt, gtMap) = M.splitLookup pkey elts
      indices' = Idc.restrictPkeys pkeys indices
      pkeys = M.keysSet elts'

  keysSet Table{ elts } = M.keysSet elts

instance {-# OVERLAPPABLE #-} (Ord key, Ord pkey, All Ord keys, Idc.IsIndexOf key keys) => Lookup key (Table pkey keys elt) where
  member key Table{ indices } = Idc.member key indices

  lookup query Table{ elts, indices } =
    Table{ elts = elts', indices = indices' }
    where
      elts' = M.restrictKeys elts pkeys
      indices' = Idc.restrictPkeys pkeys indices
      pkeys = Idc.lookup query indices

  keysSet Table{ indices } = Idc.keysSet indices

getLT :: (Lookup key table) => key -> table -> table
getLT key = lookup query
  where
    query = RangeQuery{ key = key, lt = True, eq = False, gt = False }

getLTE :: (Lookup key table) => key -> table -> table
getLTE key = lookup query
  where
    query = RangeQuery{ key = key, lt = True, eq = True, gt = False }

getEQ :: (Lookup key table) => key -> table -> table
getEQ key = lookup query
  where
    query = RangeQuery{ key = key, lt = False, eq = True, gt = False }

getGTE :: (Lookup key table) => key -> table -> table
getGTE key = lookup query
  where
    query = RangeQuery{ key = key, lt = False, eq = True, gt = True }

getGT :: (Lookup key table) => key -> table -> table
getGT key = lookup query
  where
    query = RangeQuery{ key = key, lt = False, eq = False, gt = True }

null :: Table pkey keys elt -> Bool
null Table{ elts } = M.null elts

size :: Table pkey keys elt -> Int
size Table{ elts } = M.size elts

fromList :: (Ord pkey, All Ord keys, Indexable pkey keys elt, Foldable t)
         => t elt
         -> Either pkey (Table pkey keys elt)
fromList = foldlM (flip insert) empty

toMap :: Table pkey keys elt -> Map pkey elt
toMap Table{ elts } = elts

data TableDiff elt
  = TableDiff
  { removed :: [elt]
  , updated :: [(elt, elt)]
  , added   :: [elt]
  }

emptyDiff :: TableDiff elt
emptyDiff = TableDiff{ removed = [], updated = [], added = [] }

tableDiff :: (Ord pkey, Eq elt)
          => Table pkey keys1 elt
          -> Table pkey keys2 elt
          -> TableDiff elt
tableDiff Table{ elts = oldElts } Table{ elts = newElts } =
  mapDiff oldElts newElts

mapDiff :: (Ord k, Eq v) => Map k v -> Map k v -> TableDiff v
mapDiff old new = ascKvListDiff (M.toAscList old) (M.toAscList new)

ascKvListDiff :: (Ord k, Eq v) => [(k, v)] -> [(k, v)] -> TableDiff v
ascKvListDiff old new = foldl' f emptyDiff $
                        zipAscKvListsWith (flip const) old new
  where
    f tbl@TableDiff{ removed, updated, added } = \case
      This x             -> tbl { removed = x : removed }
      That y             -> tbl { added = y : added }
      These x y | x /= y -> tbl { updated = (x, y) : updated }
      _                  -> tbl
      
zipAscKvListsWith :: Ord k
                  => (k -> These v v -> a)
                  -> [(k, v)]
                  -> [(k, v)]
                  -> [a]
zipAscKvListsWith f = go
  where
    go []                 kys                = map (uncurry f . mapSnd That) kys
    go kxs                []                 = map (uncurry f . mapSnd This) kxs
    go kxs@((kx, x):kxs') kys@((ky, y):kys') =
      case compare kx ky of
        LT -> f kx (This x) : go kxs' kys
        EQ -> f kx (These x y) : go kxs' kys'
        GT -> f ky (That y) : go kxs kys'
    mapSnd g (x, y) = (x, g y)
      
