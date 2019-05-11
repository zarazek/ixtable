{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE LambdaCase              #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}

module Data.IxTable.Indices
  ( Indices
  , mkIndices
  , insert, delete, update
  , restrictPkeys
  , IsIndexOf()
  , lookup
  , keysSet
  ) where

import           Prelude                 hiding (lookup)
import           Data.Set                (Set)

import qualified Data.IxTable.Index      as Idx
import           Data.IxTable.Index      (Index)
import           Data.IxTable.RangeQuery
import           Data.IxTable.TypeLevel

data Indices (pkey :: *) (keys :: [*]) (elt :: *) where
  INil  :: Indices pkey '[] elt
  ICons :: !(Index pkey key elt) -> !(Indices pkey keys elt) -> Indices pkey (key ': keys) elt

mkIndices :: MkIndices keys keys pkey elt r => r
mkIndices = mkIndices' id

class MkIndices keys keys' pkey elt r | r -> pkey elt keys keys' where
  mkIndices' :: (Indices pkey keys elt -> Indices pkey keys' elt) -> r

instance MkIndices '[] keys pkey elt (Indices pkey keys elt) where
  mkIndices' acc = acc INil

instance MkIndices keys keys' pkey elt r =>
         MkIndices (key ': keys) keys' pkey elt (Index pkey key elt -> r) where
  mkIndices' acc idx = mkIndices' $ \indices -> acc $ ICons idx indices

insert :: (Ord pkey, All Ord keys)
       => pkey
       -> elt
       -> Indices pkey keys elt
       -> Indices pkey keys elt
insert pkey elt = mapIndices (Idx.insert pkey elt)

delete :: (Ord pkey, All Ord keys)
       => pkey
       -> elt
       -> Indices pkey keys elt
       -> Indices pkey keys elt
delete pkey elt = mapIndices (Idx.delete pkey elt)

update :: (Ord pkey, All Ord keys)
       => pkey
       -> elt
       -> elt
       -> Indices pkey keys elt
       -> Indices pkey keys elt
update pkey old new = mapIndices (Idx.update pkey old new)

restrictPkeys :: (Ord pkey, All Ord keys)
              => Set pkey
              -> Indices pkey keys elt
              -> Indices pkey keys elt
restrictPkeys pkeys = mapIndices (Idx.restrictPkeys pkeys)

mapIndices :: All Ord keys
           => (forall key. Ord key => Index pkey key elt -> Index pkey key elt)
           -> Indices pkey keys elt
           -> Indices pkey keys elt
mapIndices _ INil             = INil
mapIndices f (ICons idx idxs) = ICons (f idx) (mapIndices f idxs)

class IsIndexOf (key :: *) (keys :: [*]) where
  access :: Indices pkey keys elt -> Index pkey key elt

instance {-# OVERLAPPING #-} IsIndexOf key (key ': keys) where
  access (ICons idx _) = idx

instance {-# OVERLAPPABLE #-} IsIndexOf key1 keys =>
                              IsIndexOf key1 (key2 ': keys) where
  access (ICons _ idxs) = access idxs

lookup :: (Ord pkey, Ord key, IsIndexOf key keys)
       => RangeQuery key
       -> Indices pkey keys elt
       -> Set pkey
lookup query indices = Idx.lookup query $ access indices

keysSet :: IsIndexOf key keys => Indices pkey keys elt -> Set key
keysSet indices = Idx.keysSet $ access indices
