{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.IxTable.TypeLevel
  ( All
  -- , Contains, NotContains
  -- , Unique
  ) where

import GHC.Exts (Constraint)

type family All (c :: k -> Constraint) (lst :: [k]) :: Constraint where
  All c '[]       = ()
  All c (t ': ts) = (c t, All c ts)

-- type family ContainsFn (x :: k) (xs :: [k]) :: Bool where
--   ContainsFn x '[]       = 'False
--   ContainsFn x (x ': ys) = 'True
--   ContainsFn x (y ': ys) = ContainsFn x ys

-- class Contains (x :: k) (xs :: [k])

-- instance (ContainsFn x xs ~ 'True) => Contains x xs

-- class NotContains (x :: k) (xs :: [k])

-- instance (ContainsFn x xs ~ 'False) => NotContains x xs

-- type family UniqueFn (xs :: [k]) :: Bool where
--   UniqueFn '[]       = 'True
--   UniqueFn (x ': xs) = And (Not (ContainsFn x xs)) (UniqueFn xs)

-- class Unique (xs :: [k])

-- instance (UniqueFn xs ~ 'True) => Unique xs

-- type family And (a :: Bool) (b :: Bool) :: Bool where
--   And 'False x = 'False
--   And 'True  x = x

-- type family Not (a :: Bool) :: Bool where
--   Not 'False = 'True
--   Not 'True  = 'False
