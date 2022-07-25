{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.HList
  ( HList (..),
    hlength,
    happend,
  )
where

import Data.Kind

type HList :: [Type] -> Type
data HList xs where
  HNil :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

infixr 5 `HCons`

instance Show (HList '[]) where
  show _ = "[]"

instance (Show x, Show (HList xs)) => Show (HList (x ': xs)) where
  show (x `HCons` xs) =
    let '[' : s = show xs
     in "[" ++ show x ++ (if s == "]" then s else ", " ++ s)

instance Eq (HList '[]) where
  _ == _ = True

instance (Eq x, Eq (HList xs)) => Eq (HList (x ': xs)) where
  (x `HCons` xs) == (y `HCons` ys) = x == y && xs == ys

hlength :: HList xs -> Int
hlength HNil = 0
hlength (_ `HCons` xs) = 1 + hlength xs

type Append :: forall a. [a] -> [a] -> [a]
type family Append xs ys where
  Append '[] ys = ys
  Append (x : xs) ys = x ': Append xs ys

happend :: HList xs -> HList ys -> HList (Append xs ys)
happend HNil ys = ys
happend (x `HCons` xs) ys = x `HCons` xs `happend` ys
