-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Searchable
-- Copyright   :  (c) Murat Kasimov 2018
-- License     :  BSD3
--
-- Maintainer  :  Murat Kasimov <iokasimov.m@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-----------------------------------------------------------------------------

module Data.Searchable (Searchable (..)) where

import Control.Applicative
import Data.Functor.Identity

class (Functor t, Foldable t) => Searchable t where
    search :: Alternative f => (a -> f b) -> t a -> f (t b)

instance Searchable (Const r) where
    search _ (Const r) = pure $ Const r

instance Searchable Identity where
    search f (Identity x) = Identity <$> f x

instance Searchable Maybe where
    search _ Nothing  = pure Nothing
    search f (Just x) = Just <$> f x

instance Searchable [] where
    search _ []     = empty
    search f (x:xs) = ((:) <$> f x <*> search f xs)
        <|> search f xs <|> pure []
