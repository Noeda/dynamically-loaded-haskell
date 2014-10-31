{-# LANGUAGE OverloadedStrings, OverloadedLists, DeriveDataTypeable #-}

module Main ( main ) where

import Compiler.HS
import Data.Dynamic
import Data.Typeable
import Data.Monoid
import Data.Maybe
import Control.Applicative
import Control.Monad
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Text as T

main :: IO ()
main = do
    z <- loadCode "Tatti"
                  (S.fromList ["thing"])
                  ("{-# LANGUAGE AutoDeriveTypeable #-}\n" <>
                   "module Tatti ( thing ) where\n" <>
                   "import Data.Typeable\n" <>
                   "thing :: Int\n" <>
                   "thing = 5\n")
    let Just n = M.lookup "thing" z
        undyn = fromDynamic n
    print n
    print (undyn :: Maybe Int)

