{-# LANGUAGE OverloadedStrings #-}
module Builder where

import Data.Int (Int64)
import Data.Monoid (mconcat)

import Criterion.Main
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as S

import qualified Text.Blaze.Internal.Builder.Core as B

main :: IO ()
main = do
    defaultMain [ bench "ByteArray"  $ whnf benchArray  arrays
                , bench "ByteString" $ whnf benchString strings
                ]
  where
    arrays :: [B.ByteArray]
    arrays = map B.makeByteArray $ strings
    {-# NOINLINE arrays #-}

    strings :: [S.ByteString]
    strings = replicate 10000 "<img>"
    {-# NOINLINE strings #-}

benchString :: [S.ByteString] -> Int64
benchString = BL.length . B.toLazyByteString
            . mconcat . map B.copyByteString

benchArray :: [B.ByteArray] -> Int64
benchArray = BL.length . B.toLazyByteString
           . mconcat . map B.fromByteArray
