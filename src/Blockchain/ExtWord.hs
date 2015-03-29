{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Blockchain.ExtWord (
  Word160,
  Word256,
  Word512,
  word256ToBytes,
  bytesToWord256
  ) where

import Data.Bits
import Data.Word
import Network.Haskoin.Internals (Word160, Word256, Word512)

import Data.Ix

instance Ix Word256 where
    range (x, y) | x == y = [x]
    range (x, y) = x:range (x+1, y)
    index (x, y) z | z < x || z > y = error $ "Ix{Word256}.index: Index (" ++ show z ++ ") out of range ((" ++ show x ++ "," ++ show y ++ "))"
    index (x, _) z = fromIntegral $ z - x
    inRange (x, y) z | z >= x && z <= y = True 
    inRange _ _ = False

word256ToBytes::Word256->[Word8]
word256ToBytes word = map (fromIntegral . (word `shiftR`)) [256-8, 256-16..0]
  
bytesToWord256::[Word8]->Word256
bytesToWord256 bytes | length bytes == 32 =
  sum $ map (\(shiftBits, byte) -> fromIntegral byte `shiftL` shiftBits) $ zip [256-8,256-16..0] bytes
bytesToWord256 _ = error "bytesToWord256 was called with the wrong number of bytes"  
