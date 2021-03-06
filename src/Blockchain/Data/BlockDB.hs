{-# LANGUAGE OverloadedStrings, ForeignFunctionInterface #-}

module Blockchain.Data.BlockDB (
  BlockData(..),
  Block(..),
  blockHash,
  powFunc,
  headerHashWithoutNonce,
  addNonceToBlock,
  findNonce,
  fastFindNonce,
  nonceIsValid,
  getBlock,
  putBlock
  ) where

import qualified Crypto.Hash.SHA3 as C
import Data.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base16 as B16
import Data.ByteString.Internal
import Data.Functor
import Data.List
import Data.Maybe
import Data.Time
import Data.Time.Clock.POSIX
import Foreign
import Foreign.ForeignPtr.Unsafe
import Numeric
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import qualified Blockchain.Colors as CL
import Blockchain.Data.Address
import Blockchain.Data.Transaction
import Blockchain.Database.MerklePatricia
import Blockchain.Data.RLP
import Blockchain.DBM
import Blockchain.ExtDBs
import Blockchain.ExtWord
import Blockchain.Format
import Blockchain.SHA
import Blockchain.Util

data BlockData = BlockData {
  blockDataParentHash::SHA,
  blockDataUnclesHash::SHA,
  blockDataCoinbase::Address,
  blockDataStateRoot::SHAPtr,
  blockDataTransactionsRoot::SHAPtr,
  blockDataReceiptsRoot::SHAPtr,
  blockDataLogBloom::B.ByteString,
  blockDataDifficulty::Integer,
  blockDataNumber::Integer,
  blockDataGasLimit::Integer,
  blockDataGasUsed::Integer,
  blockDataTimestamp::UTCTime,
  blockDataExtraData::Integer,
  blockDataNonce::Word64,
  blockDataMixHash::SHA
} deriving (Show)

data Block = Block {
  blockBlockData::BlockData,
  blockReceiptTransactions::[Transaction],
  blockBlockUncles::[BlockData]
  } deriving (Show)

instance Format Block where
  format b@Block{blockBlockData=bd, blockReceiptTransactions=receipts, blockBlockUncles=uncles} =
    CL.blue ("Block #" ++ show (blockDataNumber bd)) ++ " " ++
    tab (show (pretty $ blockHash b) ++ "\n" ++
         format bd ++
         (if null receipts
          then "        (no transactions)\n"
          else tab (intercalate "\n    " (format <$> receipts))) ++
         (if null uncles
          then "        (no uncles)"
          else tab ("Uncles:" ++ tab ("\n" ++ intercalate "\n    " (format <$> uncles)))))
              
instance RLPSerializable Block where
  rlpDecode (RLPArray [bd, RLPArray transactionReceipts, RLPArray uncles]) =
    Block (rlpDecode bd) (rlpDecode <$> transactionReceipts) (rlpDecode <$> uncles)
  rlpDecode (RLPArray arr) = error ("rlpDecode for Block called on object with wrong amount of data, length arr = " ++ show arr)
  rlpDecode x = error ("rlpDecode for Block called on non block object: " ++ show x)

  rlpEncode Block{blockBlockData=bd, blockReceiptTransactions=receipts, blockBlockUncles=uncles} =
    RLPArray [rlpEncode bd, RLPArray (rlpEncode <$> receipts), RLPArray $ rlpEncode <$> uncles]

instance RLPSerializable BlockData where
  rlpDecode (RLPArray [v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15]) =
    BlockData {
      blockDataParentHash = rlpDecode v1,
      blockDataUnclesHash = rlpDecode v2,
      blockDataCoinbase = rlpDecode v3,
      blockDataStateRoot = rlpDecode v4,
      blockDataTransactionsRoot = rlpDecode v5,
      blockDataReceiptsRoot = rlpDecode v6,
      blockDataLogBloom = rlpDecode v7,
      blockDataDifficulty = rlpDecode v8,
      blockDataNumber = rlpDecode v9,
      blockDataGasLimit = rlpDecode v10,
      blockDataGasUsed = rlpDecode v11,
      blockDataTimestamp = posixSecondsToUTCTime $ fromInteger $ rlpDecode v12,
      blockDataExtraData = rlpDecode v13,
      blockDataMixHash = rlpDecode v14,
      blockDataNonce = bytesToWord64 $ B.unpack $ rlpDecode v15
      }  
  rlpDecode (RLPArray arr) = error ("Error in rlpDecode for Block: wrong number of items, expected 15, got " ++ show (length arr) ++ ", arr = " ++ show (pretty arr))
  rlpDecode x = error ("rlp2BlockData called on non block object: " ++ show x)


  rlpEncode bd =
    RLPArray [
      rlpEncode $ blockDataParentHash bd,
      rlpEncode $ blockDataUnclesHash bd,
      rlpEncode $ blockDataCoinbase bd,
      rlpEncode $ blockDataStateRoot bd,
      rlpEncode $ blockDataTransactionsRoot bd,
      rlpEncode $ blockDataReceiptsRoot bd,
      rlpEncode $ blockDataLogBloom bd,
      rlpEncode $ blockDataDifficulty bd,
      rlpEncode $ blockDataNumber bd,
      rlpEncode $ blockDataGasLimit bd,
      rlpEncode $ blockDataGasUsed bd,
      rlpEncode (round $ utcTimeToPOSIXSeconds $ blockDataTimestamp bd::Integer),
      rlpEncode $ blockDataExtraData bd,
      rlpEncode $ blockDataMixHash bd,
      rlpEncode $ B.pack $ word64ToBytes $ blockDataNonce bd
      ]

blockHash::Block->SHA
blockHash (Block info _ _) = hash . rlpSerialize . rlpEncode $ info

instance Format BlockData where
  format b = 
    "parentHash: " ++ show (pretty
                            $ blockDataParentHash b) ++ "\n" ++
    "unclesHash: " ++ show (pretty $ blockDataUnclesHash b) ++ 
    (if blockDataUnclesHash b == hash (B.pack [0xc0]) then " (the empty array)\n" else "\n") ++
    "coinbase: " ++ show (pretty $ blockDataCoinbase b) ++ "\n" ++
    "stateRoot: " ++ show (pretty $ blockDataStateRoot b) ++ "\n" ++
    "transactionsRoot: " ++ show (pretty $ blockDataTransactionsRoot b) ++ "\n" ++
    "receiptsRoot: " ++ show (pretty $ blockDataReceiptsRoot b) ++ "\n" ++
    "difficulty: " ++ show (blockDataDifficulty b) ++ "\n" ++
    "gasLimit: " ++ show (blockDataGasLimit b) ++ "\n" ++
    "gasUsed: " ++ show (blockDataGasUsed b) ++ "\n" ++
    "timestamp: " ++ show (blockDataTimestamp b) ++ "\n" ++
    "extraData: " ++ show (pretty $ blockDataExtraData b) ++ "\n" ++
    "nonce: " ++ showHex (blockDataNonce b) "" ++ "\n"

getBlock::SHA->DBM (Maybe Block)
getBlock h = 
  fmap (rlpDecode . rlpDeserialize) <$> blockDBGet (BL.toStrict $ encode h)

putBlock::Block->DBM ()
putBlock b = do
  let bytes = rlpSerialize $ rlpEncode b
  blockDBPut (BL.toStrict $ encode $ blockHash b) bytes


--------------------------
--Mining stuff

--used as part of the powFunc
noncelessBlockData2RLP::BlockData->RLPObject
noncelessBlockData2RLP bd =
  RLPArray [
      rlpEncode $ blockDataParentHash bd,
      rlpEncode $ blockDataUnclesHash bd,
      rlpEncode $ blockDataCoinbase bd,
      rlpEncode $ blockDataStateRoot bd,
      rlpEncode $ blockDataTransactionsRoot bd,
      rlpEncode $ blockDataReceiptsRoot bd,
      rlpEncode $ blockDataLogBloom bd,
      rlpEncode $ blockDataDifficulty bd,
      rlpEncode $ blockDataNumber bd,
      rlpEncode $ blockDataGasLimit bd,
      rlpEncode $ blockDataGasUsed bd,
      rlpEncode (round $ utcTimeToPOSIXSeconds $ blockDataTimestamp bd::Integer),
      rlpEncode $ blockDataExtraData bd
      ]

{-
noncelessBlock2RLP::Block->RLPObject
noncelessBlock2RLP Block{blockData=bd, receiptTransactions=receipts, blockUncles=[]} =
  RLPArray [noncelessBlockData2RLP bd, RLPArray (rlpEncode <$> receipts), RLPArray []]
noncelessBlock2RLP _ = error "noncelessBock2RLP not definted for blockUncles /= []"
-}


headerHashWithoutNonce::Block->ByteString
headerHashWithoutNonce b = C.hash 256 $ rlpSerialize $ noncelessBlockData2RLP $ blockBlockData b

powFunc::Block->Integer
powFunc b =
  --trace (show $ headerHashWithoutNonce b) $
  byteString2Integer $ 
  C.hash 256 $
    headerHashWithoutNonce b
    `B.append`
    B.pack (word64ToBytes (blockDataNonce $ blockBlockData b))

nonceIsValid::Block->Bool
nonceIsValid b = powFunc b * blockDataDifficulty (blockBlockData b) < (2::Integer)^(256::Integer)

addNonceToBlock::Block->Integer->Block
addNonceToBlock b n =
  b {
    blockBlockData=(blockBlockData b) {blockDataNonce=fromInteger n}
    }

findNonce::Block->Integer
findNonce b =
    fromMaybe (error "Huh?  You ran out of numbers!!!!") $
              find (nonceIsValid . addNonceToBlock b) [1..]


----------

fastFindNonce::Block->IO Integer
fastFindNonce b = do
  let (theData, _, _) = toForeignPtr $ headerHashWithoutNonce b
  let (theThreshold, _, _) = toForeignPtr threshold
  retValue <- mallocArray 32
  retInt <- c_fastFindNonce (unsafeForeignPtrToPtr theData) (unsafeForeignPtrToPtr theThreshold) retValue
  print retInt
  retData <- peekArray 32 retValue
  return $ byteString2Integer $ B.pack retData
  where
    threshold::B.ByteString
    threshold = fst $ B16.decode $ BC.pack $ padZeros 64 $ showHex ((2::Integer)^(256::Integer) `quot` blockDataDifficulty (blockBlockData b)) ""

foreign import ccall "findNonce" c_fastFindNonce::Ptr Word8->Ptr Word8->Ptr Word8->IO Int
--foreign import ccall "fastFindNonce" c_fastFindNonce::ForeignPtr Word8->ForeignPtr Word8->ForeignPtr Word8


{-
fastFindNonce::Block->Integer
fastFindNonce b =
  byteString2Integer $ BC.pack $ 
  BC.unpack $
  C.hash 256 (
    first `B.append` second)
  where
    first = headerHashWithoutNonce b

fastPowFunc::Block->Integer
fastPowFunc b =
  --trace (show $ headerHashWithoutNonce b) $
  byteString2Integer $ BC.pack $ 
  BC.unpack $
  C.hash 256 (
    headerHashWithoutNonce b
    `B.append`
    sha2ByteString (nonce $ blockData b))
-}
