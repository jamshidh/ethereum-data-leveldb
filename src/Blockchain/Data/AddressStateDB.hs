{-# LANGUAGE OverloadedStrings #-}

module Blockchain.Data.AddressStateDB (
  AddressState(..),
  blankAddressState,
  getAddressState,
  getAllAddressStates,
  putAddressState,
  deleteAddressState,
  addressStateExists
  ) where

import Data.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Functor
import Data.List
import Numeric
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Blockchain.Data.Address
import qualified Blockchain.Colors as CL
import Blockchain.DBM
import Blockchain.ExtDBs
import Blockchain.Format
import qualified Data.NibbleString as N
import Blockchain.Data.RLP
import Blockchain.SHA
import Blockchain.Util

--import Debug.Trace

data AddressState = AddressState { addressStateNonce::Integer, addressStateBalance::Integer, addressStateContractRoot::SHAPtr, addressStateCodeHash::SHA } deriving (Show)


blankAddressState::AddressState
blankAddressState = AddressState { addressStateNonce=0, addressStateBalance=0, addressStateContractRoot=emptyTriePtr, addressStateCodeHash=hash "" }

instance Format AddressState where
  format a = CL.blue "AddressState" ++
             tab("\nnonce: " ++ showHex (addressStateNonce a) "" ++
                 "\nbalance: " ++ show (toInteger $ addressStateBalance a) ++
                 "\ncontractRoot: " ++ show (pretty $ addressStateContractRoot a) ++
                 "\ncodeHash: " ++ show (pretty $ addressStateCodeHash a))
  
instance RLPSerializable AddressState where
  --rlpEncode a | balance a < 0 = rlpEncode a{balance = - balance a}
  rlpEncode a | addressStateBalance a < 0 = error $ "Error in cal to rlpEncode for AddressState: AddressState has negative balance: " ++ format a
  rlpEncode a = RLPArray [
    rlpEncode $ toInteger $ addressStateNonce a,
    rlpEncode $ toInteger $ addressStateBalance a,
    rlpEncode $ addressStateContractRoot a,
    rlpEncode $ addressStateCodeHash a
                ]

  rlpDecode (RLPArray [n, b, cr, ch]) =
    AddressState {
      addressStateNonce=fromInteger $ rlpDecode n,
      addressStateBalance=fromInteger $ rlpDecode b,
      addressStateContractRoot=rlpDecode cr,
      addressStateCodeHash=rlpDecode ch
      } 
  rlpDecode x = error $ "Missing case in rlpDecode for AddressState: " ++ show (pretty x)

addressAsNibbleString::Address->N.NibbleString
addressAsNibbleString (Address s) = N.EvenNibbleString $ BL.toStrict $ encode s

getAddressState::Address->DBM AddressState
getAddressState address = do
  states <- getKeyVal $ addressAsNibbleString address
  return $ maybe blankAddressState (rlpDecode . rlpDeserialize . rlpDecode) states
  
nibbleString2ByteString::N.NibbleString->B.ByteString
nibbleString2ByteString (N.EvenNibbleString str) = str
nibbleString2ByteString (N.OddNibbleString c str) = c `B.cons` str

getAllAddressStates::DBM [(Address, AddressState)]
getAllAddressStates = do
  states <- getAllKeyVals
  return $ map convert $ states
  where
    convert::(N.NibbleString, RLPObject)->(Address, AddressState)
    convert (k, v) = (Address $ fromInteger $ byteString2Integer $ nibbleString2ByteString k, rlpDecode . rlpDeserialize . rlpDecode $ v)
  

putAddressState::Address->AddressState->DBM ()
putAddressState address newState = 
  putKeyVal (addressAsNibbleString address) $ rlpEncode $ rlpSerialize $ rlpEncode newState

deleteAddressState::Address->DBM ()
deleteAddressState address = 
  deleteKey (addressAsNibbleString address)

addressStateExists::Address->DBM Bool
addressStateExists address = 
  keyExists (addressAsNibbleString address)

