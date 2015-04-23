
module Blockchain.DB.CodeDB (
               addCode,
               getCode
              ) where

import Data.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Blockchain.DBM
import Blockchain.ExtDBs
import qualified Blockchain.Database.MerklePatricia as MP
import Blockchain.SHA

addCode::B.ByteString->DBM ()
addCode = codeDBPut

getCode::SHA->DBM (Maybe B.ByteString)
getCode theHash = 
  codeDBGet (BL.toStrict $ encode $ MP.sha2SHAPtr theHash)
