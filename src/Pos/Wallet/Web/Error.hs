-- | Types describing runtime errors related to Wallet.

module Pos.Wallet.Web.Error
       ( WalletError (..)
       ) where

import qualified Data.Text.Buildable
import           Formatting          (bprint, stext, (%))
import           Universum

data WalletError
    -- | Reasonable error for given request
    -- (e.g. get info about non-existent wallet)
    = RequestError !Text
    -- | Internal info, which ideally should never happen
    | InternalError !Text
    deriving (Show, Generic)

instance Exception WalletError

instance Buildable WalletError where
    build (RequestError  msg) = bprint ("Request error ("%stext%")") msg
    build (InternalError msg) = bprint ("Internal error ("%stext%")") msg
