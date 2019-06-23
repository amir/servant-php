module Servant.PHP.Internal
  ( PHPGenerator
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import Servant.Foreign

type PHPGenerator = [Req Text] -> Text
