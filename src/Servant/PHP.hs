{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Servant.PHP
  (
    writePHPForAPI
  , guzzle
  , LangPHP
  ) where

import Prelude hiding (writeFile)
import Data.Proxy
import Data.Text
import Data.Text (Text)
import Data.Typeable
import Data.Text.IO (writeFile)
import Servant.PHP.Guzzle
import Servant.PHP.Internal
import Servant.Foreign

data LangPHP

instance {-# OVERLAPPING #-} HasForeignType LangPHP Text Int where
  typeFor _ _ _ = "int"

instance {-# OVERLAPPING #-} Typeable t => HasForeignType LangPHP Text t where
  typeFor _ _ p = pack $ show $ typeRep p

phpForAPI :: (HasForeign LangPHP Text api, GenerateList Text (Foreign Text api))
          => Proxy api
          -> PHPGenerator
          -> Text
phpForAPI p gen = gen (listFromAPI (Proxy :: Proxy LangPHP) (Proxy :: Proxy Text) p)

writePHPForAPI :: (HasForeign LangPHP Text api, GenerateList Text (Foreign Text api))
               => Proxy api
               -> PHPGenerator
               -> FilePath
               -> IO ()
writePHPForAPI p gen fp = writeFile fp (phpForAPI p gen)
