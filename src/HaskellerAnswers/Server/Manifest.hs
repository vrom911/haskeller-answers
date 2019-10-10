{-# LANGUAGE DeriveAnyClass #-}

module HaskellerAnswers.Server.Manifest
       ( ManifestApi
       , manifestServer
       ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API ((:>), Get, JSON)
import Servant.API.Generic ((:-), ToServantApi)
import Servant.Server (Handler)
import Servant.Server.Generic (AsServerT)


data Manifest = Manifest
    { name        :: !Text
    , short_name  :: !Text
    , start_url   :: !Text
    , display     :: !Text
    , theme_color :: !Text
    , description :: !Text
    } deriving stock (Show, Eq, Generic)
      deriving anyclass (ToJSON)


haManifest :: Manifest
haManifest = Manifest
    { name        = "Haskeller Answers"
    , short_name  = "Hanswerell"
    , start_url   = "."
    , display     = "standalone"
    , theme_color = "#00d1b2"
    , description = "Typical Haskell phrases"
    }

newtype ManifestSite route = ManifestSite
    { manifestRoute :: route
        :- "manifest.json"
        :> Get '[JSON] Manifest
    } deriving stock (Generic)

type ManifestApi = ToServantApi ManifestSite

manifestServer :: ManifestSite (AsServerT Handler)
manifestServer = ManifestSite
    { manifestRoute = manifestHandler
    }

manifestHandler :: MonadIO m => m Manifest
manifestHandler = pure haManifest
