module HaskellerAnswers.Server.Manifest
       ( ManifestApi
       , manifestServer
       ) where


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

manifestServer :: ManifestSite AppServer
manifestServer = ManifestSite
    { manifestRoute = pure haManifest
    }
