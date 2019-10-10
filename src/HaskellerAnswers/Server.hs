module HaskellerAnswers.Server
       ( runServer
       ) where

import GHC.Generics (Generic)
import Lucid (renderBS, toHtml)
import Miso.TypeLevel (ToServerRoutes)
import Network.HTTP.Types (status404)
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Gzip (GzipFiles (GzipCompress), def, gzip, gzipFiles)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Servant.API ((:>), Get, JSON)
import Servant.API.Generic ((:-), ToServantApi, toServant)
import Servant.Server (Application, Handler, Server, serve)
import Servant.Server.Generic (AsServerT, genericServe)

import HaskellerAnswers.Core (Event, defaultModel)
import HaskellerAnswers.Core.Html (Wrapper (..), mainView)
import HaskellerAnswers.Server.Answers (AnswersApi, answersServer)
import HaskellerAnswers.Server.Manifest (ManifestApi, manifestServer)


runServer :: IO ()
runServer = do
    putStrLn "Running on port 3002..."
    run 3002 $ logStdout (compress application)
  where
    compress = gzip def { gzipFiles = GzipCompress }

application :: Application
application = genericServe (Proxy @HaApi) (toServant haServer :<|> Tagged handle404)

handle404 :: Application
handle404 _ respond = respond $ responseLBS
    status404
    [("Content-Type", "text/html")] $
    renderBS $ toHtml $ Wrapper $ mainView defaultModel


data HaSite route = HaSite
    { haAnswersRoute  :: route :- AnswersApi -- ToServerRoutes AnswersApi Wrapper Event
    , haManifestRoute :: route :- ManifestApi
    } deriving stock (Generic)

type HaApi = ToServantApi HaSite

haServer :: HaSite (AsServerT Handler)
haServer = HaSite
    { haAnswersRoute  = toServant answersServer
    , haManifestRoute = toServant manifestServer
    }


-- -- | Convert client side routes into server-side web handlers
-- type ServerRoutes = ToServerRoutes ClientRoutes Wrapper Event
--
-- -- | API type
-- type API = ("static" :> Raw)
--   :<|> ServerRoutes
--   :<|> ("manifest.json" :> Get '[JSON] Manifest)
