module HaskellerAnswers.Server
       ( runServer
       ) where

import Servant.API.Generic (ToServantApi)
import Servant.Server.Generic (AsServerT)


runServer :: IO ()
runServer = do
    putStrLn "Running on port 3002..."
    run 3002 $ logStdout (compress application)
  where
    compress = gzip def { gzipFiles = GzipCompress }

server :: Server HaApi
server = serve (Proxy @HaApi) (toServant haServer :<|> Tagged handle404)

application :: Application
application = serve (Proxy @HaApi) server

handle404 :: Application
handle404 _ respond = respond $ responseLBS
    status404
    [("Content-Type", "text/html")] $
    renderBS $ toHtml $ Wrapper $ the404 defaultModel


data HaSite route = HaSite
    { haAnswersRoute  :: route :- AnswersApi
    , haManifestRoute :: route :- ManifestApi
    } deriving stock (Generic)

type HaApi = ToServantApi HaSite

haServer :: HaSite AppServer
haServer = HaSite
    { haAnswersRoute  = toServant answersServer
    , haManifestRoute = toServant manifestServer
    }


-- -- | Convert client side routes into server-side web handlers
-- type ServerRoutes = ToServerRoutes ClientRoutes Wrapper Action
--
-- -- | API type
-- type API = ("static" :> Raw)
--   :<|> ServerRoutes
--   :<|> ("manifest.json" :> Get '[JSON] Manifest)
