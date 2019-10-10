module HaskellerAnswers.Client
       ( runClient
       ) where

import Data.List.NonEmpty (NonEmpty (..))
import Miso (App (..), Effect, View, defaultEvents, noEff, startApp)
-- import Miso.Router (runRoute)

import HaskellerAnswers.Core (Event (..), Model (..), defaultModel)
import HaskellerAnswers.Core.Html (mainView)

import qualified Data.List.NonEmpty as NE
import qualified Language.Javascript.JSaddle.Warp as JSaddle


runClient :: IO ()
runClient = do
    putStrLn "Working on http://localhost:8000"

    JSaddle.run 8000 $ startApp $ App
        { initialAction = NoEvent
        , model         = defaultModel
        , update        = updateApp
        , view          = viewApp
        , events        = defaultEvents
        , subs          = []
        , mountPoint    = Nothing
        }


viewApp :: Model -> View Event
viewApp = mainView
-- viewModel m = case runRoute (Proxy @AnswersApi) mainView modelUri m of
--     Left _  -> mainView m
--     Right v -> v

updateApp :: Event -> Model -> Effect Event Model
updateApp event model = case event of
    NoEvent    -> noEff model
    NextAnswer -> let cur :| rest = modelAllAnswers model in
        noEff $ model
            { modelAllAnswers    = NE.fromList rest
            , modelCurrentAnswer = cur
            }
