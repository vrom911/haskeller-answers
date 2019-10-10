module HaskellerAnswers.Server.Answers
       ( AnswersApi
       , answersServer
       ) where

import Control.Monad.IO.Class (MonadIO)
import GHC.Generics (Generic)
import Miso (View)
import Servant.API (Get)
import Servant.API.Generic ((:-), ToServantApi)
import Servant.HTML.Lucid (HTML)
import Servant.Server (Handler)
import Servant.Server.Generic (AsServerT)

import HaskellerAnswers.Core (Event, defaultModel)
import HaskellerAnswers.Core.Html (Wrapper (..), mainView)


newtype AnswersSite route = AnswersSite
    { answersRoute :: route
        :- Get '[HTML] (Wrapper (View Event))
    } deriving stock (Generic)

type AnswersApi = ToServantApi AnswersSite

answersServer :: AnswersSite (AsServerT Handler)
answersServer = AnswersSite
    { answersRoute = answersHandler
    }

answersHandler :: MonadIO m => m (Wrapper (View Event))
answersHandler = pure $ Wrapper $ mainView defaultModel
