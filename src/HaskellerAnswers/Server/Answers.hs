module HaskellerAnswers.Server.Answers
       ( AnswersApi
       , answersServer
       ) where

import Control.Monad.IO.Class (MonadIO)
import GHC.Generics (Generic)
import Miso (View)
import Servant.API (Get, JSON)
import Servant.API.Generic ((:-), ToServantApi)
import Servant.Server (Handler)
import Servant.Server.Generic (AsServerT)

import HaskellerAnswers.Core (Event, defaultModel)
import HaskellerAnswers.Core.Html (Wrapper (..), mainView)


newtype AnswersSite route = AnswersSite
    { answersRoute :: route
        :- Get '[JSON] (Wrapper (View Event))
    } deriving stock (Generic)

type AnswersApi = ToServantApi AnswersSite

answersServer :: AnswersSite (AsServerT Handler)
answersServer = AnswersSite
    { answersRoute = answersHandler
    }

answersHandler :: MonadIO m => m (Wrapper (View Event))
answersHandler = pure $ Wrapper $ mainView defaultModel
