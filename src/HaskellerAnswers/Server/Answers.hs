module HaskellerAnswers.Server.Answers
       ( AnswersApi
       , answersServer
       ) where


newtype AnswersSite route = AnswesrSite
    { answersRoute :: route
        :- Get '[JSON] (Wrapper (View Action))
    } deriving stock (Generic)

type AnswersApi = ToServantApi AnswersSite

answersServer :: AnswersSite AppServer
answersServer =  AnswersSite
    { answersRoute = pure NoContent
    }

answersHandler :: m (Wrapper (View Action))
serverHandlers = pure $ Wrapper $ home defaultModel
