module HaskellerAnswers
       ( runHaskellerAnswers
       ) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Miso (App (..), Effect, View, a_, br_, button_, class_, defaultEvents, div_, footer_, h1_,
             h2_, href_, i_, noEff, onClick, p_, span_, startApp, target_, text)
import Miso.String (ms)

import qualified Data.List.NonEmpty as NE
import qualified Language.Javascript.JSaddle.Warp as JSaddle


runHaskellerAnswers :: IO ()
runHaskellerAnswers = do
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

data Model = Model
    { modelAllAnswers    :: !(NonEmpty Text)
    , modelCurrentAnswer :: !Text
    } deriving stock (Eq, Show)

defaultModel :: Model
defaultModel = Model
    { modelAllAnswers    = answers
    , modelCurrentAnswer = "Hello, I am a Haskeller, I will answer anything!"
    }

data Event
    = NoEvent
    | NextAnswer
    deriving stock (Eq, Show)

updateApp :: Event -> Model -> Effect Event Model
updateApp event model = case event of
    NoEvent    -> noEff model
    NextAnswer -> let cur :| rest = modelAllAnswers model in
        noEff $ model
            { modelAllAnswers    = NE.fromList rest
            , modelCurrentAnswer = cur
            }

viewApp :: Model -> View Event
viewApp Model{..} = div_ [ class_ "container" ]
  [ div_ [class_ "ha-top"]
      [ h1_ [] [text "Haskeller Answers"]
      , br_ []
      , h2_ [class_ "content"] [text $ ms modelCurrentAnswer]
      , br_ []
      , button_ [ class_ "next", onClick NextAnswer ] [ text "Get Next Answer" ]
      ]
  , footer
  ]

-- | Footer
footer :: View action
footer = footer_ [ class_ "footer ha-footer" ]
    [ div_ [ ]
        [ div_ [ class_ "has-text-centered" ]
            [ a_ [ href_ "https://github.com/vrom911/haskeller-answers", target_ "_blank" ]
                [ span_ [ class_"icon is-large"] [i_ [ class_"fab fa-github"] []]]
            , p_ [] [text "FOR ENTERTAINMENT PURPOSES ONLY"]
            ]
        ]
    ]

answers :: NonEmpty Text
answers = NE.cycle $
      "Monad is just a monoid in the category of endofunctors" :|
    [ "Just use nix!"
    , "You can write it using lenses in a more readable way: list & traverse . _2 %%~ head"
    , "Windows? Why would you want that to work on Windows?"
    , "flip flip snd . (ap .) . flip flip fst . ((.) .) . flip . (((.) . (,)) .)"
    , "Types are tests"
    , "Types are docs"
    , "Types are IDE"
    , "fix (((<$>) <$> (:) <*> ((=<<) <$> (pure <$>) <$> (*) <$> join (+))) 1)"
    , "I recommend to use free monads instead"
    , "It's totally okay to use unsafePerformIO for global mutable variables"
    , "Don't start with learning Haskell immediately, learn lambda calculus and category theory first"
    , "It's okay if you don't get something since serious cognitive sophistication is required to use the language"
    ]
