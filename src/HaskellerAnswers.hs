{-# OPTIONS_GHC -fno-warn-orphans #-}

module HaskellerAnswers
       ( runHaskellerAnswers
       ) where

import Data.Bifunctor (second)
import Data.Text (Text)
import Miso (App (..), Effect, View, a_, br_, button_, class_, defaultEvents, div_, footer_, h1_,
             h2_, href_, i_, noEff, onClick, p_, span_, startApp, target_, text)
import Miso.String (ms)
import System.Random (StdGen, getStdGen, randomR)

import qualified Language.Javascript.JSaddle.Warp as JSaddle


runHaskellerAnswers :: IO ()
runHaskellerAnswers = do
    putStrLn "Working on http://localhost:8000"

    gen <- getStdGen
    JSaddle.run 8000 $ startApp $ App
        { initialAction = NoEvent
        , model         = mkDefaultModel gen
        , update        = updateApp
        , view          = viewApp
        , events        = defaultEvents
        , subs          = []
        , mountPoint    = Nothing
        }

data Model = Model
    { modelRemainingAnswers :: ![Text]
    , modelCurrentAnswer    :: !Text
    , modelRandGen          :: !StdGen
    } deriving stock (Eq, Show)

-- orphan instance because StdGen doesn't have Eq
instance Eq StdGen where
    gen1 == gen2 = show gen1 == show gen2

mkDefaultModel :: StdGen -> Model
mkDefaultModel gen = Model
    { modelRemainingAnswers = defaultAnswers
    , modelCurrentAnswer    = "Hello, I am a Haskeller, I will answer anything!"
    , modelRandGen          = gen
    }

data Event
    = NoEvent
    | NextAnswer
    deriving stock (Eq, Show)

updateApp :: Event -> Model -> Effect Event Model
updateApp event model@Model{..} = case event of
    NoEvent    -> noEff model
    NextAnswer ->
        let (cur, newAnswers, newGen) = pickRandomAnswer modelRandGen modelRemainingAnswers
        in noEff $ model
            { modelRemainingAnswers = newAnswers
            , modelCurrentAnswer    = cur
            , modelRandGen          = newGen
            }

{- | This function extracts random element from a given list and returns this
element and the list without this element. If the given list is empty
we extract from 'defaultAnswers' instead.
-}
pickRandomAnswer :: StdGen -> [Text] -> (Text, [Text], StdGen)
pickRandomAnswer gen = \case
    [] -> pickRandomAnswer gen defaultAnswers
    l  ->
        let (i, newGen) = randomR (0, length l - 1) gen
            (pick, remaining) = unsafeExtractAt i l
        in (pick, remaining, newGen)

{- | Returns an element by given index and the list without this element.
This function expects this index to be between 0 and @length l - 1@.
-}
unsafeExtractAt :: Int -> [a] -> (a, [a])
unsafeExtractAt n l
    | n < 0 = error "Negative index"
    | otherwise = go n l
  where
    go :: Int -> [a] -> (a, [a])
    go _ []     = error "Index is too large"
    go 0 (x:xs) = (x, xs)
    go i (x:xs) = second (x:) $ go (i - 1) xs

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

defaultAnswers :: [Text]
defaultAnswers =
    [ "Monad is just a monoid in the category of endofunctors"
    , "Just use nix!"
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
    , "Costate Comonad Coalgebra is equivalent of Java's member variable update technology for Haskell"
    , "You don't need language extensions, just write in Haskell98"
    ]
