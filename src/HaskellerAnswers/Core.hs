{- | Core data types and data used in the application.
-}

module HaskellerAnswers.Core
       ( -- * Model
         Model (..)
       , defaultModel
         -- * Event
       , Event (..)
         -- * DB
       , answers
       ) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)

import qualified Data.List.NonEmpty as NE


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
