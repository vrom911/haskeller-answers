module HaskellerAnswers.Core.Html
       ( Wrapper (..)
       , mainView
       ) where

import Data.Text (Text)
import Miso (View, a_, alt_, br_, button_, class_, div_, footer_, h1_, height_, href_, i_, img_,
             onClick, p_, span_, src_, strong_, style_, text, width_)
import Miso.String (ms)

import HaskellerAnswers.Core (Event (..), Model (..))

import qualified Data.Map.Strict as M
import qualified Lucid as L
import qualified Lucid.Base as L

-- | Wrapper for setting @HTML@ doctype and header.
newtype Wrapper a = Wrapper
    { unWrapper :: a
    } deriving newtype (Show, Eq)


instance L.ToHtml a => L.ToHtml (Wrapper a) where
    toHtmlRaw :: Monad m => Wrapper a -> L.HtmlT m ()
    toHtmlRaw = L.toHtml

    toHtml :: Monad m => Wrapper a -> L.HtmlT m ()
    toHtml (Wrapper x) = do
      L.doctype_
      L.html_ [ L.lang_ "en" ] $ do
          L.head_ $ do
              L.title_ "Haskeller Answers"
              L.link_ [ L.rel_ "manifest"
                      , L.href_ "/manifest.json"
                      ]
              L.meta_ [ L.charset_ "utf-8" ]
              L.meta_ [ L.name_ "theme-color", L.content_ "#00d1b2" ]
              L.meta_ [ L.httpEquiv_ "X-UA-Compatible"
                      , L.content_ "IE=edge"
                      ]
              L.meta_ [ L.name_ "viewport"
                      , L.content_ "width=device-width, initial-scale=1"
                      ]
              L.meta_ [ L.name_ "description"
                      , L.content_ "Typical Haskeller answers to everything"
                      ]
              cssRef fontAwesomeRef
              L.script_ analytics
              jsRef "static/all.js"
          L.body_ (L.toHtml x)
      where
        jsRef href = L.with (L.script_ mempty)
            [ L.makeAttribute "src" href
            , L.makeAttribute "async" mempty
            , L.makeAttribute "defer" mempty
            ]
        cssRef href = L.with (L.link_ mempty)
            [ L.rel_ "stylesheet"
            , L.type_ "text/css"
            , L.href_ href
            ]

fontAwesomeRef :: Text
fontAwesomeRef = "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"

analytics :: Text
analytics = "console.log('later');"

----------------------------------------------------------------------------
-- View
----------------------------------------------------------------------------

mainView :: Model -> View Event
mainView Model{..} = div_ []
  [ div_ [ class_ "hero-body" ]
      [ div_ [ class_ "container" ]
          [ h1_ [] [text "Haskeller Answers"]
          , br_ []
          , div_ [] [text $ ms modelCurrentAnswer]
          , br_ []
          , button_ [ onClick NextAnswer ] [ text "Get Next Answer" ]
          ]
      ]
  , footer
  ]

-- | Footer
footer :: View action
footer = footer_ [ class_ "footer" ]
    [ div_ [ class_ "container" ]
        [ div_ [ class_ "content has-text-centered" ]
            [ p_ []
                [ a_ [ href_ "https://github.com/vrom911/haskeller-answers" ]
                    [ span_ [ class_"icon is-large"] [i_ [ class_"fa fa-github"] []]]
                ]
            ]
        ]
    ]
