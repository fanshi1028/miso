module Main where

import Data.Void
import Miso
import Miso.CSS (styleInline_)
import Miso.Html
import Miso.Lens

data Action = RemoveChild

newtype Model = ShowChild Bool deriving (Eq)

main :: IO ()
main = run $ miso $ \_ -> component
  (ShowChild True)
  (\RemoveChild -> this .= ShowChild False)
  $ \(ShowChild showChild) ->
    div_ [styleInline_ $ ms "border-style: solid; padding: 4px;"] $
      text (ms "Parent")
        : if showChild
          then
            [ div_ [styleInline_ $ ms "border-style: solid"] +> childComponent,
              button_ [onClick RemoveChild] [text $ ms "click to remove child"]
            ]
          else
            [ p_ [] [text $ ms "Now, check the console, you shall find error like this:"],
              p_ [styleInline_ $ ms "color:red"] [text $ ms "NotFoundError: Failed to execute 'removeChild' on 'Node': The node to be removed is not a child of this node."],
              p_ [] [text $ ms "When a child compoenent are equipped with `scripts` or `styles`, and unmounted, this error happens."]
            ]

childComponent :: Component parent () Void
childComponent =
  (component () absurd $ \_ -> div_ [] [text $ ms "child"])
    { styles = [Href $ ms ""]
    }
