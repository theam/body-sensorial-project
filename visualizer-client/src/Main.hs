module Main where

import Miso
import Miso.String

type Model = Int

data Action
    = Inc
    | Dec
    | NoOp
    deriving (Show, Eq)

main :: IO ()
main =
    startApp
        App
        { initialAction = NoOp
        , model = 0
        , update = updateModel
        , view = viewModel
        , events = defaultEvents
        , subs = []
        }

updateModel :: Action -> Model -> Effect Action Model
updateModel Inc m = noEff (m + 1)
updateModel Dec m = noEff (m - 1)
updateModel NoOp m = noEff m

viewModel :: Model -> View Action
viewModel x =
    div_
        []
        [ button_ [onClick Inc] [text "+"]
        , text (toMisoString $ show x)
        , button_ [onClick Dec] [text "-"]
        ]

foreign import javascript unsafe "document.body.innerHTML = '';"
               clear :: IO ()
