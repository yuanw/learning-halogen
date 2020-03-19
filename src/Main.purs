module Main where


-- Imports for lesson

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Random (randomInt)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)


type State = Maybe Int

data Action = Regenerate

component :: forall q i o m. MonadEffect m =>  H.Component HH.HTML q i o m
component =
  H.mkComponent
     { initialState
     , render
     , eval: H.mkEval $ H.defaultEval {handleAction = handleAction}
     }


initialState :: forall i. i -> State
initialState = const Nothing

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  let
    value = maybe "No number generated yet" show state
  in
    HH.div_ $
      [ HH.h1_ [ HH.text "Random number" ]
      , HH.p_ [ HH.text ("Current value: " <> value) ]
      , HH.button
          [ HE.onClick \_ -> Just Regenerate ]
          [ HH.text "Generate new number" ]
      ]




handleAction :: forall o m. MonadEffect m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Regenerate -> do
    newNumber <- H.liftEffect $ randomInt 0 100
    H.put (Just newNumber)



main :: Effect Unit
main = HA.runHalogenAff do
  --log "🍝"
  body <- HA.awaitBody
  --runUI (staticComponent staticHtml) unit body
  runUI component unit body
