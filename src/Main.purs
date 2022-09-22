module Main where

import Prelude

import Components.Main (mainComponent)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Halogen.Aff (awaitBody)
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = launchAff_ do
  body <- awaitBody
  void $ runUI mainComponent unit body