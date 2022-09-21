module Main where

import Prelude

import Components.Main (mainComponent)
import Data.Subject (ex1)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Halogen.Aff (awaitBody)
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = launchAff_ do
  body <- awaitBody
  void $ runUI mainComponent unit body