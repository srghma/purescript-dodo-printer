module DodoAlignCurrentColumnShouldNotIndent where

import Prelude

import Dodo
import Effect (Effect)
import Effect.Class.Console as Console

test1 :: forall a. Doc a
test1 = flexGroup $
  mapType
  `appWithPar`
  ( mapType
    `appWithPar`
    ( mapType
      `appWithoutPar`
      numberType
      `appWithoutPar`
      booleanType
    )
    `appWithPar`
    ( mapType
      `appWithPar`
      ( mapType
        `appWithoutPar`
        numberType
        `appWithoutPar`
        booleanType
      )
      `appWithPar`
      ( mapType
        `appWithoutPar`
        numberType
        `appWithoutPar`
        booleanType
      )
    )
  )
  `appWithoutPar`
  booleanType

  where
    mapType = text "Data.Map.Map"
    numberType = text "Number"
    booleanType = text "Boolean"

    appWithoutPar l r = alignCurrentColumn $ l <> spaceBreak <> r
    appWithPar l r = alignCurrentColumn $ l <> spaceBreak <> text "(" <> r <> text ")"

main :: Effect Unit
main = do
  Console.log $ print plainText ({ pageWidth: 12, ribbonRatio: 1.0, indentUnit: " ", indentWidth: 1 }) test1
