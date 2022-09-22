module Elements.Material where

import Prelude

import Data.Array (drop, singleton, take)
import Halogen.HTML (ClassName(..), HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties (ScopeValue(..))
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA


data Column = StringColumn String | NumericColumn String

data Cell = StringCell String | NumericCell String

dataTable ∷ ∀ w i. Array Column → Array (Array Cell) → HTML w i
dataTable columns rows = 
    HH.div [ HP.class_ $ ClassName "mdc-data-table" ] $ singleton $
    HH.div [ HP.class_ $ ClassName "mdc-data-table__table-container" ] $ singleton $
    HH.table [ HP.class_ $ ClassName "mdc-data-table__table" ]
    [ HH.thead_ $ singleton $
      HH.tr [ HP.class_ $ ClassName "mdc-data-table__header-row" ] $
      header <$> columns
    , HH.tbody [ HP.class_ $ ClassName "mdc-data-table__content" ] $
      HH.tr [ HP.class_ $ ClassName "mdc-data-table__row" ] <<< row <$> rows
    ]
    where
      header :: forall w i. Column -> HTML w i
      header (StringColumn s) = HH.th [ HP.class_ $ ClassName "mdc-data-table__header-cell"
                                      , HPA.role "columnheader"
                                      , HP.scope ScopeCol
                                      ] [ HH.text s ]
      header (NumericColumn s) = HH.th [ HP.classes [ ClassName "mdc-data-table__header-cell"
                                                    , ClassName "mdc-data-table__header-cell--numeric"
                                                    ]
                                      , HPA.role "columnheader"
                                      , HP.scope ScopeCol
                                      ] [ HH.text s ]

      row :: forall w i. Array Cell -> Array (HTML w i)
      row [] = []
      row cs = map headerCell (take 1 cs) <> map cell (drop 1 cs)

      headerCell :: forall w i. Cell -> HTML w i
      headerCell (StringCell s) = HH.th [ HP.class_ $ ClassName "mdc-data-table__cell"
                                        , HP.scope ScopeRow
                                        ] [ HH.text s ]
      headerCell (NumericCell s) = HH.th [ HP.classes [ ClassName "mdc-data-table__cell"
                                                      , ClassName "mdc-data-table__cell--numeric"
                                                      ]
                                         , HP.scope ScopeRow
                                         ] [ HH.text s ]

      cell :: forall w i. Cell -> HTML w i
      cell (StringCell s) = HH.th [ HP.class_ $ ClassName "mdc-data-table__cell" ] [ HH.text s ]
      cell (NumericCell s) = HH.th [ HP.classes [ ClassName "mdc-data-table__cell"
                                                , ClassName "mdc-data-table__cell--numeric"
                                                ]
                                   , HP.scope ScopeRow
                                   ] [ HH.text s ]
