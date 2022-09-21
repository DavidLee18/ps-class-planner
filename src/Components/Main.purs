module Components.Main where

import Prelude

import Data.Array as Array
import Data.Subject (Subject(..), ex1)
import Effect.Aff (Aff)
import Halogen (Component, defaultEval)
import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML as HH


type State = { subject :: Subject }

mainComponent ∷ ∀ output query. Component query Unit output Aff
mainComponent = H.mkComponent { eval: H.mkEval $ defaultEval
                              , initialState: const { subject: ex1 }
                              , render
                              }

render ∷ ∀ w i. State → HTML w i
render { subject: (Subject s) } = HH.div_ $ Array.singleton $ HH.table_ $ map HH.tr_
    [ HH.th_ <<< Array.singleton <<< HH.text <$> ["구분", "학년", "교과목명", "과목코드", "학점", "과목구분 및 시간", "강좌시간", "정원", "타학과수강 가능여부"]
    , HH.td_ <<< Array.singleton <<< HH.text <$> [show s.subjectType, show s.grade, s.name, s.id, show s.point, show s.time, show s.lectureTimes, show s.quota, show s.open]
    ]