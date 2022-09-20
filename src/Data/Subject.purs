module Data.Subject where

import Prelude

import Control.Alternative (guard)
import Data.Date.Component (Weekday(..))
import Data.Foldable (class Foldable, all, elem, minimum, or, sum)
import Data.Generic.Rep (class Generic)
import Data.List.Lazy (List, Step(..), elemIndex, length, nil, (..), (:))
import Data.List.Lazy as List
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))

data SubjectType = Major | SelectiveMajor | Mandatory | GE

derive instance Eq SubjectType
derive instance Ord SubjectType
derive instance Generic SubjectType _
instance Show SubjectType where show = genericShow

data Grade = Freshman | Sophomore | Junior | Senior

derive instance Eq Grade
derive instance Ord Grade
derive instance Generic Grade _
instance Show Grade where show = genericShow

data Time = Theory Int | Practice Int | TheoryWithPractice Int Int

derive instance Eq Time
derive instance Ord Time
derive instance Generic Time _
instance Show Time where show = genericShow

data LectureTime = LectureTime Weekday Int

derive instance Eq LectureTime
derive instance Ord LectureTime
derive instance Generic LectureTime _
instance Show LectureTime where show = genericShow

type Point = Int

newtype Subject = Subject { subjectType :: SubjectType
                          , grade :: Grade
                          , name :: String
                          , id :: String
                          , point :: Point
                          , time :: Time
                          , lectureTimes :: List LectureTime
                          , quota :: Int
                          , open :: Boolean
                          }

derive newtype instance Eq Subject
derive newtype instance Ord Subject
derive instance Generic Subject _
instance Show Subject where show = genericShow

type Class = List Subject

ex1 :: Subject
ex1 = Subject { subjectType: SelectiveMajor
              , grade: Senior
              , name: "토지경제학"
              , id: "02691-01"
              , point: 3
              , time: Theory 3
              , lectureTimes: LectureTime Monday <$> (7 .. 9)
              , quota: 36
              , open: false
              }


planClasses ∷ ∀ f. Foldable f ⇒ Grade → Int /\ Int → String → List SubjectType → f LectureTime → List Subject → List Class
planClasses g (pMin /\ pMax) major types available subjects = do
  xs <- unique types =<< powerSet subjects
  guard $ sum ((\(Subject s) -> s.point) <$> xs) `elem` (pMin .. pMax)
  guard $ all (_ `elem` available) ((\(Subject s) -> s.lectureTimes) =<< xs)
  guard $ length xs <= 7 * 15
  pure xs
  where
    unique ∷ List SubjectType → Class → List Class
    unique ts c = do
        { yes: ds, no: ps } <- duplicates c
        d <- sortByPreference ts ds
        pure $ d : ps

    sortByPreference ∷ List SubjectType → List Subject → List Subject
    sortByPreference ts = sortBy (\(Subject s1) (Subject s2) -> compare (elemIndex s1.subjectType ts) (elemIndex s2.subjectType ts))

    preferedSubject ∷ List SubjectType → List Subject → Maybe Subject
    preferedSubject = (minimum <<< _ ) <$> sortByPreference

    duplicates ∷ Class → List { no ∷ List Subject , yes ∷ List Subject }
    duplicates c = do
        x <- c
        pure $ List.partition (overlapped x) c

    overlapped :: Subject -> Subject -> Boolean
    overlapped (Subject x) (Subject y) = or ((==) <$> x.lectureTimes <*> y.lectureTimes)

    powerSet ∷ ∀ a. List a → List (List a)
    powerSet l = case List.step l of
        Nil -> List.singleton nil
        Cons x xs -> map (x : _) (powerSet xs) <> powerSet xs



sort ∷ ∀ a. Ord a ⇒ List a → List a
sort = sortBy compare

sortBy :: forall a. (a -> a -> Ordering) -> List a -> List a
sortBy cmp = mergeAll <<< sequences
  -- implementation lifted from http://hackage.haskell.org/package/base-4.8.0.0/docs/src/Data-OldList.html#sort
  where
  sequences :: List a -> List (List a)
  sequences l = case List.step l of
    Cons a l' -> case List.step l' of
      Cons b xs -> if a `cmp` b == GT
                   then descending b (List.singleton a) xs
                   else ascending b (a : _) xs
      Nil -> List.singleton l
    Nil -> List.singleton l

  descending :: a -> List a -> List a -> List (List a)
  descending a as l = case List.step l of
    Cons b bs -> if a `cmp` b == GT
                 then descending b (a : as) bs
                 else (a : as) : sequences (b : bs)
    Nil -> (a : as) : sequences l

  ascending :: a -> (List a -> List a) -> List a -> List (List a)
  ascending a as l = case List.step l of
    Cons b bs -> if a `cmp` b /= GT
                 then ascending b (\ys -> as (a : ys)) bs
                 else ((as $ List.singleton a) : sequences bs)
    Nil -> ((as $ List.singleton a) : sequences l)

  mergeAll :: List (List a) -> List a
  mergeAll l = case List.step l of
    Cons x nil -> x
    _ -> mergeAll $ mergePairs l

  mergePairs :: List (List a) -> List (List a)
  mergePairs l = case List.step l of
    Cons a l' -> case List.step l' of
      Cons b xs -> merge a b : mergePairs xs
      Nil -> l
    Nil -> l

  merge :: List a -> List a -> List a
  merge as bs = case List.step as /\ List.step bs of
    Nil /\ _ -> bs
    _ /\ Nil -> as
    Cons a as' /\ Cons b bs' -> if a `cmp` b == GT
                                then b : merge as bs'
                                else a : merge as' bs

