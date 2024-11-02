{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TidalExtension where

import qualified Data.Map as Map
import Sound.Tidal.Context

instance Valuable (String, Value) where
  toValue (st, v) = VList [toValue st, v]

instance Valuable ValueMap where
  toValue vm = VList $ map toValue (Map.toList vm)

class Unvaluable a where
  fromValue :: Value -> Maybe a
  defaultValue :: a

_cX' :: a -> (Value -> Maybe a) -> String -> Pattern a
_cX' d f st = pattern $ \x@(State _ m) -> query (maybe (pure d) (_getP_ f . valueToPattern) $ Map.lookup st m) x

_define :: (Unvaluable a) => String -> Pattern a
_define = _cX' defaultValue fromValue

instance Unvaluable Double where
  fromValue (VF x) = Just x
  fromValue _ = Nothing
  defaultValue = 0

instance Unvaluable Int where
  fromValue (VI x) = Just x
  fromValue _ = Nothing
  defaultValue = 0

instance Unvaluable Note where
  fromValue (VN x) = Just x
  fromValue _ = Nothing
  defaultValue = 0

instance Unvaluable Time where
  fromValue (VR x) = Just x
  fromValue _ = Nothing
  defaultValue = 0

instance Unvaluable String where
  fromValue (VS x) = Just x
  fromValue _ = Nothing
  defaultValue = ""

instance Unvaluable Bool where
  fromValue (VB x) = Just x
  fromValue _ = Nothing
  defaultValue = False

instance Unvaluable ValueMap where
  fromValue (VList xs) = Just (Map.fromList $ concatMap toTuples xs)
    where
      toTuples (VList [k, v]) = case fromValue k of
        Just st -> [(st, v)]
        Nothing -> []
      toTuples _ = []
  fromValue _ = Nothing
  defaultValue = Map.empty

type SamplePattern = Pattern String

type NotePattern = Pattern Note

type FreqPattern = Pattern Double

type TimePattern = Pattern Time

type IntPattern = Pattern Int

type BoolPattern = Pattern Bool

sample :: SamplePattern -> SamplePattern
sample = id

notep :: NotePattern -> NotePattern
notep = id

freqp :: FreqPattern -> FreqPattern
freqp = id

time :: TimePattern -> TimePattern
time = id

int :: IntPattern -> IntPattern
int = id

bool :: BoolPattern -> BoolPattern
bool = id
