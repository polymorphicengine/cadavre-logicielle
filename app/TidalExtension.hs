{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TidalExtension where

import qualified Data.Map as Map
import Sound.Tidal.Context

instance Valuable (String, Value) where
  toValue (st, v) = VList [toValue st, v]

instance Valuable ValueMap where
  toValue vm = VList $ map toValue (Map.toList vm)

_cX' :: a -> (Value -> Maybe a) -> String -> Pattern a
_cX' d f st = pattern $ \x@(State _ m) -> query (maybe (pure d) (_getP_ f . valueToPattern) $ Map.lookup st m) x

_defineDouble :: String -> Pattern Double
_defineDouble = _cX' 0 _valToDouble

_defineInt :: String -> Pattern Int
_defineInt = _cX' 0 _valToInt

_defineNote :: String -> Pattern Note
_defineNote = _cX' 0 _valToNote

_defineTime :: String -> Pattern Time
_defineTime = _cX' 0 _valToTime

_defineBool :: String -> Pattern Bool
_defineBool = _cX' False _valToBool

_defineString :: String -> Pattern String
_defineString = _cX' "" _valToString

_defineVM :: String -> Pattern ValueMap
_defineVM = _cX' Map.empty _valToVM

_valToDouble :: Value -> Maybe Double
_valToDouble (VF x) = Just x
_valToDouble _ = Nothing

_valToInt :: Value -> Maybe Int
_valToInt (VI x) = Just x
_valToInt _ = Nothing

_valToNote :: Value -> Maybe Note
_valToNote (VN x) = Just x
_valToNote _ = Nothing

_valToTime :: Value -> Maybe Time
_valToTime (VR x) = Just x
_valToTime _ = Nothing

_valToString :: Value -> Maybe String
_valToString (VS x) = Just x
_valToString _ = Nothing

_valToBool :: Value -> Maybe Bool
_valToBool (VB x) = Just x
_valToBool _ = Nothing

_valToVM :: Value -> Maybe ValueMap
_valToVM (VList xs) = Just (Map.fromList $ concatMap toTuples xs)
  where
    toTuples (VList [k, v]) = case _valToString k of
      Just st -> [(st, v)]
      Nothing -> []
    toTuples _ = []
_valToVM _ = Nothing

streamSetDouble :: Stream -> String -> Pattern Double -> IO ()
streamSetDouble = streamSet

streamSetInt :: Stream -> String -> Pattern Int -> IO ()
streamSetInt = streamSet

streamSetBool :: Stream -> String -> Pattern Bool -> IO ()
streamSetBool = streamSet

streamSetNote :: Stream -> String -> Pattern Note -> IO ()
streamSetNote = streamSet

streamSetVM :: Stream -> String -> Pattern ValueMap -> IO ()
streamSetVM = streamSet

streamSetString :: Stream -> String -> Pattern String -> IO ()
streamSetString = streamSet

streamSetTime :: Stream -> String -> Pattern Time -> IO ()
streamSetTime = streamSet
