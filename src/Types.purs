module Types where

import Prelude

data RecordLabelStyle
  = ArgRecordLabels
  | AlphabetRecordLabels

derive instance eqRecordLabelStyle :: Eq RecordLabelStyle
instance showRecordLabelStyle :: Show RecordLabelStyle where
  show = case _ of
    ArgRecordLabels -> "ArgRecordLabels"
    AlphabetRecordLabels -> "AlphabetRecordLabels"
