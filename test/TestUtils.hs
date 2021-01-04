{-# LANGUAGE OverloadedStrings #-}

module TestUtils (utilTests) where

import Data.Text (Text, pack, unpack)
import Model (Kind (Tag), dropDuplicate, model, models)
import Test.HUnit (Test (TestCase, TestList), assertEqual)

models' =
  [ model "m1" "M1" (Just "string") Tag False False [],
    model "m2" "M2" (Just "string") Tag False False [],
    model "m3" "M3" (Just "string") Tag False False [],
    model "m4" "M4" (Just "string") Tag False False [],
    model "m5" "M5" (Just "string") Tag False False [],
    model "m6" "M6" (Just "string") Tag False False [],
    model "m3" "M3" (Just "string") Tag False False [],
    model "m3" "M3" (Just "string") Tag False False []
  ]

models'' =
  [ model "m1" "M1" (Just "string") Tag False False [],
    model "m2" "M2" (Just "string") Tag False False [],
    model "m3" "M3" (Just "string") Tag True False [],
    model "m4" "M4" (Just "string") Tag False False [],
    model "m5" "M5" (Just "string") Tag False False [],
    model "m6" "M6" (Just "string") Tag False False []
  ]

utilTests =
  [ TestCase (assertEqual "dropDuplicate" models'' (dropDuplicate models'))
  ]
