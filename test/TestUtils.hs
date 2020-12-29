module TestUtils (utilTests) where

import Lib
import Test.HUnit (Test (TestCase, TestList), assertEqual)

test1 = TestCase (assertEqual "test something" 0 0)

test2 =
  TestCase
    ( do
        s <- compile Go
        assertEqual "for the first result of partA," s ""
    )

utilTests =
  TestList
    [ TestCase (assertEqual "test something2" 1 1),
      TestCase
        ( do
            s <- compile Go
            assertEqual "gen go package" s ""
        )
    ]
