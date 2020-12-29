module TestUtils (utilTests) where

import Test.HUnit (Test (TestCase, TestList), assertEqual)

test1 = TestCase (assertEqual "test something" 0 0)

utilTests =
  TestList
    [ TestCase (assertEqual "test something1" 0 0),
      TestCase (assertEqual "test something2" 1 1)
    ]
