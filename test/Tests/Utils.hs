module Tests.Utils (testUtils) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, (@?=))
import Utils

testUtils :: TestTree
testUtils =
  testGroup "Utils" [testWrapSimple, testWrapLong]
  where
    testWrapSimple = testCase "wrap: simple" $ do
      let input = "Hello, world!"
      let expected = "Hello,\nworld!\n"
      (wrap 5 input) @?= expected
    testWrapLong = testCase "wrap: long" $ do
      let input = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam venenatis erat dolor, quis hendrerit dui ultrices non. Morbi fringilla a quam non auctor. Proin tincidunt ante non dictum dapibus. Quisque at sodales urna. Aenean sit amet libero quis nunc venenatis aliquet. Donec convallis, leo eu malesuada placerat, tortor dui faucibus felis, nec aliquam erat sapien id mauris. Pellentesque sed vulputate eros, eget porta metus. Nullam vitae porta dui. Morbi tempus velit a arcu rutrum, in laoreet ex ultrices. Ut nec sapien vel mi vulputate consectetur at vitae ipsum. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Integer sit amet eros quis justo pulvinar maximus. Ut ac nunc quis ligula placerat vehicula."
      let expected = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam venenatis erat\ndolor, quis hendrerit dui ultrices non. Morbi fringilla a quam non auctor. Proin\ntincidunt ante non dictum dapibus. Quisque at sodales urna. Aenean sit amet\nlibero quis nunc venenatis aliquet. Donec convallis, leo eu malesuada placerat,\ntortor dui faucibus felis, nec aliquam erat sapien id mauris. Pellentesque sed\nvulputate eros, eget porta metus. Nullam vitae porta dui. Morbi tempus velit a\narcu rutrum, in laoreet ex ultrices. Ut nec sapien vel mi vulputate consectetur\nat vitae ipsum. Pellentesque habitant morbi tristique senectus et netus et\nmalesuada fames ac turpis egestas. Integer sit amet eros quis justo pulvinar\nmaximus. Ut ac nunc quis ligula placerat vehicula.\n"
      (wrap 80 input) @?= expected

