{-# LANGUAGE OverloadedStrings #-}

module Day10.NaviSpec where

import qualified Data.Text as Text
import qualified Day10.Navi as Navi
import Test.Hspec

exampleInput :: [Text.Text]
exampleInput =
  [ "[({(<(())[]>[[{[]{<()<>>",
    "[(()[<>])]({[<{<<[]>>(",
    "{([(<{}[<>[]}>{[]{[(<()>",
    "(((({<>}<{<{<>}{[]{[]{}",
    "[[<[([]))<([[{}[[()]]]",
    "[{[{({}]{}}([{[{{{}}([]",
    "{<[[]]>}<{[{[{[]{()[[[]",
    "[<(<(<(<{}))><([]([]()",
    "<{([([[(<>()){}]>(<<{{",
    "<{([{{}}[<[[[<>{}]]]>[]]"
  ]

spec :: Spec
spec = do
  describe "example" $ do
    let syntax = Navi.parseInput exampleInput

    it "calculates syntax error score" $ do
      let syntaxErrorScore = Navi.syntaxErrorScore syntax
      syntaxErrorScore `shouldBe` 26397
    it "finds the middle auto complete score" $ do
      let autoCompleteScore = Navi.autoCompleteScore syntax
      autoCompleteScore `shouldBe` 288957

  describe "find syntax error" $ do
    it "returns the first invalid token" $ do
      Navi.checkSyntax "{([(<{}[<>[]}>{[]{[(<()>" `shouldBe` Navi.Error '}'
      Navi.checkSyntax "[[<[([]))<([[{}[[()]]]" `shouldBe` Navi.Error ')'
      Navi.checkSyntax "[{[{({}]{}}([{[{{{}}([]" `shouldBe` Navi.Error ']'
      Navi.checkSyntax "[<(<(<(<{}))><([]([]()" `shouldBe` Navi.Error ')'
      Navi.checkSyntax "<{([([[(<>()){}]>(<<{{" `shouldBe` Navi.Error '>'

  describe "find incomplete" $ do
    it "returns a stack of remaining unmatched tokens" $ do
      Navi.checkSyntax "[({(<(())[]>[[{[]{<()<>>" `shouldBe` Navi.Incomplete "{{[[({(["
      Navi.checkSyntax "[(()[<>])]({[<{<<[]>>(" `shouldBe` Navi.Incomplete "({<[{("
      Navi.checkSyntax "(((({<>}<{<{<>}{[]{[]{}" `shouldBe` Navi.Incomplete "{{<{<(((("
      Navi.checkSyntax "{<[[]]>}<{[{[{[]{()[[[]" `shouldBe` Navi.Incomplete "[[{{[{[{<"
      Navi.checkSyntax "<{([{{}}[<[[[<>{}]]]>[]]" `shouldBe` Navi.Incomplete "[({<"

  describe "score completion of one uncompleted line" $ do
    it "calculates the completion score" $ do
      Navi.scoreCompletion (Navi.Incomplete "{{[[({([") `shouldBe` (Just 288957)
      Navi.scoreCompletion (Navi.Incomplete "({<[{(") `shouldBe` (Just 5566)
      Navi.scoreCompletion (Navi.Incomplete "{{<{<((((") `shouldBe` (Just 1480781)
      Navi.scoreCompletion (Navi.Incomplete "[[{{[{[{<") `shouldBe` (Just 995444)
      Navi.scoreCompletion (Navi.Incomplete "[({<") `shouldBe` (Just 294)
