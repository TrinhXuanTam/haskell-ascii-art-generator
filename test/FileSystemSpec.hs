{-# LANGUAGE ScopedTypeVariables #-}

module FileSystemSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Utils.FileSystem

spec :: Spec
spec = do
  describe "getFileExtension" $ do
    it "returns Nothing for file paths without an extension" $
      property $ \(filePath :: FilePath) ->
        let noExtensionPath = filter (/= '.') filePath
        in getFileExtension noExtensionPath `shouldBe` Nothing

    it "returns the correct extension for file paths with an extension" $ do
      getFileExtension "test.txt" `shouldBe` Just "txt"
      getFileExtension "anotherTest.PDF" `shouldBe` Just "PDF"
      getFileExtension "./path/to/file.jpeg" `shouldBe` Just "jpeg"

  describe "fileExists" $ do
    it "returns True for existing files" $ do
      doesExist <- fileExists "./resources/test.png"
      doesExist `shouldBe` True

    it "returns False for non-existing files" $ do
      doesNotExist <- fileExists "./resources/nonExistentFile.xyz"
      doesNotExist `shouldBe` False