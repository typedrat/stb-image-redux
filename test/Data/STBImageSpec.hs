module Data.STBImageSpec (spec) where

import qualified Data.Vector.Storable as V
import           Test.Hspec

import           Data.STBImage

firstTenPixels :: V.Vector RGBColor
firstTenPixels = V.fromList [
      RGBColor 0x8B 0x84 0x5A
    , RGBColor 0x8D 0x88 0x5C
    , RGBColor 0x8E 0x83 0x59
    , RGBColor 0x8F 0x86 0x5B
    , RGBColor 0x8E 0x8B 0x5B
    , RGBColor 0x8F 0x8A 0x5E
    , RGBColor 0x92 0x8C 0x5F
    , RGBColor 0x93 0x8C 0x5E
    , RGBColor 0x93 0x8C 0x5D
    , RGBColor 0x94 0x8C 0x5F
    ]

spec :: Spec
spec = do
    describe "image loader" $ do
        it "loads the first ten pixels correctly" $ do
            Right img <- loadImage RGB "test/jellybeans.tga"
            V.take 10 (_pixels img) `shouldBe` firstTenPixels
        it "loads the same image in different formats" $ do
            Right img  <- loadImage RGBA "test/jellybeans.bmp"
            Right img' <- loadImage RGBA "test/jellybeans.tga"
            img `shouldBe` img'
        it "flips an image correctly" $ do
            Right img  <- loadImage RGBA "test/jellybeans.bmp"
            Right img' <- loadImage RGBA "test/jellybeans-flipped.bmp"
            flipImage img `shouldBe` img'
    describe "image writer" $ do
        it "works idempotently" $ do
            Right img <- loadImage RGBA "test/jellybeans.tga"
            writeTGA "test/jellybeans-out.tga" img
            Right img' <- loadImage RGBA "test/jellybeans-out.tga"
            img `shouldBe` img'
        it "saves and reloads lossless images correctly across formats" $ do
            Right img <- loadImage RGBA "test/jellybeans.tga"
            writeBMP "test/jellybeans-out.bmp" img
            Right img' <- loadImage RGBA "test/jellybeans-out.bmp"
            img `shouldBe` img'
        it "saves and reloads PNG files correctly" $ do
            Right img <- loadImage RGBA "test/jellybeans.tga"
            writePNG "test/jellybeans-out.png" img
            Right img' <- loadImage RGBA "test/jellybeans-out.png"
            img `shouldBe` img'
