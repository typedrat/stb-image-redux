{-# LANGUAGE FlexibleInstances, RecordWildCards, TypeFamilies, UndecidableInstances #-}
module Data.STBImage.Color (Color(..), ColorFlag(..), YColor(..), YAColor(..), RGBColor(..), RGBAColor(..)) where

import Data.Bifunctor
import Data.Word
import Data.STBImage.ColorTypes
import Data.STBImage.Immutable
import qualified Data.Vector.Storable as V
import Text.Printf

--

class Color a where
    data ColorFlag a :: *
    -- | 'loadImage' reads the image (with ColorFlag 'Y', 'YA', 'RGB', or 'RGBA') at the supplied path.
    loadImage :: ColorFlag a -> FilePath -> IO (Either String (Image a))
    -- | 'writePNG' writes the image passed to it out at the path 'path' in PNG format. The path must include the extension.
    writePNG  :: FilePath -> Image a -> IO ()
    -- | 'writeBMP' writes the image passed to it out at the path 'path' in BMP format. The path must include the extension.
    writeBMP  :: FilePath -> Image a -> IO ()
    -- | 'writeTGA' writes the image passed to it out at the path 'path' in TGA format. The path must include the extension.
    writeTGA  :: FilePath -> Image a -> IO ()

    red :: a -> Word8
    green :: a -> Word8
    blue :: a -> Word8
    alpha :: a -> Word8

instance Color YColor where
    data ColorFlag YColor = Y
    loadImage Y = fmap (second unsafeCastImage) . loadImageBytes 1
    writePNG = writeNChannelPNG 1
    writeBMP = writeNChannelBMP 1
    writeTGA = writeNChannelTGA 1

    red   (YColor y) = y
    green (YColor y) = y
    blue  (YColor y) = y
    alpha _          = 255

instance {-# OVERLAPS #-} Show (ColorFlag YColor) where
    show _ = "Y"

instance Color YAColor where
    data ColorFlag YAColor = YA
    loadImage YA = fmap (second unsafeCastImage) . loadImageBytes 2
    writePNG = writeNChannelPNG 2
    writeBMP = writeNChannelBMP 2
    writeTGA = writeNChannelTGA 2

    red   (YAColor y _) = y
    green (YAColor y _) = y
    blue  (YAColor y _) = y
    alpha (YAColor _ a) = a

instance {-# OVERLAPS #-} Show (ColorFlag YAColor) where
    show _ = "YA"

instance Color RGBColor where
    data ColorFlag RGBColor = RGB
    loadImage RGB = fmap (second unsafeCastImage) . loadImageBytes 3
    writePNG = writeNChannelPNG 3
    writeBMP = writeNChannelBMP 3
    writeTGA = writeNChannelTGA 3

    red   (RGBColor r _ _) = r
    green (RGBColor _ g _) = g
    blue  (RGBColor _ _ b) = b
    alpha _                = 255

instance {-# OVERLAPS #-} Show (ColorFlag RGBColor) where
    show _ = "RGB"

instance Color RGBAColor where
    data ColorFlag RGBAColor = RGBA
    loadImage RGBA = fmap (second unsafeCastImage) . loadImageBytes 4
    writePNG = writeNChannelPNG 4
    writeBMP = writeNChannelBMP 4
    writeTGA = writeNChannelTGA 4

    red   (RGBAColor r _ _ _) = r
    green (RGBAColor _ g _ _) = g
    blue  (RGBAColor _ _ b _) = b
    alpha (RGBAColor _ _ _ a) = a

instance {-# OVERLAPS #-} Show (ColorFlag RGBAColor) where
    show _ = "RGBA"

instance {-# OVERLAPS #-} (Color a) => Show a where
    show color = printf "(#%02X%02X%02X%02X)" (red color) (green color) (blue color) (alpha color)
