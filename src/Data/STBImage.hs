{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

{-|
Module      : Data.STBImage
Description : Image loading and writing based on stb_image and std_image_write
Copyright   : (c) Alexis Williams 2016
License     : BSD3
Maintainer  : sasinestro@gmail.com
Stability   : experimental

Much like the original library, the focus of this library is placed on ease of use rather than richness of feature set, thus the rather spartan interface.
-}
module Data.STBImage (
    -- * Data types
      Color(..)
    , Image(..)
    -- * Loading images
    , loadImage
    -- * Writing images
    , writePNG, writeBMP, writeTGA
    ) where

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
#if __GLASGOW_HASKELL__ <= 710
import           Data.Functor ((<$>))
#endif
import           Foreign
import           Foreign.C.Types
import           Foreign.C.String
import           Text.Printf

--

{-| 
    The 'Color' type is used to represented the colors that are read out of the image file.

    Currently, all images will be read out as RGBA, which is rather storage inefficient, but it's what I need for my application right now and a tagging scheme (to allow a Storable instance for the multiple-form datatype) would be a significant penalty to speed and reduce the space win significantly, and actively penalize the most common case for me.
-}
data Color = RGBA { _red :: Word8, _green :: Word8, _blue :: Word8, _alpha :: Word8 }
           deriving (Eq)
        --- | Greyscale { _greyscale :: Word8 }
        --- | GreyscaleAlpha { _greyscale :: Word8, _alpha :: Word8 }
        --- | RGB { _red :: Word8, _green :: Word8, _blue :: Word8 }

instance Show Color where
    show RGBA{..} = printf "(#%02X%02X%02X%02X)" _red _green _blue _alpha

instance Storable Color where
    sizeOf _ = 4
    alignment _ = 1
    peek ptr = do
        let ptr' = castPtr ptr :: Ptr Word8
        r <- peekElemOff ptr' 0
        g <- peekElemOff ptr' 1
        b <- peekElemOff ptr' 2
        a <- peekElemOff ptr' 3
        return $ RGBA r g b a
    poke ptr RGBA{..} = do
        let ptr' = castPtr ptr :: Ptr Word8
        pokeElemOff ptr' 0 _red
        pokeElemOff ptr' 1 _green
        pokeElemOff ptr' 2 _blue 
        pokeElemOff ptr' 3 _alpha

-- | 'Image' is the least opinionated reasonable type to represent an image, just a vector of pixel 'Color's (laid out top-to-bottom, left-to-right) and a size.
data Image = Image { _pixels :: V.Vector Color, _width :: Int, _height :: Int }
           deriving (Eq) 

instance Show Image where
    show (Image _ w h) = "Image (" ++ show w ++ "x" ++ show h ++ ")\n"

--

foreign import ccall "stb_image.h stbi_load" stbi_load :: CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> CInt -> IO (Ptr CUChar)
foreign import ccall "stb_image.h stbi_failure_reason" stbi_failure_reason :: IO (CString)
foreign import ccall "stb_image.h &stbi_image_free" stbi_image_free :: FunPtr (Ptr CUChar -> IO ())

{-|
    'loadImage' loads the image file at 'path' and returns either an error message from the underlying library or the 'Image' that is stored in the file.
-}
loadImage :: FilePath -> IO (Either String Image)
loadImage path = do
    cPath <- newCString path
    widthPtr <- new 0
    heightPtr <- new 0
    nComponentsPtr <- new 0

    dataPtr <- stbi_load cPath widthPtr heightPtr nComponentsPtr 4 -- forces output to be 4 bytepp layout

    case dataPtr /= nullPtr of
        True -> do
            dataForeignPtr <- newForeignPtr stbi_image_free dataPtr

            width  <- fromIntegral <$> peek widthPtr :: IO Int
            height <- fromIntegral <$> peek heightPtr :: IO Int

            let storage = V.unsafeFromForeignPtr0 dataForeignPtr (width * height * 4)

            free cPath
            free widthPtr
            free heightPtr
            free nComponentsPtr

            return $ Right (Image (V.unsafeCast storage :: V.Vector Color) width height)
        False -> do
            err <- peekCString =<< stbi_failure_reason
            return $ Left err

--

foreign import ccall "stb/stb_image_write.h stbi_write_png" stbi_write_png :: CString -> CInt -> CInt -> CInt -> Ptr CUChar -> CInt -> IO CInt
foreign import ccall "stb/stb_image_write.h stbi_write_bmp" stbi_write_bmp :: CString -> CInt -> CInt -> CInt -> Ptr CUChar         -> IO CInt
foreign import ccall "stb/stb_image_write.h stbi_write_tga" stbi_write_tga :: CString -> CInt -> CInt -> CInt -> Ptr CUChar         -> IO CInt

{-|
    'writePNG' writes the image passed to it out at the path 'path' in PNG format. The path must include the extension.
-}
writePNG :: FilePath -> Image -> IO ()
writePNG path (Image storage width height) = do
    cPath <- newCString path

    let w = fromIntegral width :: CInt
    let h = fromIntegral height :: CInt

    withForeignPtr (fst $ V.unsafeToForeignPtr0 storage) (\pixBuf ->
        stbi_write_png cPath w h 4 (castPtr pixBuf) (w * 4) -- bytes per row
        )

    free cPath

{-|
    'writeBMP' writes the image passed to it out at the path 'path' in BMP format. The path must include the extension.
-}
writeBMP :: FilePath -> Image -> IO ()
writeBMP path (Image storage width height) = do
    cPath <- newCString path

    let w = fromIntegral width :: CInt
    let h = fromIntegral height :: CInt

    withForeignPtr (fst $ V.unsafeToForeignPtr0 storage) $ stbi_write_bmp cPath w h 4 . castPtr

    free cPath

{-|
    'writeTGA' writes the image passed to it out at the path 'path' in TGA format. The path must include the extension.
-}
writeTGA :: FilePath -> Image -> IO ()
writeTGA path (Image storage width height) = do
    cPath <- newCString path

    let w = fromIntegral width :: CInt
    let h = fromIntegral height :: CInt

    withForeignPtr (fst $ V.unsafeToForeignPtr0 storage) $ stbi_write_tga cPath w h 4 . castPtr

    free cPath
