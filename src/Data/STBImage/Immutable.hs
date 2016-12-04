{-# LANGUAGE CPP             #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
module Data.STBImage.Immutable (Image(..), unsafeCastImage, flipImage, loadImageBytes, writeNChannelPNG, writeNChannelBMP, writeNChannelTGA) where

import           Data.Either
import           Data.List
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as MV
import           Foreign
import           Foreign.C.String
import           Foreign.C.Types
import           GHC.Generics

import           Data.STBImage.ColorTypes


-- | 'Image' is the least opinionated reasonable type to represent an image, just a vector of pixel 'Color's (laid out top-to-bottom, left-to-right) and a size.
data Image a = Image { _pixels :: V.Vector a, _width :: Int, _height :: Int }
           deriving (Eq, Generic)

instance Show (Image a) where
    show (Image _ w h) = "Image (" ++ show w ++ "x" ++ show h ++ ")"

unsafeCastImage :: (Storable a, Storable b) => Image a -> Image b
unsafeCastImage img@Image{ _pixels = _pixels } = img { _pixels = V.unsafeCast _pixels }

--

-- | Utility function to flip images, e.g. for use with OpenGL
flipImage :: (Storable a) => Image a -> Image a
flipImage img@Image{..} = img { _pixels = V.concat . reverse . toRows $ _pixels }
    where
        toRows :: (Storable a) => V.Vector a -> [V.Vector a]
        toRows = unfoldr (\v -> if V.null v then Nothing else Just $ V.splitAt _width v)
--

foreign import ccall "stb_image.h stbi_load" stbi_load :: CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> CInt -> IO (Ptr CUChar)
foreign import ccall "stb_image.h stbi_failure_reason" stbi_failure_reason :: IO (CString)
foreign import ccall "stb_image.h &stbi_image_free" stbi_image_free :: FunPtr (Ptr CUChar -> IO ())

loadImageBytes :: Int -> FilePath -> IO (Either String (Image CUChar))
loadImageBytes comps path = do
    cPath <- newCString path
    widthPtr <- new 0
    heightPtr <- new 0
    nComponentsPtr <- new 0

    dataPtr <- stbi_load cPath widthPtr heightPtr nComponentsPtr (fromIntegral comps)

    if dataPtr /= nullPtr
        then do
            dataForeignPtr <- newForeignPtr stbi_image_free dataPtr

            _width  <- fromIntegral <$> peek widthPtr :: IO Int
            _height <- fromIntegral <$> peek heightPtr :: IO Int

            let _pixels = V.unsafeFromForeignPtr0 dataForeignPtr (_width * _height * comps)

            free cPath
            free widthPtr
            free heightPtr
            free nComponentsPtr

            return $ Right Image{..}
        else do
            err <- peekCString =<< stbi_failure_reason
            return $ Left err

--

foreign import ccall "stb/stb_image_write.h stbi_write_png" stbi_write_png :: CString -> CInt -> CInt -> CInt -> Ptr CUChar -> CInt -> IO CInt
foreign import ccall "stb/stb_image_write.h stbi_write_bmp" stbi_write_bmp :: CString -> CInt -> CInt -> CInt -> Ptr CUChar         -> IO CInt
foreign import ccall "stb/stb_image_write.h stbi_write_tga" stbi_write_tga :: CString -> CInt -> CInt -> CInt -> Ptr CUChar         -> IO CInt

writeNChannelPNG :: (Storable a) => CInt -> FilePath -> Image a -> IO ()
writeNChannelPNG comps path Image{..} = do
    cPath <- newCString path

    let w = fromIntegral _width :: CInt
    let h = fromIntegral _height :: CInt

    withForeignPtr (fst $ V.unsafeToForeignPtr0 _pixels) (\pixBuf ->
        stbi_write_png cPath w h comps (castPtr pixBuf) (w * comps) -- bytes per row
        )

    free cPath

writeNChannelBMP :: (Storable a) => CInt -> FilePath -> Image a -> IO ()
writeNChannelBMP comps path Image{..} = do
    cPath <- newCString path

    let w = fromIntegral _width :: CInt
    let h = fromIntegral _height :: CInt

    withForeignPtr (fst $ V.unsafeToForeignPtr0 _pixels) $ stbi_write_bmp cPath w h comps . castPtr

    free cPath


writeNChannelTGA :: (Storable a) => CInt -> FilePath -> Image a -> IO ()
writeNChannelTGA comps path Image{..} = do
    cPath <- newCString path

    let w = fromIntegral _width :: CInt
    let h = fromIntegral _height :: CInt

    withForeignPtr (fst $ V.unsafeToForeignPtr0 _pixels) $ stbi_write_tga cPath w h comps . castPtr

    free cPath
