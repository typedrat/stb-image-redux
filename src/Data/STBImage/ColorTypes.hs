{-# LANGUAGE RecordWildCards #-}
module Data.STBImage.ColorTypes (YColor(..), YAColor(..), RGBColor(..), RGBAColor(..)) where

import           Foreign
import           Foreign.C.Types

data YColor = YColor { _yGreyscale :: Word8 }
            deriving (Eq)

instance Storable YColor where
    sizeOf _ = 1
    alignment _ = 1
    peek ptr = do
        let ptr' = castPtr ptr :: Ptr Word8
        y <- peek ptr'
        return $ YColor y
    poke ptr (YColor y) = poke (castPtr ptr :: Ptr Word8) y 

data YAColor = YAColor { _yaGreyscale :: Word8, _yaAlpha :: Word8 }
             deriving (Eq)

instance Storable YAColor where
    sizeOf _ = 2
    alignment _ = 1
    peek ptr = do
        let ptr' = castPtr ptr :: Ptr Word8
        y <- peek ptr'
        a <- peekElemOff ptr' 1
        return $ YAColor y a
    poke ptr (YAColor y a) = do
        let ptr' = castPtr ptr :: Ptr Word8
        pokeElemOff ptr' 0 y
        pokeElemOff ptr' 1 a

data RGBColor = RGBColor { _rgbRed :: Word8, _rgbGreen :: Word8, _rgbBlue :: Word8 }
               deriving (Eq)

instance Storable RGBColor where
    sizeOf _ = 3
    alignment _ = 1
    peek ptr = do
        let ptr' = castPtr ptr :: Ptr Word8
        r <- peekElemOff ptr' 0
        g <- peekElemOff ptr' 1
        b <- peekElemOff ptr' 2
        return $ RGBColor r g b
    poke ptr (RGBColor r g b) = do
        let ptr' = castPtr ptr :: Ptr Word8
        pokeElemOff ptr' 0 r
        pokeElemOff ptr' 1 g
        pokeElemOff ptr' 2 b

data RGBAColor = RGBAColor { _rgbaRed :: Word8, _rgbaGreen :: Word8, _rgbaBlue :: Word8, _rgbaAlpha :: Word8 }
               deriving (Eq)

instance Storable RGBAColor where
    sizeOf _ = 4
    alignment _ = 1
    peek ptr = do
        let ptr' = castPtr ptr :: Ptr Word8
        r <- peekElemOff ptr' 0
        g <- peekElemOff ptr' 1
        b <- peekElemOff ptr' 2
        a <- peekElemOff ptr' 3
        return $ RGBAColor r g b a
    poke ptr (RGBAColor r g b a) = do
        let ptr' = castPtr ptr :: Ptr Word8
        pokeElemOff ptr' 0 r
        pokeElemOff ptr' 1 g
        pokeElemOff ptr' 2 b
        pokeElemOff ptr' 3 a
