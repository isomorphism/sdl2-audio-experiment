module Main where

import SDL
import SDL.Audio
import Data.Monoid
import Data.Foldable (toList)
import Foreign.C.Types
import qualified Sound.File.Sndfile as Snd
import qualified Sound.File.Sndfile.Buffer.Vector as BV
import qualified Data.Vector.Storable as VS
import System.Environment
import Control.Concurrent
import Data.IORef
import Data.Int
import Data.Word
import Data.Bits
import Unsafe.Coerce

main :: IO ()
main = do 
    args <- getArgs
    let (fn, bufferSize) = case args of
                 [] -> error "no filename given"
                 [f] -> (f, 2048)
                 (f:b:_) -> (f, read b)
    
    (info, s) <- Snd.readFile fn
    let song :: BV.Buffer Int32
        song = case s of
                  Nothing -> error "failed to load song"
                  Just x -> x
    print info
    
    initialize [InitTimer, InitAudio]
    drs <- getAudioDrivers
    let dr = case toList drs of
                [] -> error "no drivers found"
                (x:_) -> x
    audioInit dr
    dvs <- getAudioDeviceNames ForPlayback
    
    let dv = case maybe [] toList dvs of
                [] -> error "no devices found"
                (x:_) -> x
    print dv
    
    pos <- newIORef 0
    let songData = BV.fromBuffer song
    let callback i = do
        p <- readIORef pos
        print (p, i, VS.length songData)
        writeIORef pos $ p + fromIntegral i `div` sampleBytes song
        if p > VS.length songData
          then do return . VS.convert $ VS.replicate (fromIntegral i) 0
          else if p + fromIntegral i > VS.length songData
          then do return . VS.convert . VS.unsafeCast 
                        $ VS.slice p (VS.length songData - p) songData <> VS.replicate (fromIntegral i - (VS.length songData - p)) 0
          else return . VS.convert . VS.unsafeCast $ VS.slice p (fromIntegral i) songData

    
    (adv, _) <- openAudioDevice $ 
        OpenDeviceSpec (Desire . fromIntegral $ Snd.samplerate info) 
                       (Desire $ getAudioFormat song (Snd.endianFormat (Snd.format info) == Snd.EndianBig))
                       (Desire $ if Snd.channels info == 2 then Stereo else Mono)
                       bufferSize
                       callback
                       ForPlayback
                       Nothing
    setAudioDevicePlaybackState adv Play
    
    putStrLn "Playing..."
    
    delay (fromIntegral (Snd.frames info `div` Snd.samplerate info) * 1000 + 500)
    return ()

signedFlag, bigendFlag, floatFlag :: Word16
signedFlag = 1 `shiftL` 15
bigendFlag = 1 `shiftL` 12
floatFlag = 1 `shiftL` 8

bigend True = bigendFlag
bigend False = 0

class AudioSample a where
    getAudioFormat :: proxy a -> Bool -> AudioFormat
    sampleBytes :: proxy a -> Int

instance AudioSample Word16 where
    getAudioFormat _ be = unsafeCoerce $ 16 + bigend be
    sampleBytes = const 2

instance AudioSample Word32 where
    getAudioFormat _ be = unsafeCoerce $ 32 + bigend be
    sampleBytes = const 4

instance AudioSample Int16 where
    getAudioFormat _ be = unsafeCoerce $ 16 + signedFlag + bigend be
    sampleBytes = const 2

instance AudioSample Int32 where
    getAudioFormat _ be = unsafeCoerce $ 32 + signedFlag + bigend be
    sampleBytes = const 4

instance AudioSample Float where
    getAudioFormat _ be = unsafeCoerce $ 32 + signedFlag + bigend be + floatFlag
    sampleBytes = const 4

