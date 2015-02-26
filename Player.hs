{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
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
import Control.Monad
import Control.Concurrent
import Control.Applicative
import Data.IORef
import Data.Int
import Data.Word
import Data.Bits
import qualified Data.Text as T
import Data.Maybe
import Linear
import Unsafe.Coerce

main :: IO ()
main = do 
    args <- getArgs
    let bufferSize = 1024
    let (fn1, fn2) = case args of
                 [f1, f2] -> (f1, f2)
                 [] -> error "bad arguments"
    
    (info1, Just snd1) <- Snd.readFile fn1 
    (info2, Just snd2) <- Snd.readFile fn2
    
    --  let song :: BV.Buffer Int32
        --  song = case s of
                  --  Nothing -> error "failed to load song"
                  --  Just x -> x
    print info1
    print info2
    
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
    
    window <- SDL.createWindow (T.pack "Drum") SDL.defaultWindow { SDL.windowSize = V2 200 200 }
    SDL.showWindow window
    screenSurface <- SDL.getWindowSurface window
    
    let drum1, drum2 :: IO (IORef Int, VS.Vector Int32) 
        drum1 = do
            p <- newIORef 0
            putStrLn "1"
            return (p, BV.fromBuffer snd1)
        drum2 = do
            p <- newIORef 0
            putStrLn "2"
            return (p, BV.fromBuffer snd2)
    
    let drummit SDL.KeyboardEvent{..} | keyboardEventKeyMotion == SDL.KeyDown =
                    case SDL.keysymKeycode keyboardEventKeysym of
                        SDL.KeycodeSpace  -> Just <$> drum1
                        SDL.KeycodeReturn -> Just <$> drum2
                        _ -> return Nothing
        drummit _ = return Nothing
    
    --  let songData = BV.fromBuffer song
    --  pos <- newIORef 0
    sounds <- newIORef [] :: IO (IORef [(IORef Int, VS.Vector Int32)])
    
    let play i (pos, dat) = do
        p <- readIORef pos
        print (p, i, VS.length dat)
        writeIORef pos $ p + fromIntegral i `div` sampleBytes snd1
        if p > VS.length dat
          then do return Nothing -- . VS.convert $ VS.replicate (fromIntegral i) 0
          else if p + fromIntegral i > VS.length dat
          then do return . Just $ VS.slice p (VS.length dat - p) dat <> VS.replicate (fromIntegral i - (VS.length dat - p)) 0
          else return . Just $ VS.slice p (fromIntegral i) dat
    
    let callback i = do
        let smps = fromIntegral i `div` sampleBytes snd1
        
        snds <- readIORef sounds
        out <- catMaybes <$> mapM (play i) snds
        return . VS.convert . VS.unsafeCast $ (foldr (VS.zipWith (+)) (VS.replicate smps 0) (VS.map (`div` 4) <$> out) :: VS.Vector Int32)
        --  p <- readIORef pos
        --  print (p, i, VS.length songData)
        --  writeIORef pos $ p + fromIntegral i `div` sampleBytes song
        --  if p > VS.length songData
          --  then do return . VS.convert $ VS.replicate (fromIntegral i) 0
          --  else if p + fromIntegral i > VS.length songData
          --  then do return . VS.convert . VS.unsafeCast 
                        --  $ VS.slice p (VS.length songData - p) songData <> VS.replicate (fromIntegral i - (VS.length songData - p)) 0
          --  else return . VS.convert . VS.unsafeCast $ VS.slice p (fromIntegral i) songData

    
    (adv, _) <- openAudioDevice $ 
        OpenDeviceSpec (Desire . fromIntegral $ Snd.samplerate info1) 
                       (Desire $ getAudioFormat snd1 (Snd.endianFormat (Snd.format info1) == Snd.EndianBig))
                       (Desire $ if Snd.channels info1 == 2 then Stereo else Mono)
                       bufferSize
                       callback
                       ForPlayback
                       Nothing
    setAudioDevicePlaybackState adv Play
    
    let loop = do
            events <- map SDL.eventPayload <$> collectEvents
            let q = any (== SDL.QuitEvent) events
            newSnds <- catMaybes <$> mapM drummit events
            curSnds <- readIORef sounds
            curSnds' <- filterM (\(pos, dat) -> (< VS.length dat) <$> readIORef pos) curSnds
            --  print $ length $ curSnds' ++ newSnds
            writeIORef sounds $ curSnds' ++ newSnds
            SDL.updateWindowSurface window

            unless q loop

    loop
    SDL.destroyWindow window
    SDL.quit
    
    --  putStrLn "Playing..."
    
    --  delay (fromIntegral (Snd.frames info `div` Snd.samplerate info) * 1000 + 500)
    --  return ()

signedFlag, bigendFlag, floatFlag :: Word16
signedFlag = 1 `shiftL` 15
bigendFlag = 1 `shiftL` 12
floatFlag = 1 `shiftL` 8

collectEvents = do
            e <- SDL.pollEvent
            case e of
              Nothing -> return []
              Just e' -> (e' :) <$> collectEvents

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

