-----------------------------------------------------------------------------
--
-- Module      :  Adaptive
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification, DoAndIfThenElse, FlexibleInstances, FlexibleContexts #-}

module Adaptive where

import Data.IORef
import Control.Monad
import System.IO.Unsafe
import Data.Typeable


data AdaptiveValue m r a = AdaptiveValue { adaptiveId :: Int, cache :: r a, outOfDate :: r Bool, level :: r Int, compute :: m a, outputs :: r [IAdaptiveValue m r] }

data IAdaptiveValue m r = forall a. IAdaptiveValue (AdaptiveValue m r a)


instance Eq (AdaptiveValue m r a) where
    a == b = adaptiveId a == adaptiveId b

instance Eq (IAdaptiveValue m r) where
    (IAdaptiveValue a) == (IAdaptiveValue b) = adaptiveId a == adaptiveId b

modId :: IORef Int
modId = unsafePerformIO $ newIORef 0

newId = do
    atomicModifyIORef modId (\x -> (x+1,x+1))

mark' :: IAdaptiveValue IO IORef -> IO ()
mark' (IAdaptiveValue v) = do
    let r = outOfDate v
    writeIORef r True
    o <- readIORef $ outputs v
    mapM_ mark' o

mark = mark' . IAdaptiveValue

addOutput i o = modifyIORef (outputs i) (\old -> (IAdaptiveValue o):old)
addOutput' (IAdaptiveValue i) o = modifyIORef (outputs i) (\old -> o:old)


removeOutput i o = modifyIORef (outputs i) (filter (\a -> a /= (IAdaptiveValue o)))

initMod :: a -> IO (AdaptiveValue IO IORef a)
initMod value = do
    c <- newIORef value
    outOfDate <- newIORef True
    op <- newIORef []
    level <- newIORef 0
    let compute = readIORef c
    id <- newId


    return $ AdaptiveValue { adaptiveId = id, cache = c, outOfDate = outOfDate, level = level, compute = compute, outputs = op }

getValue m = do
    o <- readIORef $ outOfDate m
    if o then do
        c <- compute m
        writeIORef (cache m) c
        writeIORef (outOfDate m) False
        return c
    else
        readIORef (cache m)

change m v = do
    let r = cache m
    writeIORef r v
    mark m

mapMod f m = unsafePerformIO $ do
    c <- newIORef undefined
    outOfDate <- newIORef True
    op <- newIORef []
    l <- readIORef $ level m
    level <- newIORef (l + 1)
    let compute = do
        i <- getValue m
        return $ f i
    id <- newId

    let res = AdaptiveValue { adaptiveId = id, cache = c, outOfDate = outOfDate, level = level, compute = compute, outputs = op }
    addOutput m res

    return res

fromJust :: Maybe a -> a
fromJust (Just v) = v
fromJust Nothing = error "not just"

bindMod :: (a -> AdaptiveValue IO IORef b) -> (AdaptiveValue IO IORef a) -> AdaptiveValue IO IORef b
bindMod f m = unsafePerformIO $ do
    c <- newIORef (error "empty cache")
    outOfDate <- newIORef True
    op <- newIORef []
    l <- readIORef $ level m
    level <- newIORef (l + 1)
    inner <- newIORef Nothing
    self <- newIORef (error "empty self-reference")
    let compute = do
        i <- getValue m
        old <- readIORef inner
        self' <- readIORef self

        case old of
            Just o ->
                let r = f i in
                if r == o then
                    return ()
                else do
                    addOutput r self'
                    removeOutput o self'
                    writeIORef inner (Just r)
            Nothing ->
                let r = f i in
                addOutput r self' >> writeIORef inner (Just r)

        v <- readIORef inner
        getValue $ fromJust v

    id <- newId
    let res = AdaptiveValue { adaptiveId = id, cache = c, outOfDate = outOfDate, level = level, compute = compute, outputs = op }
    addOutput m res
    writeIORef self res
    return res



instance Functor (AdaptiveValue IO IORef) where
    fmap = mapMod

instance Monad (AdaptiveValue IO IORef) where
    return = unsafePerformIO . initMod
    m >>= f = bindMod f m


