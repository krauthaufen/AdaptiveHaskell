{-# LANGUAGE CPP, TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
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
{-# LANGUAGE MultiParamTypeClasses, ExistentialQuantification, ScopedTypeVariables,
             DoAndIfThenElse, FlexibleInstances, FlexibleContexts, TypeFamilies, NoMonomorphismRestriction #-}

module Main (
    main
) where

--import Adaptive
import Data.IORef
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Ref
import Data.Set
import System.IO.Unsafe
import Debug.Trace
import Unsafe.Coerce

{-# NOINLINE newId #-}{-# NOINLINE currentId #-}

currentId = unsafePerformIO $ newRef 0
newId () = unsafePerformIO $ do
    r <- readIORef currentId
    writeRef currentId (r+1)
    return r


class (MonadRef m) => Adaptive m r where
    outOfDateMod :: r m a -> Ref m Bool
    computeMod   :: r m a -> m a
    cacheMod     :: r m a -> Ref m a
    idMod        :: r m a -> Int
    depthMod     :: r m a -> Ref m Int
    newMod       :: a -> m (r m a)
    mapMod       :: (a -> b) -> r m a -> m (r m b)
    bindMod      :: (a -> m (r m b)) -> r m a -> m (r m b)
    outputsMod   :: r m a -> Ref m (Set (IAdaptive m))

data IAdaptive m = forall r a. (Adaptive m r) => IAdaptive (r m a)

newtype ModT r m a = ModT { runModT :: m (r m a) }

instance (MonadRef m, Adaptive m r) => Monad (ModT r m) where
    return v = ModT $ newMod v
    m >>= f =
        ModT $ do
            m' <- runModT m
            bindMod (runModT . f) m'




instance (Adaptive m r) => Eq (r m a) where
    a == b = idMod a == idMod b

instance (Adaptive m r) => Ord (r m a) where
    compare a b = compare (idMod a) (idMod b)

instance Eq (IAdaptive m) where
    (IAdaptive a) == (IAdaptive b) = idMod a == idMod b

instance Ord (IAdaptive m) where
    compare (IAdaptive a) (IAdaptive b) = compare (idMod a) (idMod b)

relabel' min (IAdaptive o) = do
    let r = depthMod o
    d <- readRef r
    if d > min then return ()
    else do
        writeRef r (min)
        o' <- readRef $ outputsMod o
        mapM_ (relabel' (min + 1)) (toAscList o')

relabel min m =
    relabel' min (IAdaptive m)


addOutput' :: (Adaptive m r, MonadRef m) => r m a -> IAdaptive m -> m ()
addOutput' i o = modifyRef (outputsMod i) (insert o)

addOutput i o = addOutput' i (IAdaptive o)

removeOutput' :: (Adaptive m r, MonadRef m) => r m a -> IAdaptive m -> m ()
removeOutput' i o = modifyRef (outputsMod i) (delete o)

removeOutput i o = addOutput' i (IAdaptive o)


data Computation m a = Comp { compId :: Int
                            , compDepth :: Ref m Int
                            , compCache :: Ref m a
                            , compCompute :: m a
                            , compOutOfDate :: Ref m Bool
                            , compOutputs :: Ref m (Set (IAdaptive m)) }

createMod :: (MonadRef m) => (m a) -> (m Int) -> m (Computation m a)
createMod compute depth = do
    let id = newId()
    d <- depth
    depth <- newRef d
    v <- compute
    cache <- newRef v
    outDated <- newRef False
    op <- newRef empty
    return $ Comp { compId = id, compDepth = depth, compCache = cache, compCompute = compute, compOutOfDate = outDated, compOutputs = op }

initMod :: (MonadRef m) => a -> m (Computation m a)
initMod cacheValue = do
    let id = newId()
    depth <- newRef 0
    cache <- newRef cacheValue
    let compute = readRef cache
    outDated <- newRef False
    op <- newRef empty
    return $ Comp { compId = id, compDepth = depth, compCache = cache, compCompute = compute, compOutOfDate = outDated, compOutputs = op }


forceMod m = do
    o <- readRef $ outOfDateMod m
    if o then do
        v <- computeMod m
        --trace ("aa: " ++ (show $ (unsafeCoerce v :: Int))) (return ())
        writeRef (cacheMod m) v
        writeRef (outOfDateMod m) False
        return v
    else readRef (cacheMod $ m)

instance MonadRef m => Adaptive m Computation where
    cacheMod = compCache
    computeMod = compCompute
    outOfDateMod = compOutOfDate
    idMod = compId
    depthMod = compDepth
    newMod v = initMod v
    mapMod f i = createMod (do v <- forceMod i; return $ f v) (return 1)
    bindMod = undefined
    outputsMod = compOutputs


instance Adaptive m r => Functor (ModT r m) where
    fmap f m =
        let i  = unsafePerformIO $ unsafeCoerce ( runModT m) in
        ModT $ mapMod f i

foosada x = newMod x

unsafeToIO :: m (r m a) -> IO (r m a)
unsafeToIO m = unsafeCoerce m

new :: (MonadRef m, Adaptive m r) => a -> (ModT r m a)
new v =
    let a = unsafePerformIO $ unsafeToIO (newMod v) in
    ModT $ return a

force :: Adaptive m r => ModT r m a -> m a
force m = do
    m' <- runModT m

    forceMod m'


printMod m = force m >>= print

markOutdatedMod' (IAdaptive m) = do
    o <- readRef (outOfDateMod m)
    if o then do
        return ()
    else do
        o <- readRef $ outputsMod m
        mapM_ markOutdatedMod' (toAscList o)

markOutdatedMod m = markOutdatedMod' (IAdaptive m)


change m v = do
    c <- runModT m
    writeRef (compCache c) v
    markOutdatedMod c


main :: IO ()
main = do
    let a = new 10 :: ModT Computation IO Int
    let b = fmap (*3) a

    print "initial"
    v <- force b
    printMod b

    print "a = 100"
    change a 100
    printMod b

    change a 8
    printMod b

    print "asdasd"





{-
printMod m = getValue m >>= print

-- define a simple monadic multiplication
-- which is equivalent to our F#-adaptive-blocks
modMul ma mb = do
    a <- ma
    b <- mb
    return $ a * b




main = do
    a <- initMod 10
    x <- initMod 5

    let c = modMul a x

    printMod c
    change x 100
    printMod c

    let b = a >>= return . (*4)
    printMod b


    change a 12
    printMod b


    change a 5
    printMod b
-}

