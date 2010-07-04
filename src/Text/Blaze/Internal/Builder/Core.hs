-- | The builder monoid from BlazeHtml.
--
{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Text.Blaze.Internal.Builder.Core
    ( 
      -- * Main builder type
      Builder (..)

      -- * ByteArray type
    , ByteArray
    , makeByteArray

      -- * Custom writes to the builder
    , Write (..)
    , writeByte
    , writeByteString
    , writeByteArray
    , writeSingleton
    , writeList

      -- * Creating builders
    , singleton
    , copyByteString
    , fromByteArray

      -- * Extracting the result from a builder
    , toLazyByteString
    ) where

import Foreign
import Data.Monoid (Monoid, mempty, mappend, mconcat)
import qualified Data.ByteString.Char8 ()
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy as L

-- | Main builder type.
--
newtype Builder = Builder
    { -- ^ Extract the data
      unBuilder :: BuildStep -> BuildStep
    }

-- | A buildsignal is a signal returned from a write to the builder, it tells us
-- what should happen next.
--
data BuildSignal
  -- | Signal the completion of the write process.
  = Done {-# UNPACK #-} !(Ptr Word8)  -- ^ Pointer to the next free byte
  -- | Signal that the buffer is full and a new one needs to be allocated.
  | BufferFull
      {-# UNPACK #-} !Int          -- ^ Minimal size required for next buffer
      {-# UNPACK #-} !(Ptr Word8)  -- ^ Pointer to the next free byte
      {-# UNPACK #-} !BuildStep    -- ^ Continuation

-- | Type for a single build step. Every build step checks that
--
-- > free + bytes-written <= last
--
type BuildStep =  Ptr Word8       -- ^ Ptr to the next free byte in the buffer
               -> Ptr Word8       -- ^ Ptr to the first byte AFTER the buffer
               -> IO BuildSignal  -- ^ Signal the next step to be taken

instance Monoid Builder where
    mempty = Builder id
    {-# INLINE mempty #-}
    mappend (Builder f) (Builder g) = Builder $ f . g
    {-# INLINE mappend #-}
    mconcat = foldr mappend mempty
    {-# INLINE mconcat #-}

-- | A simple byte array.
--
data ByteArray = ByteArray
    {-# UNPACK #-} !(Ptr Word8)  -- ^ Start byte
    {-# UNPACK #-} !Int          -- ^ Length

-- | Create a 'ByteArray' from a 'S.ByteString'
--
makeByteArray :: S.ByteString  -- ^ Bytestring to unwrap
              -> ByteArray     -- ^ Resulting byte array
makeByteArray bs = ByteArray (ptr `plusPtr` o) l
  where
    (fptr, o, l) = S.toForeignPtr bs
    ptr = unsafeForeignPtrToPtr fptr

-- | Write abstraction so we can avoid some gory and bloody details.
--
data Write = Write
    {-# UNPACK #-} !Int   -- ^ Exact size of the write, in bytes
    (Ptr Word8 -> IO ())  -- ^ Function to carry out the write

-- A monoid interface for the write actions.
instance Monoid Write where
    mempty = Write 0 (const $ return ())
    {-# INLINE mempty #-}
    mappend (Write l1 f1) (Write l2 f2) = Write (l1 + l2) $ \ptr -> do
        f1 ptr
        f2 (ptr `plusPtr` l1)
    {-# INLINE mappend #-}

-- | Write a single byte.
--
writeByte :: Word8  -- ^ Byte to write
          -> Write  -- ^ Resulting write
writeByte x = Write 1 (\pf -> poke pf x)
{-# INLINE writeByte #-}

-- | Write a strict 'S.ByteString'.
--
writeByteString :: S.ByteString  -- ^ 'S.ByteString' to write
                -> Write         -- ^ Resulting write
writeByteString bs = Write l io
  where
  (fptr, o, l) = S.toForeignPtr bs
  io pf = withForeignPtr fptr $ \p -> copyBytes pf (p `plusPtr` o) l
{-# INLINE writeByteString #-}

-- | Write a 'ByteArray'
--
writeByteArray :: ByteArray  -- ^ 'ByteArray' to write.
               -> Write      -- ^ Resulting write
writeByteArray (ByteArray ptr l) = Write l $ \dst -> copyBytes ptr dst l
{-# INLINE writeByteArray #-}

-- | Construct a 'Builder' from a single 'Write' abstraction.
--
writeSingleton :: (a -> Write)  -- ^ 'Write' abstraction
               -> a             -- ^ Actual value to write
               -> Builder       -- ^ Resulting 'Builder'
writeSingleton write = makeBuilder
  where 
    makeBuilder x = Builder step
      where
        step k pf pe
          | pf `plusPtr` size <= pe = do
              io pf
              let pf' = pf `plusPtr` size
              pf' `seq` k pf' pe
          | otherwise               = return $ BufferFull size pf (step k)
          where
            Write size io = write x
{-# INLINE writeSingleton #-}

-- | Construct a builder writing a list of data from a write abstraction.
--
writeList :: (a -> Write)  -- ^ 'Write' abstraction
          -> [a]           -- ^ List of values to write
          -> Builder       -- ^ Resulting 'Builder'
writeList write = makeBuilder
  where
    makeBuilder []  = mempty
    makeBuilder xs0 = Builder $ step xs0
      where
        step xs1 k pf0 pe0 = go xs1 pf0
          where
            go []          !pf = k pf pe0
            go xs@(x':xs') !pf
              | pf `plusPtr` size <= pe0  = do
                  io pf
                  go xs' (pf `plusPtr` size)
              | otherwise = do return $ BufferFull size pf (step xs k)
              where
                Write size io = write x'
{-# INLINE writeList #-}

-- | Construct a 'Builder' from a single byte.
--
singleton :: Word8    -- ^ Byte to create a 'Builder' from
          -> Builder  -- ^ Resulting 'Builder'
singleton = writeSingleton writeByte

-- | /O(n)./ A Builder taking a 'S.ByteString`, copying it.
--
copyByteString :: S.ByteString  -- ^ Strict 'S.ByteString' to copy
               -> Builder       -- ^ Resulting 'Builder'
copyByteString = writeSingleton writeByteString
{-# INLINE copyByteString #-}

-- | Create a 'Builder' from a 'ByteArray'
--
fromByteArray :: ByteArray  -- ^ 'ByteArray' to copy
              -> Builder    -- ^ Resulting 'Builder'
fromByteArray = writeSingleton writeByteArray
{-# INLINE fromByteArray #-}

-- | Copied from Data.ByteString.Lazy.
--
defaultSize :: Int
defaultSize = 32 * k - overhead
    where k = 1024
          overhead = 2 * sizeOf (undefined :: Int)

-- | Run the builder with the default buffer size.
--
runBuilder :: Builder -> [S.ByteString] -> [S.ByteString]
runBuilder = runBuilderWith defaultSize
{-# INLINE runBuilder #-}

-- | Run the builder with buffers of at least the given size.
--
-- Note that the builders should guarantee that on average the desired buffer
-- size is attained almost perfectly. "Almost" because builders may decide to
-- start a new buffer and not completely fill the existing buffer, if this is
-- faster. However, they should not spill too much of the buffer, if they
-- cannot compensate for it.
--
runBuilderWith :: Int -> Builder -> [S.ByteString] -> [S.ByteString]
runBuilderWith bufSize (Builder b) k = 
    S.inlinePerformIO $ go bufSize (b finalStep)
  where
    finalStep pf _ = return $ Done pf

    go !size !step = do
        buf <- S.mallocByteString size
        withForeignPtr buf $ \pf -> do
            next <- step pf (pf `plusPtr` size)
            case next of
                Done pf'
                  | pf == pf' -> return k
                  | otherwise -> return $ S.PS buf 0 (pf' `minusPtr` pf) : k 
                BufferFull newSize pf' nextStep
                  | pf == pf' -> bufferFullError
                  | otherwise -> return $ S.PS buf 0 (pf' `minusPtr` pf) : 
                       S.inlinePerformIO (go (max newSize bufSize) nextStep)

    bufferFullError =
        error "runBuilder: buffer cannot be full; no data was written."

-- | /O(n)./ Extract the lazy 'L.ByteString' from the builder.
--
toLazyByteString :: Builder       -- ^ 'Builder' to evaluate
                 -> L.ByteString  -- ^ Resulting UTF-8 encoded 'L.ByteString'
toLazyByteString = L.fromChunks . flip runBuilder []
{-# INLINE toLazyByteString #-}
