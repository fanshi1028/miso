-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeApplications #-}
-----------------------------------------------------------------------------
module Miso.DSL.FFI where
-----------------------------------------------------------------------------
import           Data.Text (Text, pack, unpack)
import           Text.Read (readMaybe)
import           qualified Language.Javascript.JSaddle as J (JSVal, jsNull, global)
import           Language.Javascript.JSaddle hiding (JSVal, jsNull, global)
import           qualified JavaScript.Array as J (write, read)
import           Control.Monad.IO.Class
import           Control.Concurrent.MVar
import           System.IO.Unsafe
import           Control.Monad
-----------------------------------------------------------------------------
type JSVal = J.JSVal
-----------------------------------------------------------------------------
instance Eq JSVal where
  _ == _ = True
-----------------------------------------------------------------------------
currentJSContext :: MVar JSContextRef
currentJSContext = unsafePerformIO $ newEmptyMVar

runJSM0 :: JSM a -> IO a
runJSM0 f = readMVar currentJSContext >>= runJSM f

runJSM1 :: (a -> JSM b) -> a -> IO b
runJSM1 f a = readMVar currentJSContext >>= runJSM (f a)

runJSM2 :: (a -> b -> JSM c) -> a -> b -> IO c
runJSM2 f a b = readMVar currentJSContext >>= runJSM (f a b)

runJSM3 :: (a -> b -> c -> JSM d) -> a -> b -> c -> IO d
runJSM3 f a b c = readMVar currentJSContext >>= runJSM (f a b c)
-----------------------------------------------------------------------------
toJSVal_Bool :: Bool -> IO JSVal
toJSVal_Bool = runJSM1 toJSVal
-----------------------------------------------------------------------------
toJSVal_Double :: Double -> IO JSVal
toJSVal_Double = runJSM1 toJSVal
-----------------------------------------------------------------------------
toJSVal_Int :: Int -> IO JSVal
toJSVal_Int = runJSM1 toJSVal
-----------------------------------------------------------------------------
toJSVal_List :: [JSVal] -> IO JSVal
toJSVal_List = runJSM1 toJSValListOf
-----------------------------------------------------------------------------
-- | The 'null' value in JS.
jsNull :: JSVal
jsNull = J.jsNull
-----------------------------------------------------------------------------
toJSVal_JSVal :: JSVal -> IO JSVal
toJSVal_JSVal = pure
-----------------------------------------------------------------------------
toJSVal_Char :: Char -> IO JSVal
toJSVal_Char = runJSM1 toJSVal
-----------------------------------------------------------------------------
toJSVal_Float :: Float -> IO JSVal
toJSVal_Float = runJSM1 toJSVal
-----------------------------------------------------------------------------
toJSVal_Text :: Text -> IO JSVal
toJSVal_Text = runJSM1 toJSVal
-----------------------------------------------------------------------------
fromJSVal_Text :: JSVal -> IO (Maybe Text)
fromJSVal_Text = runJSM1 fromJSVal
-----------------------------------------------------------------------------
fromJSValUnchecked_Text :: JSVal -> IO Text
fromJSValUnchecked_Text = runJSM1 fromJSValUnchecked
-----------------------------------------------------------------------------
fromJSVal_Char :: JSVal -> IO (Maybe Char)
fromJSVal_Char = runJSM1 fromJSVal
-----------------------------------------------------------------------------
fromJSValUnchecked_Char :: JSVal -> IO Char
fromJSValUnchecked_Char = runJSM1 fromJSValUnchecked
-----------------------------------------------------------------------------
fromJSVal_Float :: JSVal -> IO (Maybe Float)
fromJSVal_Float = runJSM1 fromJSVal
-----------------------------------------------------------------------------
fromJSValUnchecked_Float :: JSVal -> IO Float
fromJSValUnchecked_Float = runJSM1 fromJSValUnchecked
-----------------------------------------------------------------------------
fromJSVal_Bool :: JSVal -> IO (Maybe Bool)
fromJSVal_Bool = runJSM1 fromJSVal
-----------------------------------------------------------------------------
new_ffi :: JSVal -> JSVal -> IO JSVal
new_ffi constr args = runJSM0 $ fromJSValUncheckedListOf @JSVal args >>= new constr
-----------------------------------------------------------------------------
eval_ffi :: Text -> IO JSVal
eval_ffi = runJSM1 eval
-----------------------------------------------------------------------------
create_ffi :: IO JSVal
create_ffi = runJSM0 $ obj >>= toJSVal
-----------------------------------------------------------------------------
getProp_ffi :: Text -> JSVal -> IO JSVal
getProp_ffi k obj' = runJSM0 $ valToObject obj' >>= getProp (toJSString k)
-----------------------------------------------------------------------------
setProp_ffi :: Text -> JSVal -> JSVal -> IO ()
setProp_ffi k v obj' = runJSM0 $ valToObject obj' >>= setProp (toJSString k) v
-----------------------------------------------------------------------------
fromJSVal_Int :: JSVal -> IO (Maybe Int)
fromJSVal_Int = runJSM1 fromJSVal
-----------------------------------------------------------------------------
fromJSVal_Double :: JSVal -> IO (Maybe Double)
fromJSVal_Double  = runJSM1 fromJSVal
-----------------------------------------------------------------------------
getPropIndex_ffi :: Int -> JSVal -> IO JSVal
getPropIndex_ffi i array' =  runJSM2 J.read i $ SomeJSArray array'
-----------------------------------------------------------------------------
isNull_ffi :: JSVal -> Bool
isNull_ffi = unsafePerformIO . runJSM1 (ghcjsPure . isNull)
-----------------------------------------------------------------------------
isUndefined_ffi :: JSVal -> Bool
isUndefined_ffi = unsafePerformIO . runJSM1 (ghcjsPure . isUndefined)
-----------------------------------------------------------------------------
freeFunction_ffi :: JSVal -> IO ()
freeFunction_ffi = runJSM1 $ valToObject >=> freeFunction . Function
-----------------------------------------------------------------------------
requestAnimationFrame :: JSVal -> IO Int
requestAnimationFrame = runJSM1 $ jsg1 "requestAnimationFrame" >=> fromJSValUnchecked
-----------------------------------------------------------------------------
cancelAnimationFrame :: Int -> IO ()
cancelAnimationFrame =  runJSM1 $ jsg1 "cancelAnimationFrame" >=> fromJSValUnchecked
-----------------------------------------------------------------------------
toJSVal_JSString :: Text -> IO JSVal
toJSVal_JSString = runJSM1 toJSVal
-----------------------------------------------------------------------------
fromJSValUnchecked_Maybe :: JSVal -> IO (Maybe JSVal)
fromJSValUnchecked_Maybe = runJSM1 fromJSValUnchecked
-----------------------------------------------------------------------------
fromJSVal_Maybe :: JSVal -> IO (Maybe (Maybe JSVal))
fromJSVal_Maybe = runJSM1 fromJSVal
-----------------------------------------------------------------------------
fromJSValUnchecked_Bool :: JSVal -> IO Bool
fromJSValUnchecked_Bool = runJSM1 fromJSValUnchecked
-----------------------------------------------------------------------------
invokeFunction :: JSVal -> JSVal -> JSVal -> IO JSVal
invokeFunction f obj' args = runJSM0 $ fromJSValUncheckedListOf @JSVal args >>= call f obj'
-----------------------------------------------------------------------------
listProps_ffi :: JSVal -> IO JSVal
listProps_ffi = runJSM1 $ valToObject >=> listProps >=> toJSVal
-----------------------------------------------------------------------------
setPropIndex_ffi :: Int -> JSVal -> JSVal -> IO ()
setPropIndex_ffi i v array' = runJSM3 J.write i v $ SomeJSArray array'
-----------------------------------------------------------------------------
-- | The @globalThis@ object in JS.
global :: JSVal
global = let Object g = J.global in g
-----------------------------------------------------------------------------
fromJSVal_List :: JSVal -> IO (Maybe [JSVal])
fromJSVal_List = runJSM1 fromJSValListOf
-----------------------------------------------------------------------------
fromJSValUnchecked_Int :: JSVal -> IO Int
fromJSValUnchecked_Int = runJSM1 fromJSValUnchecked
-----------------------------------------------------------------------------
fromJSValUnchecked_Double :: JSVal -> IO Double
fromJSValUnchecked_Double = runJSM1 fromJSValUnchecked
-----------------------------------------------------------------------------
fromJSVal_JSString :: JSVal -> IO (Maybe Text)
fromJSVal_JSString = runJSM1 fromJSVal
-----------------------------------------------------------------------------
-- | A asynchronous callback
asyncCallback :: IO () -> IO JSVal
asyncCallback f = runJSM0 $ asyncFunction ( \_ _ _ -> liftIO f) >>= toJSVal
-- | A asynchronous callback with one argument
asyncCallback1 :: (JSVal -> IO ()) -> IO JSVal
asyncCallback1 f =
  runJSM0 $
    asyncFunction
      ( \_ _ -> \case
          a : _ -> liftIO $ f a
          _ -> undefined
      )
      >>= toJSVal
-- | A asynchronous callback with two arguments
asyncCallback2 :: (JSVal -> JSVal -> IO ()) -> IO JSVal
asyncCallback2 f =
  runJSM0 $
    asyncFunction
      ( \_ _ -> \case
          a : b : _ -> liftIO $ f a b
          _ -> undefined
      )
      >>= toJSVal
-- | A asynchronous callback with three arguments
asyncCallback3 :: (JSVal -> JSVal -> JSVal -> IO ()) -> IO JSVal
asyncCallback3 f =
  runJSM0 $
    asyncFunction
      ( \_ _ -> \case
          a : b : c : _ -> liftIO $ f a b c
          _ -> undefined
      )
      >>= toJSVal

-----------------------------------------------------------------------------
-- | A synchronous callback
syncCallback :: IO () -> IO JSVal
syncCallback f = runJSM0 $ function (\_ _ _ -> liftIO f) >>= toJSVal

-- | A synchronous callback with a single argument
syncCallback1 :: (JSVal -> IO ()) -> IO JSVal
syncCallback1 f =
  runJSM0 $
    function
      ( \_ _ -> \case
          a : _ -> liftIO $ f a
          _ -> undefined
      )
      >>= toJSVal
-- | A synchronous callback with two arguments
syncCallback2 :: (JSVal -> JSVal -> IO ()) -> IO JSVal
syncCallback2 f =
  runJSM0 $
    function
      ( \_ _ -> \case
          a : b : _ -> liftIO $ f a b
          _ -> undefined
      )
      >>= toJSVal
-- | A synchronous callback with three arguments
syncCallback3 :: (JSVal -> JSVal -> JSVal -> IO ()) -> IO JSVal
syncCallback3 f =
  runJSM0 $
    function
      ( \_ _ -> \case
          a : b : c : _ -> liftIO $ f a b c
          _ -> undefined
      )
      >>= toJSVal
-----------------------------------------------------------------------------
-- | A synchronous callback that returns a value
syncCallback' :: IO JSVal -> IO JSVal
syncCallback' f = syncCallback $ () <$ f
-- | A synchronous callback that takes a single argument and returns a value
syncCallback1' :: (JSVal -> IO JSVal) -> IO JSVal
syncCallback1' f = syncCallback1 $ \a -> () <$ f a
-- | A synchronous callback that takes two arguments and returns a value
syncCallback2' :: (JSVal -> JSVal -> IO JSVal) -> IO JSVal
syncCallback2' f = syncCallback2 $ \a b -> () <$ f a b
-- | A synchronous callback that takes three arguments and returns a value
syncCallback3' :: (JSVal -> JSVal -> JSVal -> IO JSVal) -> IO JSVal
syncCallback3' f = syncCallback3 $ \a b c -> () <$ f a b c
-----------------------------------------------------------------------------
parseInt :: Text -> Maybe Int
parseInt = readMaybe . unpack
-----------------------------------------------------------------------------
parseDouble :: Text -> Maybe Double
parseDouble = readMaybe . unpack
-----------------------------------------------------------------------------
parseWord :: Text -> Maybe Word
parseWord = readMaybe . unpack
-----------------------------------------------------------------------------
parseFloat :: Text -> Maybe Float
parseFloat = readMaybe . unpack
-----------------------------------------------------------------------------
toString_Int :: Int -> Text
toString_Int = pack . show
-----------------------------------------------------------------------------
toString_Word :: Word -> Text
toString_Word = pack . show
-----------------------------------------------------------------------------
toString_Float :: Float -> Text
toString_Float = pack . show
-----------------------------------------------------------------------------
toString_Double :: Double -> Text
toString_Double = pack . show
-----------------------------------------------------------------------------
