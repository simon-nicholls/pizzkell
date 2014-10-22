{-# LANGUAGE OverloadedStrings #-}

module Util where

import           Snap.Core
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Text
import           Data.Text.Encoding
import           Text.Blaze
import           Text.Blaze.Html5.Attributes
import           Text.Blaze.Internal (Attribute, AttributeValue, attribute)

href :: (ToValue a) => a -> Attribute
-- href = attribute (stringTag ("href" :: String)) " href=\"" . toValue
href = attribute (stringTag "href") (stringTag " href=\"") . toValue

getParamText ::  MonadSnap m => ByteString -> m (Maybe Text)
getParamText = fmap (fmap decodeUtf8) . getParam

finishWithStatus :: MonadSnap m =>
  (Response -> Response) -> ByteString -> Int -> m a
finishWithStatus f s c =
  getsResponse (setResponseStatus c s . f) >>= finishWith

require' :: MonadSnap m
  => (Response -> Response)
  -- ^ Response modifier on failure
  -> ByteString
  -- ^ HTTP status message on failure
  -> m (Maybe a)
  -- ^ Computation which might fail
  -> m a
require' f msg = (maybe (finishWithStatus f msg 400) return =<<)

-- | Same as @require'@, but sets empty response.
require :: MonadSnap m => ByteString -> m (Maybe a) -> m a
require msg m = require' (const emptyResponse) msg m

requireParamText :: MonadSnap m => ByteString -> m Text
requireParamText p = require err $ getParamText p
  where err = BS.append "required parameter: " p
