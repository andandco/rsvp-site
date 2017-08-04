module S3 where

import ClassyPrelude.Yesod   as Import

import           Control.Lens ((&), (<&>), set, (.~), view)
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Conduit.List as CL (concatMap)
import qualified Network.AWS.Data.Text as T (Text)
import qualified Network.AWS.S3 as S3
import           Network.AWS

type AWSReq m = (MonadBaseControl IO m, MonadCatch m , MonadIO m)

presignAsset :: (AWSReq m) => Text -> m Text
presignAsset objectKey = do
  t <- liftIO getCurrentTime
  res <- liftIO $ S3.execAWS $ presignURL t (60*60*24) $
    S3.getObject (S3.BucketName "duskhost") (S3.ObjectKey objectKey)
  return $ decodeUtf8 res

execAWS :: AWSReq m => AWS a -> m a
execAWS op = do
    lgr <- newLogger Debug stdout
    env <- newEnv Discover <&> set envLogger lgr . set envRegion Oregon
    -- env <- AWS.newEnv AWS.Discover
    runResourceT . runAWS env $ op

getKeys :: MonadAWS m
    => S3.BucketName -> T.Text -> ConduitM () S3.ObjectKey m ()
getKeys bucket keyPrefix =
    paginate listObjects =$= CL.concatMap objectKeyName
    where
        listObjects = S3.listObjects bucket
            & S3.loPrefix .~ Just keyPrefix
            & S3.loMaxKeys .~ Just 1000

        objectKeyName rs =
            map (view S3.oKey) (view S3.lorsContents rs)

get :: (MonadResource m, MonadAWS m)
    => S3.BucketName -> Text -> m S3.GetObjectResponse
get bucket key = do
  send (S3.getObject bucket (S3.ObjectKey key))
--   view S3.gorsBody response `AWS.sinkBody` CC.mapM_ (liftIO . BS.putStr)

put :: (ToBody a, MonadAWS m)
    => S3.BucketName
    -> Text
    -> a               -- | the file contents
    -> m S3.PutObjectResponse
put b key body =
  send $ S3.putObject b (S3.ObjectKey key) (toBody body)
