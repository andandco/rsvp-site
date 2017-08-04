module S3 where

import ClassyPrelude.Yesod   as Import

import           Control.Lens ((&), (<&>), set, (.~), view)
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Conduit.List as CL (concatMap)
import qualified Network.AWS.Data.Text as T (Text)
import qualified Network.AWS.S3 as S3
import           Network.AWS


execAWS :: (MonadBaseControl IO m, MonadCatch m , MonadIO m)
    => AWS a -> m a
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
    => S3.BucketName -> S3.ObjectKey -> m S3.GetObjectResponse
get bucket key = do
  send (S3.getObject bucket key)
--   view S3.gorsBody response `AWS.sinkBody` CC.mapM_ (liftIO . BS.putStr)

put :: (ToBody a, MonadAWS m)
    => S3.BucketName
    -> S3.ObjectKey
    -> a               -- | the file contents
    -> m S3.PutObjectResponse
put b k f =
  send $ S3.putObject b k (toBody f)
