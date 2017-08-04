module Handler.Admin.Event where

import           Data.Conduit.Binary (sinkLbs)
import           Control.Monad
import           Data.Conduit
import           Data.Text               (Text)
import           Data.Time
import           Control.Monad.Trans.AWS
import           Network.AWS.S3 hiding (Event, redirect)
import           Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

import           Import
import qualified S3

getAdminEventR :: Handler Html
getAdminEventR = do
  (formWidget, formEncType) <- generateFormPost eventForm
  defaultLayout $ do
    setTitle' "Create Event"
    $(widgetFile "admin/events")

postAdminEventR :: Handler Html
postAdminEventR = do
  ((result, formWidget), formEncType) <- runFormPost eventForm
  userId  <- requireAuthId
  case result of
    FormSuccess ef@(EventForm{..}) -> do
      filename <- writeToServer ef_fileInfo
      case eventFormToEvent ef userId filename of
        Left msg -> do
          setMessage $ toHtml msg
          redirect $ AdminEventR
        Right e -> do
          _ <- runDB $ insert e

          setMessage "Event saved"
          redirect $ AdminEventR

    FormFailure reasons -> defaultLayout $ do
      setMessage $ toHtml $ unlines reasons
      redirect $ AdminEventR
    _ -> defaultLayout $ do
      setMessage "something went wrong"
      redirect $ AdminEventR

eventFormToEvent :: EventForm -> UserId -> Text -> Either Text Event
eventFormToEvent EventForm{..} uid filename = do
  let dt_end = case ef_eventEndDatetime of
        Nothing -> Nothing
        Just j -> parseISO8601 j

  case parseDatetime ef_eventStartDatetime of
    Left _ -> Left "invalid start date"
    Right start -> do
      Right $ Event {
          eventName = ef_name
        , eventDescription = (fmap unTextarea ef_description)
        , eventAsset_id = filename
        , eventOwner_id = uid
        , eventAll_day = False
        , eventStart_datetime = start
        , eventEnd_datetime = dt_end
        }


writeToServer :: FileInfo -> Handler Text
writeToServer file = do
  bucketname <- fmap (appBucketName . appSettings) getYesod
  fileContents <- runResourceT $ fileSource file $$ sinkLbs
  let fn = genFileName fileContents
  _por <- S3.execAWS (
    S3.put bucketname fn fileContents)
  return fn
  where
    genFileName lbs = "logo/upload-" <> (pack $ base64md5 lbs)

data EventForm
  = EventForm
  { ef_name :: Text
  , ef_description :: Maybe Textarea
  , ef_eventStartDatetime :: Text
  , ef_eventEndDatetime :: Maybe Text
  , ef_fileInfo :: FileInfo
  }

eventForm :: Form EventForm
eventForm = renderBootstrap3 BootstrapBasicForm $
  EventForm
    <$> areq textField (textSettings "name" "Your Event's name") Nothing
    <*> aopt textareaField (textSettings "description" "Describe your event") Nothing
    <*> areq textField (daySettings "start") Nothing
    <*> aopt textField (daySettings "end") Nothing
    <*> fileAFormReq "Choose an Event Image"
  where
    daySettings t = FieldSettings
          { fsLabel = SomeMessage t
          , fsTooltip = Nothing
          , fsId = Just $ "datetimepicker_" <> t <> "_time"
          , fsName = Nothing
          , fsAttrs =
              [ ("class", "form-control")
              , ("placeholder", "placeholder")
              ]
          }
    textSettings t p = FieldSettings
          { fsLabel = t
          , fsTooltip = Nothing
          , fsId = Nothing
          , fsName = Nothing
          , fsAttrs =
              [ ("class", "form-control")
              , ("placeholder", p)
              ]
          }

parseISO8601 :: Text -> Maybe UTCTime
parseISO8601 = parseTimeM True defaultTimeLocale "%Y-%-m-%-d %H:%M" . unpack

parseDatetime :: Text -> Either FormMessage UTCTime
parseDatetime = maybe (Left MsgInvalidDay) Right . parseISO8601
