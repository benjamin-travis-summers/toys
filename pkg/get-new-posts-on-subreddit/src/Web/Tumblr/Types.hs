module Web.Tumblr.Types where

import Prelude
import Data.Aeson
import Data.Maybe
import Control.Applicative ((<$>), (<*>), empty, pure)

import Data.Time           (UTCTime,ZonedTime)
import Data.Time.Format    (parseTime, defaultTimeLocale)
import Data.Time.LocalTime (zonedTimeToUTC)
import Control.Monad.Trans.Control

data BlogInfo = BlogInfo
                { blogInfoTitle :: String
                , blogInfoPosts :: Int
                , blogInfoName :: String
                , blogInfoURL :: Maybe String
                , blogInfoUpdated :: Int -- seconds since epoch
                , blogInfoDescription :: String
                , blogInfoAsk :: Bool
                , blogInfoAskAnon :: Bool
                , blogInfoLikes :: Int } deriving (Show, Eq)

instance FromJSON BlogInfo where
  parseJSON (Object w) =
    (w .: "blog") >>= \v ->
     BlogInfo <$> v .: "title" <*>
                  v .: "posts" <*>
                  v .: "name" <*>
                  v .: "url" <*>
                  v .: "updated" <*>
                  v .: "description" <*>
                  v .: "ask" <*>
                  v .: "ask_anon" <*>
                  (fromMaybe 0 <$> v .:? "likes")
  parseJSON _ = empty


data Avatar = Avatar { avatarURL :: String } deriving (Show, Eq)

instance FromJSON Avatar where
  parseJSON (Object v) = Avatar <$> v .: "avatar_url"
  parseJSON _ = empty


data Likes = Likes
             { likedPosts :: [Post]
             , likedCount :: Int }
             deriving (Show, Eq)

instance FromJSON Likes where
  parseJSON (Object v) = Likes <$>
                         v .: "liked_posts" <*>
                         v .: "liked_count"
  parseJSON _ = empty


data Followers = Followers { followers :: [User] } deriving (Show, Eq)

instance FromJSON Followers where
  parseJSON (Object v) = Followers <$> v .: "users"
  parseJSON _ = empty


data User = User
            { userName :: String
            , userURL :: String
            , userUpdated :: Int }
            deriving (Show, Eq)

instance FromJSON User where
  parseJSON (Object v) = User <$>
                         v .: "name" <*>
                         v .: "url" <*>
                         v .: "updated"
  parseJSON _ = empty


data Posts = Posts { postsBlog :: BlogInfo, posts :: [Post] } deriving (Show, Eq)

instance FromJSON Posts where
  parseJSON o@(Object v) = Posts <$>
                           parseJSON o <*>
                           v .: "posts"
  parseJSON _ = empty


data JustPosts = JustPosts { justPosts :: [Post] } deriving (Show, Eq)

instance FromJSON JustPosts where
  parseJSON o@(Object v) = JustPosts <$>
                           v .: "posts"
  parseJSON _ = empty


data PostState = Published | Queued | Draft | Private deriving (Show, Eq)

instance FromJSON PostState where
  parseJSON (String "published") = pure Published
  parseJSON (String "queued") = pure Queued
  parseJSON (String "draft") = pure Draft
  parseJSON (String "private") = pure Private
  parseJSON _ = empty


data PostFormat = Html | Markdown deriving (Show, Eq)

instance FromJSON PostFormat where
  parseJSON (String "html") = pure Html
  parseJSON (String "markdown") = pure Markdown
  parseJSON _ = empty

data PhotoInfo = PhotoInfo { sizeWidth :: Int, sizeHeight :: Int, photoURL :: String } deriving (Show, Eq)
data Photo = Photo { originalSize :: PhotoInfo
                   -- , alt_sizes :: [PhotoInfo] -- TODO
                   } deriving (Show, Eq)
data Dialogue = Dialogue { dialogueSpeaker :: String, dialogueSpeakerLabel :: String, dialoguePhrase :: String } deriving (Show, Eq)
data VideoPlayer = VideoPlayer { videoPlayerWidth :: Int, videoPlayerEmbedCode :: String } deriving (Show, Eq)
data PostData = TextPost { textTitle :: String, textBody :: String }
              | PhotoPost { photoPostPhotos :: [Photo], photoPostCaption :: String }
              | QuotePost { quoteText :: String, quoteSource :: String }
              | LinkPost { linkTitle :: String, linkURL :: String, linkDescription :: String }
              | ChatPost { chatTitle :: Maybe String, chatBody :: String, chatDialogue :: [Dialogue] }
              | AudioPost { audioCaption :: String, audioPlayer :: String, audioPlays :: Int,
                            audioAlbumArt :: Maybe String, audioArtist :: Maybe String, audioAlbum :: Maybe String, audioTrackName :: Maybe String,
                            audioTrackNumber :: Maybe Int, audioYear :: Maybe Int }
              | VideoPost { videoCaption :: String, videoPlayer :: [VideoPlayer] }
              | AnswerPost { askingName :: String, askingURL :: String, answerQuestion :: String, answerAnswer :: String }
              deriving (Show, Eq)

instance FromJSON PhotoInfo where
  parseJSON (Object v) = PhotoInfo <$>
                         v .: "width" <*>
                         v .: "height" <*>
                         v .: "url"
  parseJSON _ = empty

instance FromJSON Photo where
  parseJSON (Object v) = Photo <$>
                         v .: "original_size" -- <*>
                         -- v .: "alt_sizes" -- TODO
  parseJSON _ = empty

instance FromJSON Dialogue where
  parseJSON (Object v) = Dialogue <$>
                         v .: "name" <*>
                         v .: "label" <*>
                         v .: "phrase"
  parseJSON _ = empty

instance FromJSON VideoPlayer where
  parseJSON (Object v) = VideoPlayer <$>
                         v .: "width" <*>
                         v .: "embed_code"
  parseJSON _ = empty

data Post = Post
            { postBlogName :: String
            , postId :: Int
            , postURL :: String
            , postDate :: UTCTime
            , postTime :: Int
            , postState :: PostState
            , postFormat :: PostFormat
            , postReblogKey :: String
            , postTags :: [String]
            , noteCount :: Int
            , postBookmarklet :: Bool
            , postMobile :: Bool
            , postSourceURL :: Maybe String
            , postSourceTitle :: Maybe String
            , postLiked :: Bool
            , postTypeSpecificData :: PostData
            } deriving (Show, Eq)

instance FromJSON Post where
  parseJSON (Object v) = Post <$>
                         v .: "blog_name" <*>
                         v .: "id" <*>
                         v .: "post_url" <*>
                         ((v .: "date") >>= parseTumblrTime) <*>
                         v .: "timestamp" <*>
                         v .: "state" <*>
                         v .: "format" <*>
                         v .: "reblog_key" <*>
                         v .: "tags" <*>
                         v .:? "note_count" .!= 0 <*>
                         v .:? "bookmarklet" .!= False <*>
                         v .:? "mobile" .!= False <*>
                         v .:? "source_title" <*>
                         v .:? "source_url" <*>
                         v .:? "liked" .!= False <*>
                         ((v .: "type") >>= parseJSONTypeSpecific)
                           where
                             parseJSONTypeSpecific ("text" :: String) = TextPost <$>
                                                                       v .: "title" <*>
                                                                       v .: "body"
                             parseJSONTypeSpecific "photo" = PhotoPost <$>
                                                             v .: "photos" <*>
                                                             v .: "caption"
                             parseJSONTypeSpecific "quote" = QuotePost <$>
                                                             v .: "text" <*>
                                                             v .: "source"
                             parseJSONTypeSpecific "link" = LinkPost <$>
                                                            v .: "title" <*>
                                                            v .: "url" <*>
                                                            v .: "description"
                             parseJSONTypeSpecific "chat" = ChatPost <$>
                                                            v .: "title" <*>
                                                            v .: "body" <*>
                                                            v .: "dialogue"
                             parseJSONTypeSpecific "audio" = AudioPost <$>
                                                             v .: "caption" <*>
                                                             v .: "player" <*>
                                                             v .: "plays" <*>
                                                             v .:? "album_art" <*>
                                                             v .:? "artist" <*>
                                                             v .:? "album" <*>
                                                             v .:? "track_name" <*>
                                                             v .:? "track_number" <*>
                                                             v .:? "year"
                             parseJSONTypeSpecific "video" = VideoPost <$>
                                                             v .: "caption" <*>
                                                             v .: "player"
                             parseJSONTypeSpecific "answer" = AnswerPost <$>
                                                              v .: "asking_name" <*>
                                                              v .: "asking_url" <*>
                                                              v .: "question" <*>
                                                              v .: "answer"
                             parseJSONTypeSpecific _ = fail "Invalid post type."

                             parseTumblrTime t = case parseTime defaultTimeLocale "%F %X %Z" t of
                               Just t' -> pure (zonedTimeToUTC t')
                               Nothing -> fail "Could not parse date"

  parseJSON _ = empty
