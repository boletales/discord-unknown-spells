{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Lib

import UnliftIO.Concurrent
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Data.Maybe
import Data.Map as M
import Data.List as L
import Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as TLIO

import Discord
import Discord.Types
import qualified Discord.Requests as R
import Data.Aeson
import GHC.Generics
import Data.IORef
import Data.Aeson.Text
import Data.Either.Combinators

data GeneralSettings = GeneralSettings{
                            token  :: T.Text,
                            status :: T.Text
                        } deriving (Show, Generic)
instance FromJSON GeneralSettings
instance ToJSON   GeneralSettings

defaultGeneralSettings = GeneralSettings{
        token  = "your bot token here",
        status = ""
    }

data ServerSettings = ServerSettings{
                            prefix       :: T.Text,
                            spellsplitter :: T.Text
                        } deriving (Show, Generic)
instance FromJSON ServerSettings
instance ToJSON   ServerSettings

defaultServerSettings = ServerSettings{
        prefix        = "%m",
        spellsplitter  = "!"
    }

saveGeneralSettings :: T.Text -> GeneralSettings -> IO ()
saveGeneralSettings serverid settings = TLIO.writeFile "settings.json" (encodeToLazyText settings)

saveServerSettings :: T.Text -> ServerSettings -> IO ()
saveServerSettings serverid settings = TLIO.writeFile ("settings_"++T.unpack serverid++".json") (encodeToLazyText settings)

loadServerSettings :: IORef(Map T.Text ServerSettings) -> T.Text -> IO ServerSettings
loadServerSettings sref serverid = do
    sm <- readIORef sref
    case sm !? serverid of
        Just s  -> return s
        Nothing ->  do
            msettings <- decodeFileStrict ("settings_"++T.unpack serverid++".json")
            case msettings of
                Nothing -> saveServerSettings serverid defaultServerSettings
                _       -> return ()
            let s = fromMaybe defaultServerSettings msettings
            writeIORef sref (M.insert serverid s sm)
            return s

main :: IO ()
main = do
    esettings <- eitherDecodeFileStrict' "settings.json"
    ref <- newIORef M.empty
    case esettings of
        Right settings -> TIO.putStrLn =<< (runDiscord $ def { discordToken   = token settings
                                                             , discordOnEvent = eventHandler ref })
        Left error -> putStrLn "error on loading settings.json" >> putStrLn error


eventHandler :: IORef(Map T.Text ServerSettings) -> Event -> DiscordHandler ()
eventHandler sref event = 
    case event of
        MessageCreate m -> unless (fromBot m) $ 
          case messageGuild m of
            Just gid ->
              do
                ss <- liftIO $ loadServerSettings sref (serveridWithText gid)
                result <- runExceptT $  if not $ isCommand ss (messageText m)
                                        then hE $ Right ()
                                        else handleCommand ss gid (userId $ messageAuthor m) (snd $ T.splitAt (T.length $ prefix ss) (messageText m))
                case result of
                    Right _ -> pure ()
                    Left ts -> forM_ ts (restCall . R.CreateMessage (messageChannel m))
                pure ()

        _ -> pure ()

hE e = ExceptT $ return e

handleCommand :: ServerSettings -> GuildId -> UserId -> Text -> ExceptT [Text] (ReaderT DiscordHandle IO) b
handleCommand ss gid sid t = do
    let gidt = serveridWithText gid
    let sidt = useridWithText sid

    if T.length t == 0
    then hE $ Left ["詠唱失敗: コマンドがありません"]
    else hE $ Right ()

    case T.head t of
        'c' -> do
            let args = T.tail t
            let splitter = spellsplitter ss
            (target, spell) <- hE $ maybeToRight ["詠唱失敗: 呪文がありません"] $
                                    (\(i,s) -> (T.strip (T.take i args), T.strip (T.drop (T.length splitter) s)))
                                    <$> L.find (T.isPrefixOf splitter . snd) (L.zip [0..] $ T.tails args)
            eusers <- lift $ restCall (R.ListGuildMembers gid (R.GuildMembersTiming Nothing Nothing))
            case eusers of
                Left _ -> hE $ Left ["詠唱失敗: ユーザー情報の取得に失敗しました"]
                Right users -> do
                    let humans = L.filter (not . userIsBot . memberUser) users
                    (players, env) <- liftIO $ withTimeElapsed "load" $ loadGameData gidt (L.map (useridWithText . userId . memberUser) humans) (L.map (fromMaybe "null" . memberNick) humans)
                    tidt <- case M.toList $ M.filter (\p -> target `T.isPrefixOf` name p) players of
                                [ ]   -> hE $ Left ["詠唱失敗: 該当するプレイヤーが存在しません"]
                                p:q:_ -> hE $ Left ["詠唱失敗: 該当するプレイヤーが複数存在します"] 
                                [p]   -> hE $ Right $ fst p
                    --hE $ maybeToRight ["該当するプレイヤーが存在しない/複数います"]
                    let (rplayers, logs) = doCast env spell sidt tidt players
                    let reviveLog = L.map (\p -> name (snd p) `T.append` " が死亡しました……蘇生します") $ M.toList $ M.filter (\p -> hp p <= 0) rplayers
                    let s = settings env
                    let revived = M.map (\p -> if hp p > 0 then p else toDefault s p) rplayers
                    liftIO $ withTimeElapsed "save" $ saveGameData gidt revived s
                    hE $ Left (logs ++ reviveLog)
                

        _   -> hE $ Left ["詠唱失敗: 無効なコマンドです"]

serveridWithText gid = T.pack ("s"++show gid) 
useridWithText uid = T.pack ("u"++show uid)

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isCommand :: ServerSettings -> Text -> Bool
isCommand s = (prefix s `T.isPrefixOf`)