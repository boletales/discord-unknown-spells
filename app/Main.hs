{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import Lib

import UnliftIO.Concurrent
import UnliftIO.Exception
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
import Data.Time.Clock.POSIX
import Data.Time.Clock
import System.Environment

getServerSettingsPath :: T.Text -> FilePath
getServerSettingsPath serverid = "settings_"++T.unpack serverid++".json"

saveServerSettings :: T.Text -> Settings -> IO ()
saveServerSettings serverid settings = TLIO.writeFile ("settings_"++T.unpack serverid++".json") (encodeToLazyText settings)

loadServerSettings :: T.Text -> IO Settings
loadServerSettings serverid = do
    msettings <- decodeFileStrictIfExist ("loading "++getServerSettingsPath serverid)
    case msettings of
        Nothing -> saveServerSettings serverid defaultSettings
        _       -> return ()
    let s = fromMaybe defaultSettings msettings
    return s

tryReloadGameData :: T.Text -> [T.Text] -> [T.Text] -> Settings -> Map T.Text (Map T.Text Player) -> IO (Map T.Text Player, MagicEnv)
tryReloadGameData serverid pids names settings plsmap = 
    case plsmap !? serverid of
        Just pls -> reLoadGameData serverid pids names settings pls
        Nothing  -> loadGameData   serverid pids names settings 
    

main :: IO ()
main = do
    mref <- newIORef M.empty
    pref <- newIORef M.empty
    mtoken <- lookupEnv "DISCORD_UNKNOWN_SPELLS_TOKEN"
    case mtoken of
        Just token -> do
            putStrLn "working"
            TIO.putStrLn =<< (runDiscord $ def { discordToken   = T.pack token
                                               , discordOnEvent = \ev -> catchAny (eventHandler mref pref ev) (\e -> liftIO $ print e)})
        Nothing -> putStrLn "env var DISCORD_UNKNOWN_SPELLS_TOKEN is not set."

eventHandler :: IORef(Map T.Text (MVar())) -> IORef(Map T.Text (Map T.Text Player)) -> Event -> DiscordHandler ()
eventHandler mref pref event = 
    case event of
        MessageCreate m -> unless (fromBot m) $ 
          case messageGuild m of
            Just gid -> withTimeElapsed "total"
              do
                liftIO $ TIO.putStrLn $ (userName $ messageAuthor m) `T.append` ": " `T.append` (messageText m)
                ss <- liftIO $ loadServerSettings (serveridWithText gid)
                result <- runExceptT $  if not $ isCommand ss (messageText m)
                                        then hE $ Right ()
                                        else do
                                            --lift $ restCall (R.CreateReaction (messageChannel m, messageId m) "eyes")
                                            withTimeElapsed "command" $ handleCommand ss mref pref gid (userId $ messageAuthor m) (snd $ T.splitAt (T.length $ prefix ss) (messageText m))
                case result of
                    Right _ -> pure ()
                    Left ts -> forM_ ts (restCall . R.CreateMessage (messageChannel m))
                
                liftIO $ Prelude.putStrLn $ "end."
                pure ()

        _ -> pure ()

hE e = ExceptT $ return e

getNick m = fromMaybe (userName $ memberUser m) (memberNick m)

getMembers gid = 
    let go fetched i = 
            (do 
                l <- ExceptT $ restCall (R.ListGuildMembers gid (R.GuildMembersTiming (Just 1000) (Just i)))
                --liftIO $ forM_ (L.map getNick l) TIO.putStrLn
                if L.null l
                then return fetched
                else go (l++fetched) (userId . memberUser $ L.last l)
                )
    in runExceptT $ go [] 0

getMVarFromMref mref lid = liftIO $ (\newmv -> atomicModifyIORef mref (\m -> 
                                    case m !? lid of
                                        Just mv -> (m, mv)
                                        Nothing -> (M.insert lid newmv m, newmv)
                                )) =<< newMVar ()

lidData gidt = "data_" `T.append` gidt

handleCommand :: Settings -> IORef(Map T.Text (MVar())) -> IORef(Map T.Text (Map T.Text Player)) -> GuildId -> UserId -> Text -> ExceptT [Text] (ReaderT DiscordHandle IO) b
handleCommand ss mref pref gid sid t = do
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
            eusers <- withTimeElapsed "get members" $ lift $ getMembers gid
            case eusers of
                Left e -> (liftIO $ print e) >> (hE $ Left ["詠唱失敗: ユーザー情報の取得に失敗しました"])
                Right users -> do
                    let humans = L.filter (not . userIsBot . memberUser) users
                    takeMVar =<< getMVarFromMref mref (lidData gidt)
                    plsmap <- liftIO $ readIORef pref
                    (players, env) <-  withTimeElapsed "load" $ liftIO $ tryReloadGameData gidt (L.map (useridWithText . userId . memberUser) humans) (L.map getNick humans) ss plsmap
                    
                    tidt <- withTimeElapsed "target" $
                            case M.toList $ M.filter (\p -> target `T.isPrefixOf` name p) players of
                                [ ]   -> hE $ Left ["詠唱失敗: 該当するプレイヤーが存在しません"]
                                p:q:_ -> hE $ Left ["詠唱失敗: 該当するプレイヤーが複数存在します"] 
                                [p]   -> hE $ Right $ fst p

                    liftIO $ TIO.putStrLn $ "target: " `T.append` (fromMaybe "null" $ name <$> players !? tidt) `T.append` ", spell: " `T.append` spell
                    
                    start <- liftIO getPOSIXTime
                    let (rplayers, logs) = doCast env spell sidt tidt players
                    let reviveLog = L.map (\p -> name (snd p) `T.append` " が死亡しました……蘇生します") $ M.toList $ M.filter (\p -> hp p <= 0) rplayers
                    let revived = M.map (\p -> if hp p > 0 then p else toDefault ss p) rplayers
                    end <- liftIO getPOSIXTime
                    liftIO $ Prelude.putStrLn $ "cast: "++show (end-start)

                    !_ <- liftIO $ atomicModifyIORef pref (\a -> (M.insert gidt revived a ,()))
                    liftIO $ withTimeElapsed "save" $ saveGameData gidt revived ss
                    flip putMVar () =<< getMVarFromMref mref (lidData gidt)

                    liftIO $ forM_ (logs ++ reviveLog) TIO.putStrLn
                    hE $ Left (logs ++ reviveLog)
                

        _   -> hE $ Left ["詠唱失敗: 無効なコマンドです"]

serveridWithText gid = T.pack ("s"++show gid) 
useridWithText uid = T.pack ("u"++show uid)

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isCommand :: Settings -> Text -> Bool
isCommand s = (prefix s `T.isPrefixOf`)