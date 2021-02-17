{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import Lib

import System.Directory

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
import Safe

getServerSettingsPath :: T.Text -> FilePath
getServerSettingsPath serverid = datadir ++ "settings_"++T.unpack serverid++".json"

saveServerSettings :: T.Text -> Settings -> IO ()
saveServerSettings serverid settings = TLIO.writeFile (getServerSettingsPath serverid) (encodeToLazyText settings)

loadServerSettings :: T.Text -> IO Settings
loadServerSettings serverid = do
    msettings <- decodeFileStrictIfExist (getServerSettingsPath serverid)
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
    doesDatadirExist <- doesDirectoryExist datadir
    unless doesDatadirExist (createDirectory datadir)
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
                    Left ts -> void $ restCall $ R.CreateMessage (messageChannel m) (T.intercalate "\n" ts)
                
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
    if T.length t == 0
    then hE $ Left ["詠唱失敗: コマンドがありません"]
    else hE $ Right ()

    case T.head t of
        'c' -> handleCast   ss mref pref gid sid t
        's' -> handleStatus False ss mref pref gid sid t
        'S' -> handleStatus True  ss mref pref gid sid t

        _   -> hE $ Left ["詠唱失敗: 無効なコマンドです"]

data Abbr = Abbr Char Int | NoAbbr deriving(Show, Eq)
checkAbbr t =
    if T.null t then NoAbbr else
        let char = T.head t
            same = T.foldl (\n c -> if c==char then (1+) <$> n else Nothing) (Just 0) t
            num  = readMay $ T.unpack $ T.tail t
        in case same of
            Just n  -> Abbr char n
            Nothing -> case num of
                        Just n -> Abbr char n
                        Nothing -> NoAbbr

withLog t log =
    case checkAbbr t of
        Abbr '.' n -> fromMaybe t (log `atMay` (n-1))
        _ -> t

withLock :: MonadIO m => IORef (Map Text (MVar ())) -> Text -> ExceptT e m a -> ExceptT e m a
withLock mref gidt f = ExceptT $ do
    takeMVar =<< getMVarFromMref mref (lidData gidt)
    r <- runExceptT f
    flip putMVar () =<< getMVarFromMref mref (lidData gidt)
    return r

handleCast ss mref pref gid sid t = do
    let gidt = serveridWithText gid
    let sidt = useridWithText sid
    let args = T.tail t
    let splitter = spellsplitter ss
    (rawtarget, rawspell) <- hE $ maybeToRight ["詠唱失敗: 呪文がありません"] $
                            (\(i,s) -> (T.strip (T.take i args), T.strip (T.drop (T.length splitter) s)))
                            <$> L.find (T.isPrefixOf splitter . snd) (L.zip [0..] $ T.tails args)
    eusers <- withTimeElapsed "get members" $ lift $ getMembers gid
    case eusers of
        Left e -> (liftIO $ print e) >> (hE $ Left ["詠唱失敗: ユーザー情報の取得に失敗しました"])
        Right users -> withLock mref gidt do
            start <- liftIO getPOSIXTime
            let humans = L.filter (not . userIsBot . memberUser) users
            plsmap <- liftIO $ readIORef pref
            (players, env) <-  withTimeElapsed "load" $ liftIO $ tryReloadGameData gidt (L.map (useridWithText . userId . memberUser) humans) (L.map getNick humans) ss plsmap
            
            let spell  = maybe rawspell  (withLog rawspell  . spellLog ) (players !? sidt)
            tidt <- withTimeElapsed "target" $
                        let fromLog = case checkAbbr rawtarget of
                                        Abbr '.' n -> ((`atMay` (n-1)).targetLog) =<< (players !? sidt)
                                        _          -> Nothing 
                        in case fromLog of
                            Just t -> return t
                            _ -> case M.toList $ M.filter (\p -> rawtarget `T.isPrefixOf` name p) players of
                                    [ ]   -> hE $ Left ["取得失敗: 該当するプレイヤーが存在しません"]
                                    p:q:_ -> hE $ Left ["取得失敗: 該当するプレイヤーが複数存在します"] 
                                    [p]   -> hE $ Right $ fst p

            liftIO $ TIO.putStrLn $ "target: " `T.append` (fromMaybe "null" $ name <$> players !? tidt) `T.append` ", spell: " `T.append` spell
            
            let (rplayers, logs) = doCast env spell sidt tidt players
            let reviveLog = L.map (\p -> name (snd p) `T.append` " が死亡しました……蘇生します") $ M.toList $ M.filter (\p -> hp p <= 0) rplayers
            let revived = M.map (\p -> if hp p > 0 then p else toDefault ss p) rplayers

            !_ <- liftIO $ atomicModifyIORef pref (\a -> (M.insert gidt revived a ,()))
            liftIO $ withTimeElapsed "save" $ saveGameData gidt revived ss

            liftIO $ forM_ (logs ++ reviveLog) TIO.putStrLn
            end <- liftIO getPOSIXTime
            liftIO $ Prelude.putStrLn $ "internal total: "++show (end-start)
            hE $ Left (logs ++ reviveLog)

handleStatus detail ss mref pref gid sid t = do
    let gidt = serveridWithText gid
    let sidt = useridWithText sid
    let rawtarget = T.strip $ T.tail t
    eusers <- withTimeElapsed "get members" $ lift $ getMembers gid
    case eusers of
        Left e -> (liftIO $ print e) >> (hE $ Left ["詠唱失敗: ユーザー情報の取得に失敗しました"])
        Right users -> do
            start <- liftIO getPOSIXTime
            let humans = L.filter (not . userIsBot . memberUser) users
            plsmap <- liftIO $ readIORef pref
            (players, env) <-  withTimeElapsed "load" $ liftIO $ tryReloadGameData gidt (L.map (useridWithText . userId . memberUser) humans) (L.map getNick humans) ss plsmap
            
            tidt <- withTimeElapsed "target" $
                        let fromLog = case checkAbbr rawtarget of
                                        Abbr '.' n -> ((`atMay` (n-1)).targetLog) =<< (players !? sidt)
                                        _          -> Nothing 
                        in case fromLog of
                            Just t -> return t
                            _ -> case M.toList $ M.filter (\p -> rawtarget `T.isPrefixOf` name p) players of
                                    [ ]   -> hE $ Left ["詠唱失敗: 該当するプレイヤーが存在しません"]
                                    p:q:_ -> hE $ Left ["詠唱失敗: 該当するプレイヤーが複数存在します"] 
                                    [p]   -> hE $ Right $ fst p

            let result = if detail then statusTextDetails env players tidt else statusText env players tidt 
            end <- liftIO getPOSIXTime
            liftIO $ Prelude.putStrLn $ "internal total: "++show (end-start)
            hE $ Left result

serveridWithText gid = T.pack ("s"++show gid) 
useridWithText uid = T.pack ("u"++show uid)

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isCommand :: Settings -> Text -> Bool
isCommand s = (prefix s `T.isPrefixOf`)