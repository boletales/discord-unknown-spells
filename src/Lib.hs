{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Lib where

import Control.Monad.IO.Class

import System.Directory
import Data.List as L
import Crypto.Hash 
import Crypto.Hash.Algorithms
import Data.ByteString.Char8 as BSC8
import Data.ByteString as BS
import Data.ByteArray  as BA
import Data.Text.Encoding as TE
import GHC.Word
import GHC.Generics
import Data.Time.Clock.POSIX
import Data.Time.Clock
import GHC.Float
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Text
import Data.Maybe
import Data.Map as M
import Data.Text as T
import Data.Text.Lazy.IO as TLIO
import Control.Monad
import Data.Double.Conversion.Text

{-
属性 → ()
威力
命中
バフ → 幸運, 攻撃, 防御
-}

data Settings = Settings {
        prefix        :: T.Text,
        spellsplitter :: T.Text,
        pepper        :: T.Text,
        manaStart     :: Int,
        hpStart       :: Int,
        manaRegenMin  :: Double,
        buffPow       :: Double,
        damagePow     :: Double,
        damageMag     :: Double,
        healPow       :: Double,
        healMag       :: Double,
        costMag       :: Double,
        buffLifeSpan  :: Double
    } deriving (Show, Generic)

instance FromJSON Settings
instance ToJSON   Settings


defaultSettings = Settings {
        prefix        = "%m",
        spellsplitter = "!",
        pepper        = "",
        hpStart       = 400,
        manaStart     = 400,
        manaRegenMin  = 60,
        buffPow       = 2,
        damagePow     = 4,
        damageMag     = 50,
        healPow       = 4,
        healMag       = 50,
        costMag       = 50,
        buffLifeSpan  = 120
    }

hpMax   s p = hpStart s
manaMax s p = manaStart s

data MagicEnv = MagicEnv {
        settings :: Settings,
        time     :: POSIXTime
    }

settingsToEnv :: Settings -> IO MagicEnv
settingsToEnv s = MagicEnv s <$> getPOSIXTime

data SpellResult = Casted T.Text  | ManaConsumed Int | ManaRestored Int | Damaged Int | Healed Int | Buffed BuffType Double deriving (Show, Eq)

data BuffType  = StAttack | StDefense
                    deriving (Show, Eq, Enum)

data SpellType = Attack | Heal | Buff BuffType
                    deriving (Show, Eq)

data SpellData = SpellData {
        spellstr  :: T.Text,
        spelltype :: SpellType,
        power     :: Int
    } deriving (Show, Eq)

data Player = Player {
        hp   :: Int,
        mana :: Int,
        name :: T.Text,
        buff :: [(BuffType, Double, POSIXTime)],
        imData     :: ImData,
        targetLog  :: [T.Text],
        spellLog   :: [T.Text],
        lastLoaded :: POSIXTime 
    } deriving (Show, Eq)

emptyPlayer = Player {
        hp   = 0,
        mana = 0,
        name = "",
        buff = [],
        targetLog  = [],
        spellLog   = [],
        imData     = imDataDefault,
        lastLoaded = secondsToNominalDiffTime 0
    }

data ImData = ImData {
        totalDeath       :: Int,
        totalDamageGiven :: Int,
        totalDamageTaken :: Int,
        totalCast        :: Int,
        totalMana        :: Int
    } deriving (Show, Eq, Generic)

instance FromJSON ImData
instance ToJSON   ImData

newtype HashReader a = HashReader {
                                runHashReaderWithList  :: [Word8] -> (a, [Word8])
                            }

hashPepper = "Just a spell "

runHashReader :: HashReader a -> ByteString -> (a, [Word8])
runHashReader hr str = runHashReaderWithList hr (L.map (flip BS.append (hashPepper `BS.append` str) . BSC8.pack . show) [0..] >>= BA.unpack . hashWith SHA256)

execHashReader :: HashReader a -> ByteString -> a
execHashReader xs = fst . runHashReader xs

evalHashReader :: HashReader a -> ByteString -> [Word8]
evalHashReader xs = snd . runHashReader xs

instance Functor HashReader where
    fmap f h = h >>= return . f

instance Applicative HashReader where
    pure a = HashReader (a, )
    hf <*> ha = hf >>= (\f -> ha >>= return.f)

instance Monad HashReader where
    return a = HashReader (a, )
    (HashReader run) >>= f = HashReader (\xs -> let (re,lf) = run xs 
                                                 in runHashReaderWithList (f re) lf)


askBytes n = HashReader (L.splitAt n)

weightedChoose choices weights i = let go j [x] = fst x
                                       go j (x:xs) = if snd x > j then fst x else go (j - snd x) xs
                                       zipped = L.zip choices weights
                                    in go (i `mod` sum (L.map snd zipped)) zipped

bytesToInt :: [Word8] -> Int
bytesToInt bytes = let go i (x:xs) = go (i * 2^8 + fromInteger (toInteger x)) xs
                       go i []     = i
                    in go 0 bytes

askInt :: HashReader Int
askInt = bytesToInt <$> askBytes 4

getSpell :: T.Text -> T.Text -> SpellData
getSpell str p = flip execHashReader (TE.encodeUtf8 $ p `T.append` str) $ do
    st <- weightedChoose [Attack, Heal, Buff StAttack, Buff StDefense] [14,4,1,1] <$> askInt
    pw <- askInt

    return SpellData {
                spellstr  = str,
                spelltype = st,
                power     = pw
            }

intToPowDouble :: Double -> Int -> Double
intToPowDouble mag int = mag ** (fromIntegral int / (2^32) - 0.5)

calcBuff env spell source target = intToPowDouble (buffPow $ settings env) (power spell)

calcBuffTotal env buffType player = 
    (L.product
        $ L.map    (\(bt, po, pt, ag, ls) -> po ** (1- ag/ls) ) 
        $ L.filter (\(bt, po, pt, ag, ls) -> bt == buffType && ag < ls ) 
        $ L.map    (\(bt, po, pt) -> (bt, po, pt, realToFrac (nominalDiffTimeToSeconds (time env - pt)), buffLifeSpan (settings env)) )
        $ buff player)

calcDamage :: MagicEnv -> SpellData -> Player -> Player -> Int
calcDamage env spell source target = 
    let saBuff = calcBuffTotal env StAttack  source
        tdBuff = calcBuffTotal env StDefense target
        baseDamage = (damageMag $ settings env) * intToPowDouble (damagePow $ settings env) (power spell)
    in double2Int $ baseDamage * saBuff / tdBuff

calcHeal env spell source target = 
    let saBuff = calcBuffTotal env StAttack  source
        tdBuff = calcBuffTotal env StDefense target
        baseDamage = (damageMag $ settings env) * intToPowDouble (damagePow $ settings env) (power spell)
    in double2Int $ baseDamage * saBuff

calcCost spell ss = double2Int $ intToPowDouble 6 (power spell) * 
                                case spelltype spell of
                                    Attack -> costMag ss * 1.0
                                    Heal   -> costMag ss * 1.0
                                    Buff t -> costMag ss * 1.0


castSideEff env cost player = player{ imData = let d = imData player in d{totalCast = totalCast d + 1, totalMana = totalMana d + cost}}

consumeMana env cost player = player{mana   = max 0 $ mana player - cost,
                                     imData = let d = imData player in d{totalMana = totalMana d + cost}}

dealDamage env damageam player = player{ hp     = hp player - damageam,
                                         imData = let d = imData player in d {totalDamageTaken = totalDamageTaken d + damageam}}

dealAttack env damageam player = player{ imData = let d = imData player in d {totalDamageGiven = totalDamageGiven d + damageam}}

dealHeal env healam player = player{ hp = min (hpStart $ settings env) (hp player + healam)}
dealBuff env t buffam player = player{ buff = (t, buffam, time env) : buff player }
setLog spell target player = player{ spellLog = spell : spellLog player, targetLog = target : targetLog player}
                                     
tryCast :: MagicEnv -> SpellData -> T.Text -> T.Text -> Map T.Text Player -> Maybe (Map T.Text Player, [(T.Text , SpellResult)])
tryCast env spell sourceid targetid players = do
    source <- players !? sourceid
    let s = settings env
    let cost = calcCost spell s
    if mana source < cost
    then Nothing
    else Just ()
    let castedpls = M.adjust (castSideEff env cost) sourceid players
    target <- players !? targetid
    (m,r) <- case spelltype spell of
        Attack -> Just (let damageam = calcDamage env spell source target
                        in ( M.adjust   (dealAttack env damageam) sourceid
                             $ M.adjust (dealDamage env damageam) targetid
                             $ M.adjust (consumeMana env cost) sourceid
                             $ castedpls
                            , [(sourceid ,ManaConsumed cost), (targetid, Damaged damageam)]))

        Heal   -> Just (let healam   = calcHeal   env spell source target
                        in ( M.adjust (consumeMana env cost) sourceid
                             $ M.adjust (dealHeal env healam) targetid
                             $ castedpls
                            , [(sourceid ,ManaConsumed cost), (targetid, Healed healam)]))

        Buff t -> Just (let buffam   = calcBuff env spell source target
                        in ( M.adjust (consumeMana env cost) sourceid
                             $ M.adjust (dealBuff env t buffam) targetid
                             $ castedpls
                            , [(sourceid ,ManaConsumed cost), (targetid, Buffed t buffam)]))
    return (m, (sourceid, Casted $ spellstr spell) : r)

doCast :: MagicEnv -> Text -> Text -> Text -> Map Text Player -> (Map Text Player, [Text])
doCast env spellstr sourceid targetid players =
    let source = players !? sourceid
        target = players !? targetid
        spell  = getSpell spellstr (pepper $ settings env)
        mr     = tryCast env spell sourceid targetid players
    in case mr of
        Nothing -> (players, ["詠唱失敗…… " `T.append`  maybe "" (manaBar (settings env)) source ])
        Just (pls, rs) ->  (M.adjust (setLog spellstr targetid) sourceid pls , spellResultToText (settings env) rs pls)

statusText :: MagicEnv -> M.Map T.Text Player -> T.Text -> [T.Text]
statusText env players tid = maybe ["取得失敗: 該当するプレイヤーが存在しません"] (\p -> 
    [name p `T.append` " (" `T.append` "攻撃力: " `T.append` (toFixed 2 $ calcBuffTotal env StAttack p) `T.append` "x, " `T.append` "防御力: " `T.append` (toFixed 2 $ calcBuffTotal env StDefense p) `T.append` "x" `T.append` ")"
   , "mana: " `T.append` manaBar (settings env) p
   , "hp: " `T.append` hpBar (settings env) p
   ]) (players !? tid)

clip mn mx x = max mn (min mx x)

--"🌕🌖🌗🌘🌑"
--"❤️💔🖤"
phasedProgressBar length phase max now = 
    let pl  = L.length phase -1
        i   = truncate (clip 0.0 1.0 (fromIntegral now/fromIntegral max)*fromIntegral (length*pl))
        bar = L.take length $ L.replicate (i `div` pl) (L.head phase) ++ [phase !! (pl - i `mod` pl)] ++ L.replicate (length - i `div` pl) (L.last phase)
     in T.pack $ show now ++ "/" ++ show max ++ " " ++ bar

manaBar s p = phasedProgressBar 10 "🌕🌖🌗🌘🌑" (manaMax s p) (mana p)
hpBar s p   = phasedProgressBar 10 "❤️💔🖤" (hpMax s p) (hp p)

spellResultToText :: Ord k => Settings -> [(k, SpellResult)] -> Map k Player -> [Text]
spellResultToText s results players =
    join $ L.map 
        (\(pid, r) -> 
            case players !? pid of
              Nothing -> []
              Just p  -> case r of
                Casted      str -> [name p `T.append` "の" `T.append` str  `T.append` "!"]
                ManaConsumed am -> [name p `T.append` ": " `T.append` T.pack (show am)  `T.append` "マナ消費 \n"  `T.append` manaBar s p]
                ManaRestored am -> [name p `T.append` ": " `T.append` T.pack (show am)  `T.append` "マナ回復 \n"  `T.append` manaBar s p]
                Damaged      am -> [name p `T.append` "に" `T.append` T.pack (show am)  `T.append` "ダメージ! \n" `T.append` hpBar s p]
                Healed       am -> [name p `T.append` ": " `T.append` T.pack (show am)  `T.append` "回復! \n"     `T.append` hpBar s p]
                Buffed    bt am -> [name p `T.append` "の" `T.append` buffTypeToText bt `T.append` "が一時的に" `T.append` toFixed 2 am `T.append` "倍になった!"]
                x  -> [T.pack $ show x]) results

buffTypeToText bt =
    case bt of
        StAttack  -> "攻撃力"
        StDefense -> "防御力"

imDataDefault = ImData{
        totalDeath       = 0,
        totalDamageGiven = 0,
        totalDamageTaken = 0,
        totalCast        = 0,
        totalMana        = 0
    }

toDefault :: Settings -> Player -> Player
toDefault settings player = player{
        hp   = hpMax   settings player,
        mana = manaMax settings player,
        buff = []
    }

brandNewPlayer settings = (toDefault settings emptyPlayer){
        imData = imDataDefault
    }

loadFromJsonObj :: Settings -> Value -> Player
loadFromJsonObj settings obj = 
    (toDefault settings emptyPlayer){
        imData = fromMaybe imDataDefault $ parseMaybe parseJSON obj
    }

checkLife :: Settings -> Player -> Player ->Player
checkLife settings prev p = if (hp p > 0) || (hp prev <= 0) then p else p{ imData = let d = imData p in d{totalDeath = totalDeath d + 1} }

completePlayerData :: Settings -> [T.Text] -> [T.Text] -> Map T.Text Player -> Map T.Text Player
completePlayerData settings ids names pmap = L.foldl' (\m (i, n) -> case m !? i of
                                                                        Just p  -> M.insert i (p{name = n}) m
                                                                        Nothing -> M.insert i (brandNewPlayer settings){name = n} m) pmap (L.zip ids names)

dealTimeRelatedEff :: Settings -> POSIXTime -> Player -> Player
dealTimeRelatedEff s n p = let t       = realToFrac $ n - lastLoaded p
                               newmana = min (manaMax s p) (mana p + floor (t * manaRegenMin s /60))
                            in p{lastLoaded = n, mana = newmana}

decodeFileStrictIfExist :: FromJSON a => FilePath -> IO (Maybe a)
decodeFileStrictIfExist path = do
    e <- doesFileExist path
    if e then decodeFileStrict path else return Nothing

saveGameData :: T.Text -> Map T.Text Player -> Settings -> IO ()
saveGameData serverid players settings = TLIO.writeFile ("data_"++T.unpack serverid++".json") (encodeToLazyText $ object [ "settings" .= settings, "players" .= M.map imData players])

loadGameData :: T.Text -> [T.Text] -> [T.Text] -> Settings -> IO (Map T.Text Player, MagicEnv)
loadGameData serverid pids names settings = do
    obj <- decodeFileStrictIfExist ("data_"++T.unpack serverid++".json") :: IO (Maybe Object)
    now <- getPOSIXTime
    let players  = M.map (\p->p{lastLoaded = now}) 
                    $ completePlayerData settings pids names
                    $ M.map (loadFromJsonObj settings)
                    $ fromMaybe (M.empty :: M.Map T.Text Value) $ join $ parseMaybe (.:? "players") =<< obj
    env <- settingsToEnv settings
    return (players, env)

reLoadGameData :: T.Text -> [T.Text] -> [T.Text] -> Settings -> Map T.Text Player -> IO (Map T.Text Player, MagicEnv)
reLoadGameData serverid pids names settings pls= do
    now <- getPOSIXTime
    let players  = completePlayerData settings pids names (M.map (dealTimeRelatedEff settings now) pls)
    env <- settingsToEnv settings
    return (players, env)

withTimeElapsed :: (MonadIO m) => String -> m a -> m a
withTimeElapsed t f = do
    start <- liftIO getPOSIXTime
    r <- f
    end <- liftIO getPOSIXTime
    liftIO $ Prelude.putStrLn $ t++": "++show (end-start)
    return r