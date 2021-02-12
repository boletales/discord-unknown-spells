{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib where

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
        pepper       :: T.Text,
        manaStart    :: Int,
        hpStart      :: Int,
        buffPow      :: Double,
        damagePow    :: Double,
        damageMag    :: Double,
        healPow      :: Double,
        healMag      :: Double,
        buffLifeSpan :: Double
    } deriving (Show, Generic)

instance FromJSON Settings
instance ToJSON   Settings

defaultSettings = Settings {
        pepper        = "",
        hpStart       = 300,
        manaStart     = 300,
        buffPow       = 2,
        damagePow     = 4,
        damageMag     = 50,
        healPow       = 4,
        healMag       = 50,
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

data SpellResult = ManaConsumed Int | ManaRestored Int | Damaged Int | Healed Int | Buffed BuffType Double

data BuffType  = StAttack | StDefense
                    deriving (Show, Eq, Enum)

data SpellType = Attack | Heal | Buff BuffType
                    deriving (Show, Eq)

data SpellData = SpellData {
        spelltype :: SpellType,
        power     :: Int
    } deriving (Show, Eq)

data Player = Player {
        hp   :: Int,
        mana :: Int,
        name :: T.Text,
        buff :: [(BuffType, Double, POSIXTime)],
        imData :: ImData,
        lastLoaded   :: POSIXTime 
    } deriving (Show, Eq)

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
                spelltype = st,
                power     = pw
            }

intToPowDouble :: Double -> Int -> Double
intToPowDouble mag int = mag ** (fromIntegral int / (2^32) - 0.5)


calcBuffMag env buffType player = 
    (L.product
        $ L.map    (\(bt, po, pt, ag, ls) -> po * (1- ag/ls) ) 
        $ L.filter (\(bt, po, pt, ag, ls) -> bt == buffType && ag < ls ) 
        $ L.map    (\(bt, po, pt) -> (bt, po, pt, realToFrac (10^12 * nominalDiffTimeToSeconds (time env - pt)), buffLifeSpan (settings env)) )
        $ buff player)

calcDamage :: MagicEnv -> SpellData -> Player -> Player -> Int
calcDamage env spell source target = 
    let saBuff = calcBuffMag env StAttack  source
        tdBuff = calcBuffMag env StDefense target
        baseDamage = (damageMag $ settings env) * intToPowDouble (damagePow $ settings env) (power spell)
    in double2Int $ baseDamage * saBuff / tdBuff

calcHeal env spell source target = 
    let saBuff = calcBuffMag env StAttack  source
        tdBuff = calcBuffMag env StDefense target
        baseDamage = (damageMag $ settings env) * intToPowDouble (damagePow $ settings env) (power spell)
    in double2Int $ baseDamage * saBuff

calcCost spell = double2Int $ intToPowDouble 6 (power spell) * 
                                case spelltype spell of
                                    Attack -> 30
                                    Heal   -> 50
                                    Buff t -> 50

castSideEff env player = player{ imData = let d = imData player in d{totalCast = totalCast d + 1}}

consumeMana env cost player = player{mana   = mana player - cost,
                                     imData = let d = imData player in d{totalMana = totalMana d + cost}}

dealDamage env damageam player = player{ hp     = hp player - damageam,
                                         imData = let d = imData player in d {totalDamageTaken = totalDamageTaken d + damageam}}

dealAttack env damageam player = player{ imData = let d = imData player in d {totalDamageGiven = totalDamageGiven d + damageam}}

dealHeal env healam player = player{ hp = min (hpStart $ settings env) (hp player + healam)}
dealBuff env t buffam player = player{ buff = (t, buffam, time env) : buff player }
                                     
tryCast env spell source target = 
    let cost = calcCost spell
        paid = castSideEff env $ consumeMana env cost source
        s = settings env
    in  (\((p1,p2),r) -> ((checkLife s source p1,checkLife s target p2),r)) <$>
        if mana source < cost
        then Nothing
        else case spelltype spell of
                Attack -> Just (let damageam = calcDamage env spell source target in ((dealAttack env damageam paid, dealDamage env damageam target), ([ManaConsumed cost], [Damaged damageam])))
                Heal   -> Just (let healam   = calcHeal   env spell source target in ((paid, dealHeal env healam target), ([ManaConsumed cost], [Healed  healam  ])))
                Buff t -> Just (let buffam   = buffPow (settings env) ** fromIntegral (power spell) in ((paid, dealBuff env t buffam target), ([ManaConsumed cost], [Buffed t buffam])))

doCast env spellstr sourceid targetid players =
    let source = players !? sourceid
        target = players !? targetid
        spell  = getSpell spellstr (pepper $ settings env)
        mr     = join $ tryCast env spell <$> source <*> target
    in case mr of
        Nothing -> (players, ["詠唱失敗……"])
        Just ((ns,nt),(lrs,lrt)) ->  (M.insert sourceid ns $ M.insert targetid nt players , join [spellResultToText ns lrs ,spellResultToText nt lrt])
        
spellResultToText p results =
    L.map (name p `T.append` ": " `T.append`)
    $ join $ L.map 
        (\r ->  case r of
                    ManaConsumed am -> [T.pack (show am) `T.append` "マナ消費"]
                    ManaRestored am -> [T.pack (show am) `T.append` "マナ回復"]
                    Damaged      am -> [T.pack (show am) `T.append` "ダメージ!"]
                    Healed       am -> [T.pack (show am) `T.append` "回復!"]
                    Buffed    bt am -> [buffTypeToText bt `T.append` "が一時的に" `T.append` toFixed (-2) am `T.append` "倍になった!"]) results

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

brandNewPlayer settings = (toDefault settings Player{}){
        imData = imDataDefault
    }

loadFromJsonObj :: Settings -> Value -> Player
loadFromJsonObj settings obj = 
    (toDefault settings Player{}){
        imData = fromMaybe imDataDefault $ parseMaybe parseJSON obj
    }

checkLife :: Settings -> Player -> Player ->Player
checkLife settings prev p = if (hp p > 0) || (hp prev <= 0) then p else p{ imData = let d = imData p in d{totalDeath = totalDeath d + 1} }

completePlayerData :: Settings -> [T.Text] -> [T.Text] -> Map T.Text Player -> Map T.Text Player
completePlayerData settings ids names pmap = L.foldl' (\m (i, n) -> case m !? i of
                                                                        Just p  -> M.insert i (p{name = n}) m
                                                                        Nothing -> M.insert i (brandNewPlayer settings){name = n} m) pmap (L.zip ids names)

saveGameData :: T.Text -> Map T.Text Player -> Settings -> IO ()
saveGameData serverid players settings = TLIO.writeFile ("data_"++T.unpack serverid++".json") (encodeToLazyText $ object [ "settings" .= settings, "players" .= M.map imData players])

loadGameData :: T.Text -> [T.Text] -> [T.Text] -> IO (Map T.Text Player, MagicEnv)
loadGameData serverid pids names = do
    obj <- decodeFileStrict ("data_"++T.unpack serverid++".json") :: IO (Maybe Object)
    let settings = fromMaybe defaultSettings $ join $ parseMaybe (.:? "settings") =<< obj
    let players  = completePlayerData settings pids names
                    $ M.map (loadFromJsonObj settings)
                    $ fromMaybe (M.empty :: M.Map T.Text Value) $ join $ parseMaybe (.:? "players") =<< obj
    env <- settingsToEnv settings
    return (players, env)

withTimeElapsed :: String -> IO a -> IO a
withTimeElapsed t f = do
    start <- getPOSIXTime
    r <- f
    end <- getPOSIXTime
    Prelude.putStrLn $ t++": "++show (end-start)
    return r