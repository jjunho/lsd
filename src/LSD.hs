{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module LSD (
    CoinNote
  , Pounds
  , Shillings
  , Pence
  , Farthings
  , value
  , toCoins
  , mkLSD
  , txt
  , toFarthings
  , coinsLSD
  , putPriceCoin
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type Txt = T.Text

tshow :: Show a => a -> Txt
tshow = T.pack . show

type Pounds = Int
type Shillings = Int
type Pence = Int
type Farthings = Int

data CoinNote
  = Farthing
  | Halfpenny
  | Penny
  | Threepence
  | Sixpence
  | Shilling
  | Florin
  | HalfCrown
  | Crown
  | Guinea
  | TenShilling
  | OnePound
  | FivePound
  | TenPound
  deriving (Show, Eq, Enum, Bounded)

value :: CoinNote -> Farthings
value c = case c of
  Farthing    ->  1
  Halfpenny   ->  2
  Penny       ->  1 * 4
  Threepence  ->  3 * value Penny
  Sixpence    ->  6 * value Penny
  Shilling    -> 12 * value Penny
  Florin      ->  2 * value Shilling
  HalfCrown   ->  2 * value Shilling + 6 * value Penny
  Crown       ->  5 * value Shilling
  Guinea      -> 21 * value Shilling
  TenShilling -> 10 * value Shilling
  OnePound    -> 20 * value Shilling
  FivePound   ->  5 * value OnePound
  TenPound    -> 10 * value OnePound

toCoins :: Farthings -> [CoinNote]
toCoins i
  | i >= value TenPound    = TenPound    : toCoins (i - value TenPound)
  | i >= value FivePound   = FivePound   : toCoins (i - value FivePound)
  | i >= value OnePound    = OnePound    : toCoins (i - value OnePound)
  | i >= value TenShilling = TenShilling : toCoins (i - value TenShilling)
  | i >= value Crown       = Crown       : toCoins (i - value Crown)
  | i >= value HalfCrown   = HalfCrown   : toCoins (i - value HalfCrown)
  | i >= value Florin      = Florin      : toCoins (i - value Florin)
  | i >= value Shilling    = Shilling    : toCoins (i - value Shilling)
  | i >= value Sixpence    = Sixpence    : toCoins (i - value Sixpence)
  | i >= value Threepence  = Threepence  : toCoins (i - value Threepence)
  | i >= value Penny       = Penny       : toCoins (i - value Penny)
  | i >= value Halfpenny   = Halfpenny   : toCoins (i - value Halfpenny)
  | i >= value Farthing    = Farthing    : toCoins (i - value Farthing)
  | i == 0                 = []

data LSD = LSD Pounds Shillings Pence Farthings
  deriving (Show, Eq)

class ToTxt a where
  txt :: a -> Txt

instance ToTxt LSD where
  txt lsd = case lsd of
    (LSD 0 0 0 0) -> "0"
    (LSD 0 0 0 f) -> fraction f <> "d."
    (LSD 0 0 d 0) -> txt d <> "d."
    (LSD 0 0 d f) -> txt d <> fraction f <> "d."
    (LSD 0 s 0 0) -> txt s <> "/-"
    (LSD 0 s 0 f) -> txt s <> "/" <> fraction f
    (LSD 0 s d 0) -> txt s <> "/" <> tshow d
    (LSD 0 s d f) -> txt s <> "/" <> tshow d <> fraction f
    (LSD l 0 0 0) -> "£" <> txt l
    (LSD l 0 0 f) -> "£" <> txt l <> fraction f <> "d."
    (LSD l 0 d 0) -> "£" <> txt l <> " " <> txt d <> "d."
    (LSD l 0 d f) -> "£" <> txt l <> " " <> txt d <> fraction f <> "d."
    (LSD l s 0 0) -> "£" <> txt l <> " " <> txt s <> "/-"
    (LSD l s 0 f) -> "£" <> txt l <> " " <> txt s <> "/" <> fraction f
    (LSD l s d 0) -> "£" <> txt l <> " " <> txt s <> "/" <> txt d
    (LSD l s d f) -> "£" <> txt l <> " " <> txt s <> "/" <> txt d <> fraction f

fraction :: Farthings -> Txt
fraction 1 = "¼"
fraction 2 = "½"
fraction 3 = "¾"

instance ToTxt [CoinNote] where
  txt coins = T.intercalate ", " (txt <$> coins)

instance ToTxt CoinNote where
  txt = tshow

instance ToTxt Int where
  txt = tshow

change :: CoinNote -> [CoinNote]
change coin = case coin of
  Farthing    -> [Farthing]
  Halfpenny   -> [Farthing,Farthing]
  Penny       -> [Halfpenny,Halfpenny]
  Threepence  -> [Penny,Penny,Penny]
  Sixpence    -> [Threepence,Threepence]
  Shilling    -> [Sixpence,Sixpence]
  Florin      -> [Shilling,Shilling]
  HalfCrown   -> [Florin,Sixpence]
  Crown       -> [HalfCrown,Florin]
  TenShilling -> [Crown,Crown]
  OnePound    -> [TenShilling,TenShilling]
  Guinea      -> [OnePound,Shilling]
  FivePound   -> [OnePound,OnePound,OnePound,OnePound,OnePound]
  TenPound    -> [FivePound,FivePound]

instance Semigroup LSD where
  LSD l1 s1 d1 f1 <> LSD l2 s2 d2 f2 = mkLSD (l1 + l2) (s1 + s2) (d1 + d2) (f1 + f2)

sInL :: Shillings
sInL = 20

dInS :: Pence
dInS = 12

fInD :: Farthings
fInD = 4

mkLSD :: Pounds -> Shillings -> Pence -> Farthings -> LSD
mkLSD l s d f
  | s >= sInL = mkLSD (l + s `div` sInL) (s `rem` sInL)      d f
  | d >= dInS = mkLSD  l                 (s + d `div` dInS) (d `rem` dInS)      f
  | f >= fInD = mkLSD  l                  s                 (d + f `div` fInD) (f `rem` fInD)
  | otherwise = LSD    l                  s                  d                  f

toFarthings :: LSD -> Farthings
toFarthings (LSD l s d f) = (l * 4 * 240) + (s * 4 * 12) + (4 * d) + f

putPriceCoin :: Pounds -> Shillings -> Pence -> Farthings -> IO ()
putPriceCoin l s d f = TIO.putStrLn $ "Price: " <> (txt $ mkLSD l s d f) <>
                                    "\nCoins: " <> (txt (toCoins $ toFarthings $ mkLSD l s d f))

coinsLSD :: [CoinNote] -> LSD
coinsLSD =  mkLSD 0 0 0 . sum . fmap value
