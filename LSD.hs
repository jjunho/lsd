{-# LANGUAGE FlexibleInstances #-}

module LSD where

import Data.List (intercalate)

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

value :: Num a => CoinNote -> a
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

toCoins :: (Ord a, Num a) => a -> [CoinNote]
toCoins i
  | i >= value TenPound    = TenPound    : toCoins (i - value TenPound)
  | i >= value FivePound   = FivePound   : toCoins (i - value FivePound)
  | i >= value OnePound    = OnePound    : toCoins (i - value OnePound)
  | i >= value TenShilling = TenShilling : toCoins (i - value TenShilling)
  | i >= value Guinea      = Guinea      : toCoins (i - value Guinea)
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
  | otherwise = error "Error"

data LSD = LSD Int Int Int Int
  deriving (Show, Eq)

class Txt a where
  txt :: a -> String

instance Txt LSD where
  txt lsd = case lsd of
    (LSD 0 0 0 0) -> "0"
    (LSD 0 0 0 f) -> fraction f <> "d."
    (LSD 0 0 d 0) -> show d <> "d."
    (LSD 0 0 d f) -> show d <> fraction f <> "d."
    (LSD 0 s 0 0) -> show s <> "/-"
    (LSD 0 s 0 f) -> show s <> "/" <> fraction f <> "d."
    (LSD 0 s d 0) -> show s <> "/" <> show d
    (LSD 0 s d f) -> show s <> "/" <> show d <> fraction f
    (LSD l 0 0 0) -> "£" <> show l
    (LSD l 0 0 f) -> "£" <> show l <> fraction f <> "d."
    (LSD l s 0 0) -> "£" <> show l <> " " <> show s <> "/-"
    (LSD l s 0 f) -> "£" <> show l <> " " <> show s <> "/" <> fraction f
    (LSD l 0 d 0) -> "£" <> show l <> " " <> show d <> "d."
    (LSD l 0 d f) -> "£" <> show l <> " " <> show d <> fraction f <> "d."
    (LSD l s d 0) -> "£" <> show l <> " " <> show s <> "/" <> show d
    (LSD l s d f) -> "£" <> show l <> " " <> show s <> "/" <> show d <> fraction f

fraction :: Int -> String
fraction 1 = "¼"
fraction 2 = "½"
fraction 3 = "¾"
fraction _ = error "Impossible"

instance Txt [CoinNote] where
  txt coins = intercalate ", " (txt <$> coins)

instance Txt CoinNote where
  txt = show

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
  Guinea      -> [OnePound,Shilling]
  TenShilling -> [Crown,Crown]
  OnePound    -> [TenShilling,TenShilling]
  FivePound   -> [OnePound,OnePound,OnePound,OnePound,OnePound]
  TenPound    -> [FivePound,FivePound]

instance Semigroup LSD where
  LSD l1 s1 d1 f1 <> LSD l2 s2 d2 f2 = mkLSD (l1 + l2) (s1 + s2) (d1 + d2) (f1 + f2)

mkLSD :: Int -> Int -> Int -> Int -> LSD
mkLSD l s d f
  | s >= 20 = mkLSD (l + s `div` 20) (s `rem` 20) d f
  | d >= 12 = mkLSD l (s + d `div` 12) (d `rem` 12) f
  | f >= 4  = mkLSD l s (d + f `div` 4) (f `rem` 4)
  | otherwise = LSD l s d f

toFarthings :: LSD -> Int
toFarthings (LSD l s d f) = (l * 4 * 240) + (s * 4 * 12) + (4 * d) + f

putPriceCoin :: Int -> Int -> Int -> Int -> IO ()
putPriceCoin l s d f = putStrLn $ "Price: " <> (txt $ mkLSD l s d f) <>
                                  "\nCoins: " <> (txt (toCoins $ toFarthings $ mkLSD l s d f))
