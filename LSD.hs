{-# LANGUAGE FlexibleInstances #-}

module LSD where

import Data.List (intercalate)

data CoinNote
  = Penny
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
  Penny       ->  1
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
  | i == 0                 = []
  | otherwise = error "Error"

data LSD = LSD Int Int Int
  deriving (Show, Eq)

class Txt a where
  txt :: a -> String

instance Txt LSD where
  txt lsd = case lsd of
    (LSD 0 0 d) -> show d <> "d."
    (LSD 0 s 0) -> show s <> "/-"
    (LSD 0 s d) -> show s <> "/" <> show d
    (LSD l 0 0) -> "£" <> show l
    (LSD l s 0) -> "£" <> show l <> " " <> show s <> "/-"
    (LSD l 0 d) -> "£" <> show l <> " " <> show d <> "d."
    (LSD l s d) -> "£" <> show l <> " " <> show s <> "/" <> show d

instance Txt [CoinNote] where
  txt coins = intercalate ", " (txt <$> coins)

instance Txt CoinNote where
  txt = show

change :: CoinNote -> [CoinNote]
change coin = case coin of
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
  LSD l1 s1 d1 <> LSD l2 s2 d2 = mkLSD (l1 + l2) (s1 + s2) (d1 + d2)

mkLSD :: Int -> Int -> Int -> LSD
mkLSD l s d
  | s >= 20 = mkLSD (l + s `div` 20) (s `rem` 20) d
  | d >= 12 = mkLSD l (s + d `div` 12) (d `rem` 12)
  | otherwise = LSD l s d

toPence :: LSD -> Int
toPence (LSD l s d) = (l * 240) + (s * 12) + d

putPriceCoin :: Int -> Int -> Int -> IO ()
putPriceCoin l s d = putStrLn $ "Price: " <> (txt $ mkLSD l s d) <>
                                "\nCoins: " <> (txt (toCoins $ toPence $ mkLSD l s d))
