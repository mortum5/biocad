{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Type where

import GHC.Generics
import Data.Default
import           Data.Aeson (ToJSON (..), object, (.=))
import           Data.Text    (Text, pack)

import Database.Bolt (Record, Value (..), Relationship (..), RecordValue (..), Node (..), at)

data Chem = ChM Molecule | ChR Reaction deriving (Show, Eq, ToJSON, Generic)

data Molecule = Molecule { id :: Int
                         , smiles :: Text
                         , iupacName :: Text
                         } deriving (Show, Eq, ToJSON, Generic)

instance Default Text where
  def = pack ""

instance Default Molecule where
  def = Molecule def def def

data Catalyst = Catalyst { id :: Int
                         , smiles :: Text
                         , name :: Maybe Text
                         } deriving (Show, Eq, ToJSON, Generic)

instance Default Catalyst where
  def = Catalyst def def Nothing

data Reaction = Reaction { id :: Int
                         , name :: Text
                         , reagentOne :: ReagentIn
                         , reagentTwo :: ReagentIn
                         , accelerate :: Accelerate
                         , productFrom :: ProductFrom
                         } deriving (Show, Eq, ToJSON, Generic)

instance Default Reaction where
  def = Reaction def def def def def def

data ProductFrom = ProductFrom { amount :: Double
                               , molecule :: Molecule
                               } deriving (Show, Eq, ToJSON, Generic)

instance Default ProductFrom where
  def = ProductFrom def def

data ReagentIn = ReagentIn { molecule :: Molecule
                           } deriving (Show, Eq, ToJSON, Generic)

instance Default ReagentIn where
  def = ReagentIn def

data Accelerate = Accelerate { temperature :: Double
                             , pressure :: Double
                             , catalyst :: Catalyst
                             } deriving (Show, Eq, ToJSON, Generic)

instance Default Accelerate where
  def = Accelerate def def def

instance ToJSON Value where
  toJSON (N _) = toJSON ()
  toJSON (B b) = toJSON b
  toJSON (I i) = toJSON i
  toJSON (F d) = toJSON d
  toJSON (T t) = toJSON t
  toJSON (L l) = toJSON l
  toJSON _     = undefined  -- we do not need Maps and Structures in this example

toChem :: Monad m => Node -> m Chem
toChem n | (labels n == ["Molecule"]) = fmap (ChM) (toMolecule n)
         | otherwise                  = fmap (ChR) (toReact n >>= (\x -> return $ x def def def def))

toReaction :: Monad m => [Record] -> m Reaction
toReaction rec = do reaction <- first `at` "re" >>= exact >>= toReact
                    catalyst <- first `at` "ct" >>= toCatalyst
                    mr       <- first `at` "mr" >>= exact >>= toMolecule
                    mO       <- ReagentIn <$> (first `at` "ml" >>= exact >>= toMolecule)
                    mT       <- ReagentIn <$> (second `at` "ml" >>= exact >>= toMolecule)
                    a        <- first `at` "rc" >>= toAccelerate
                    pf       <- first `at` "rm" >>= toProductFrom
                    return $ reaction mO mT (a catalyst) (pf mr)
  where
    first = head rec
    second = last rec

toMolecule :: (Monad m) => Node -> m Molecule
toMolecule n = do let props = nodeProps n
                  id :: Int <- (props `at` "id") >>= exact
                  smiles :: Text <- (props `at` "smiles") >>= exact
                  iupac :: Text <- (props `at` "iupacName") >>= exact
                  return $ Molecule id smiles iupac


toCatalyst c = do res :: Node <- exact c
                  let props = nodeProps res
                  id :: Int <- (props `at` "id") >>= exact
                  smiles :: Text <- (props `at` "smiles") >>= exact
                  name :: Text <- (props `at` "name") >>= exact
                  return $ Catalyst id smiles (stringToMaybe name)

toReact r = do let props = nodeProps r
               id :: Int <- (props `at` "id") >>= exact
               name :: Text <- (props `at` "name") >>= exact
               return $ Reaction (id) (name)

toAccelerate a = do res :: Relationship <- exact a
                    let props = relProps res
                    pressure :: Double <- (props `at` "pressure") >>= exact
                    temperature :: Double <- (props `at` "temperature") >>= exact
                    return $ Accelerate pressure temperature

toProductFrom p = do res :: Relationship <- exact p
                     let props = relProps res
                     amount :: Double <- (props `at` "amount") >>= exact
                     return $ ProductFrom amount

createSimpleReaction :: Int -> Reaction
createSimpleReaction x = Reaction x (help "name" x) rO rT a pF
  where
    rO = ReagentIn m1
    rT = ReagentIn m2
    a  = Accelerate ((fromIntegral $ 20 + x)) (fromIntegral (460 + x) ) c
    pF = ProductFrom (fromIntegral $ 3 + x) m3
    m1 = Molecule (x*10) (help "smiles" (x*10)) (help "iupac" (x*10))
    m2 = Molecule (x*10+1) (help "smiles" (x*10+1)) (help "iupac" (x*10+1))
    m3 = Molecule (x*10+2) (help "smiles" (x*10+2)) (help "iupac" (x*10+2))
    c  = Catalyst x (help "smiles" x) (Just $ help "name" x)

class ToParams a where
  toParams :: a -> Int -> [(Text, Value)]

instance ToParams Reaction where
  toParams (Reaction id name rO rT a pF) x = [(help "id" 5, I id),(help "name" 5, T name)] ++ (toParams rO 1) ++ (toParams rT 2) ++ (toParams pF 3) ++ (toParams a 4)

instance ToParams Molecule where
  toParams (Molecule id smiles iupac) x = [(help "id" x, I id),(help "smiles" x, T smiles), (help "iupac" x, T iupac)]

instance ToParams Catalyst where
  toParams (Catalyst id smiles name) x = [(help "id" x, I id), (help "smiles" x, T smiles), (help "name" x, T $ maybeToText name)]

instance ToParams ReagentIn where
  toParams (ReagentIn m) x = toParams m x

instance ToParams Accelerate where
  toParams (Accelerate t p c) x = [("temperature", F t),("pressure", F p)] ++ (toParams c x)

instance ToParams ProductFrom where
  toParams (ProductFrom a m) x = [("amount", F a)] ++ (toParams m x)

help s x = pack $ s ++ show x
maybeToText x = case x of
    Nothing -> ""
    Just s  -> s

stringToMaybe :: Text -> Maybe Text
stringToMaybe "" = Nothing
stringToMaybe x  = Just x
