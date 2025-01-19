module DataTypes where

import Database.SQLite.Simple

data RawAdData = RawAdData
  { rawAdId :: Integer,
    rawSeller :: Integer,
    rawObjectId :: Integer,
    rawObjectType :: Integer,
    rawCost :: Float,
    rawDescription :: String,
    rawAddressId :: Integer,
    rawState :: String,
    rawCity :: String,
    rawDistrict :: String,
    rawPostalCode :: String,
    rawStreetName :: String,
    rawHouseNumber :: String,
    rawEntrance :: Maybe Integer,
    rawDoorNumber :: Maybe Integer,
    rawObjectArea :: Int
  }
  deriving (Show)

instance FromRow RawAdData where
  fromRow =
    RawAdData
      <$> field -- id
      <*> field -- seller
      <*> field -- objectId
      <*> field -- objectType
      <*> field -- cost
      <*> field -- description
      <*> field -- addressId
      <*> field -- state
      <*> field -- city
      <*> field -- district
      <*> field -- postalCode
      <*> field -- streetName
      <*> field -- houseNumber
      <*> field -- entrance
      <*> field -- doorNumber
      <*> field -- objectArea