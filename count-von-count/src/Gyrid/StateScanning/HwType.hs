{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Gyrid.StateScanning.HwType (HwType(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data HwType = HwType_BLUETOOTH
            | HwType_WIFI
            deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable HwType
 
instance Prelude'.Bounded HwType where
  minBound = HwType_BLUETOOTH
  maxBound = HwType_WIFI
 
instance P'.Default HwType where
  defaultValue = HwType_BLUETOOTH
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe HwType
toMaybe'Enum 1 = Prelude'.Just HwType_BLUETOOTH
toMaybe'Enum 2 = Prelude'.Just HwType_WIFI
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum HwType where
  fromEnum HwType_BLUETOOTH = 1
  fromEnum HwType_WIFI = 2
  toEnum = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type Gyrid.StateScanning.HwType") . toMaybe'Enum
  succ HwType_BLUETOOTH = HwType_WIFI
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Gyrid.StateScanning.HwType"
  pred HwType_WIFI = HwType_BLUETOOTH
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Gyrid.StateScanning.HwType"
 
instance P'.Wire HwType where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB HwType
 
instance P'.MessageAPI msg' (msg' -> HwType) HwType where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum HwType where
  reflectEnum = [(1, "HwType_BLUETOOTH", HwType_BLUETOOTH), (2, "HwType_WIFI", HwType_WIFI)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".Gyrid.StateScanning.HwType") [] ["Gyrid", "StateScanning"] "HwType")
      ["Gyrid", "StateScanning", "HwType.hs"]
      [(1, "HwType_BLUETOOTH"), (2, "HwType_WIFI")]