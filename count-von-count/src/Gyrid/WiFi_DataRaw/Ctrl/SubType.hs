{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Gyrid.WiFi_DataRaw.Ctrl.SubType (SubType(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data SubType = SubType_PSPOLL
             | SubType_OTHER
             deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable SubType
 
instance Prelude'.Bounded SubType where
  minBound = SubType_PSPOLL
  maxBound = SubType_OTHER
 
instance P'.Default SubType where
  defaultValue = SubType_PSPOLL
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe SubType
toMaybe'Enum 1 = Prelude'.Just SubType_PSPOLL
toMaybe'Enum 2 = Prelude'.Just SubType_OTHER
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum SubType where
  fromEnum SubType_PSPOLL = 1
  fromEnum SubType_OTHER = 2
  toEnum
   = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type Gyrid.WiFi_DataRaw.Ctrl.SubType") . toMaybe'Enum
  succ SubType_PSPOLL = SubType_OTHER
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Gyrid.WiFi_DataRaw.Ctrl.SubType"
  pred SubType_OTHER = SubType_PSPOLL
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Gyrid.WiFi_DataRaw.Ctrl.SubType"
 
instance P'.Wire SubType where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB SubType
 
instance P'.MessageAPI msg' (msg' -> SubType) SubType where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum SubType where
  reflectEnum = [(1, "SubType_PSPOLL", SubType_PSPOLL), (2, "SubType_OTHER", SubType_OTHER)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".Gyrid.WiFi_DataRaw.Ctrl.SubType") [] ["Gyrid", "WiFi_DataRaw", "Ctrl"] "SubType")
      ["Gyrid", "WiFi_DataRaw", "Ctrl", "SubType.hs"]
      [(1, "SubType_PSPOLL"), (2, "SubType_OTHER")]