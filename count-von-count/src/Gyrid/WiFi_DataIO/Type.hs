{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Gyrid.WiFi_DataIO.Type (Type(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data Type = Type_ACCESSPOINT
          | Type_DEVICE
          deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable Type
 
instance Prelude'.Bounded Type where
  minBound = Type_ACCESSPOINT
  maxBound = Type_DEVICE
 
instance P'.Default Type where
  defaultValue = Type_ACCESSPOINT
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe Type
toMaybe'Enum 1 = Prelude'.Just Type_ACCESSPOINT
toMaybe'Enum 2 = Prelude'.Just Type_DEVICE
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum Type where
  fromEnum Type_ACCESSPOINT = 1
  fromEnum Type_DEVICE = 2
  toEnum = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type Gyrid.WiFi_DataIO.Type") . toMaybe'Enum
  succ Type_ACCESSPOINT = Type_DEVICE
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Gyrid.WiFi_DataIO.Type"
  pred Type_DEVICE = Type_ACCESSPOINT
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Gyrid.WiFi_DataIO.Type"
 
instance P'.Wire Type where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB Type
 
instance P'.MessageAPI msg' (msg' -> Type) Type where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum Type where
  reflectEnum = [(1, "Type_ACCESSPOINT", Type_ACCESSPOINT), (2, "Type_DEVICE", Type_DEVICE)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".Gyrid.WiFi_DataIO.Type") [] ["Gyrid", "WiFi_DataIO"] "Type")
      ["Gyrid", "WiFi_DataIO", "Type.hs"]
      [(1, "Type_ACCESSPOINT"), (2, "Type_DEVICE")]