{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Gyrid.WiFi_DataRaw.Mgmt.Beacon.Type (Type(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data Type = Type_ESS
          | Type_IBSS
          deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable Type

instance Prelude'.Bounded Type where
  minBound = Type_ESS
  maxBound = Type_IBSS

instance P'.Default Type where
  defaultValue = Type_ESS

toMaybe'Enum :: Prelude'.Int -> P'.Maybe Type
toMaybe'Enum 1 = Prelude'.Just Type_ESS
toMaybe'Enum 2 = Prelude'.Just Type_IBSS
toMaybe'Enum _ = Prelude'.Nothing

instance Prelude'.Enum Type where
  fromEnum Type_ESS = 1
  fromEnum Type_IBSS = 2
  toEnum
   = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type Gyrid.WiFi_DataRaw.Mgmt.Beacon.Type") .
      toMaybe'Enum
  succ Type_ESS = Type_IBSS
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Gyrid.WiFi_DataRaw.Mgmt.Beacon.Type"
  pred Type_IBSS = Type_ESS
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Gyrid.WiFi_DataRaw.Mgmt.Beacon.Type"

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
  reflectEnum = [(1, "Type_ESS", Type_ESS), (2, "Type_IBSS", Type_IBSS)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".Gyrid.WiFi_DataRaw.Mgmt.Beacon.Type") [] ["Gyrid", "WiFi_DataRaw", "Mgmt", "Beacon"] "Type")
      ["Gyrid", "WiFi_DataRaw", "Mgmt", "Beacon", "Type.hs"]
      [(1, "Type_ESS"), (2, "Type_IBSS")]

instance P'.TextType Type where
  tellT = P'.tellShow
  getT = P'.getRead