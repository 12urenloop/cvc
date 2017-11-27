{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Gyrid.WiFi_DataRaw.FrameType (FrameType(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data FrameType = FrameType_MGMT
               | FrameType_CTRL
               | FrameType_DATA
               deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data,
                         Prelude'.Generic)

instance P'.Mergeable FrameType

instance Prelude'.Bounded FrameType where
  minBound = FrameType_MGMT
  maxBound = FrameType_DATA

instance P'.Default FrameType where
  defaultValue = FrameType_MGMT

toMaybe'Enum :: Prelude'.Int -> P'.Maybe FrameType
toMaybe'Enum 1 = Prelude'.Just FrameType_MGMT
toMaybe'Enum 2 = Prelude'.Just FrameType_CTRL
toMaybe'Enum 3 = Prelude'.Just FrameType_DATA
toMaybe'Enum _ = Prelude'.Nothing

instance Prelude'.Enum FrameType where
  fromEnum FrameType_MGMT = 1
  fromEnum FrameType_CTRL = 2
  fromEnum FrameType_DATA = 3
  toEnum
   = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type Gyrid.WiFi_DataRaw.FrameType") . toMaybe'Enum
  succ FrameType_MGMT = FrameType_CTRL
  succ FrameType_CTRL = FrameType_DATA
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Gyrid.WiFi_DataRaw.FrameType"
  pred FrameType_CTRL = FrameType_MGMT
  pred FrameType_DATA = FrameType_CTRL
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Gyrid.WiFi_DataRaw.FrameType"

instance P'.Wire FrameType where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'

instance P'.GPB FrameType

instance P'.MessageAPI msg' (msg' -> FrameType) FrameType where
  getVal m' f' = f' m'

instance P'.ReflectEnum FrameType where
  reflectEnum
   = [(1, "FrameType_MGMT", FrameType_MGMT), (2, "FrameType_CTRL", FrameType_CTRL), (3, "FrameType_DATA", FrameType_DATA)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".Gyrid.WiFi_DataRaw.FrameType") [] ["Gyrid", "WiFi_DataRaw"] "FrameType")
      ["Gyrid", "WiFi_DataRaw", "FrameType.hs"]
      [(1, "FrameType_MGMT"), (2, "FrameType_CTRL"), (3, "FrameType_DATA")]

instance P'.TextType FrameType where
  tellT = P'.tellShow
  getT = P'.getRead