{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Gyrid.StateScanning.Type (Type(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data Type = Type_STARTED
          | Type_STOPPED
          deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable Type

instance Prelude'.Bounded Type where
  minBound = Type_STARTED
  maxBound = Type_STOPPED

instance P'.Default Type where
  defaultValue = Type_STARTED

toMaybe'Enum :: Prelude'.Int -> P'.Maybe Type
toMaybe'Enum 1 = Prelude'.Just Type_STARTED
toMaybe'Enum 2 = Prelude'.Just Type_STOPPED
toMaybe'Enum _ = Prelude'.Nothing

instance Prelude'.Enum Type where
  fromEnum Type_STARTED = 1
  fromEnum Type_STOPPED = 2
  toEnum = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type Gyrid.StateScanning.Type") . toMaybe'Enum
  succ Type_STARTED = Type_STOPPED
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Gyrid.StateScanning.Type"
  pred Type_STOPPED = Type_STARTED
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Gyrid.StateScanning.Type"

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
  reflectEnum = [(1, "Type_STARTED", Type_STARTED), (2, "Type_STOPPED", Type_STOPPED)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".Gyrid.StateScanning.Type") [] ["Gyrid", "StateScanning"] "Type")
      ["Gyrid", "StateScanning", "Type.hs"]
      [(1, "Type_STARTED"), (2, "Type_STOPPED")]

instance P'.TextType Type where
  tellT = P'.tellShow
  getT = P'.getRead