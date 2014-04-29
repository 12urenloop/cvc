{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Gyrid.StateGyrid.Type (Type(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data Type = Type_CONNECTED
          | Type_DISCONNECTED
          deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable Type
 
instance Prelude'.Bounded Type where
  minBound = Type_CONNECTED
  maxBound = Type_DISCONNECTED
 
instance P'.Default Type where
  defaultValue = Type_CONNECTED
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe Type
toMaybe'Enum 1 = Prelude'.Just Type_CONNECTED
toMaybe'Enum 2 = Prelude'.Just Type_DISCONNECTED
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum Type where
  fromEnum Type_CONNECTED = 1
  fromEnum Type_DISCONNECTED = 2
  toEnum = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type Gyrid.StateGyrid.Type") . toMaybe'Enum
  succ Type_CONNECTED = Type_DISCONNECTED
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Gyrid.StateGyrid.Type"
  pred Type_DISCONNECTED = Type_CONNECTED
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Gyrid.StateGyrid.Type"
 
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
  reflectEnum = [(1, "Type_CONNECTED", Type_CONNECTED), (2, "Type_DISCONNECTED", Type_DISCONNECTED)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".Gyrid.StateGyrid.Type") [] ["Gyrid", "StateGyrid"] "Type")
      ["Gyrid", "StateGyrid", "Type.hs"]
      [(1, "Type_CONNECTED"), (2, "Type_DISCONNECTED")]