{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Gyrid.Bluetooth_DataIO.Move (Move(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data Move = Move_IN
          | Move_OUT
          deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable Move
 
instance Prelude'.Bounded Move where
  minBound = Move_IN
  maxBound = Move_OUT
 
instance P'.Default Move where
  defaultValue = Move_IN
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe Move
toMaybe'Enum 1 = Prelude'.Just Move_IN
toMaybe'Enum 2 = Prelude'.Just Move_OUT
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum Move where
  fromEnum Move_IN = 1
  fromEnum Move_OUT = 2
  toEnum
   = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type Gyrid.Bluetooth_DataIO.Move") . toMaybe'Enum
  succ Move_IN = Move_OUT
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Gyrid.Bluetooth_DataIO.Move"
  pred Move_OUT = Move_IN
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Gyrid.Bluetooth_DataIO.Move"
 
instance P'.Wire Move where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB Move
 
instance P'.MessageAPI msg' (msg' -> Move) Move where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum Move where
  reflectEnum = [(1, "Move_IN", Move_IN), (2, "Move_OUT", Move_OUT)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".Gyrid.Bluetooth_DataIO.Move") [] ["Gyrid", "Bluetooth_DataIO"] "Move")
      ["Gyrid", "Bluetooth_DataIO", "Move.hs"]
      [(1, "Move_IN"), (2, "Move_OUT")]