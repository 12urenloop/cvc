{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Gyrid.ScanPattern.Action (Action(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data Action = Action_ADD
            | Action_REMOVE
            | Action_REMOVEALL
            deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable Action
 
instance Prelude'.Bounded Action where
  minBound = Action_ADD
  maxBound = Action_REMOVEALL
 
instance P'.Default Action where
  defaultValue = Action_ADD
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe Action
toMaybe'Enum 1 = Prelude'.Just Action_ADD
toMaybe'Enum 2 = Prelude'.Just Action_REMOVE
toMaybe'Enum 3 = Prelude'.Just Action_REMOVEALL
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum Action where
  fromEnum Action_ADD = 1
  fromEnum Action_REMOVE = 2
  fromEnum Action_REMOVEALL = 3
  toEnum = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type Gyrid.ScanPattern.Action") . toMaybe'Enum
  succ Action_ADD = Action_REMOVE
  succ Action_REMOVE = Action_REMOVEALL
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Gyrid.ScanPattern.Action"
  pred Action_REMOVE = Action_ADD
  pred Action_REMOVEALL = Action_REMOVE
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Gyrid.ScanPattern.Action"
 
instance P'.Wire Action where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB Action
 
instance P'.MessageAPI msg' (msg' -> Action) Action where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum Action where
  reflectEnum = [(1, "Action_ADD", Action_ADD), (2, "Action_REMOVE", Action_REMOVE), (3, "Action_REMOVEALL", Action_REMOVEALL)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".Gyrid.ScanPattern.Action") [] ["Gyrid", "ScanPattern"] "Action")
      ["Gyrid", "ScanPattern", "Action.hs"]
      [(1, "Action_ADD"), (2, "Action_REMOVE"), (3, "Action_REMOVEALL")]