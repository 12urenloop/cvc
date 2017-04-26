{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Gyrid.WiFi_DataRaw.Mgmt.SubType (SubType(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data SubType = SubType_BEACON
             | SubType_PROBERESP
             | SubType_PROBEREQ
             | SubType_DEAUTH
             | SubType_DISAS
             | SubType_ATIM
             | SubType_ASSOREQ
             | SubType_ASSORESP
             | SubType_REASSOREQ
             | SubType_REASSORESP
             deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable SubType

instance Prelude'.Bounded SubType where
  minBound = SubType_BEACON
  maxBound = SubType_REASSORESP

instance P'.Default SubType where
  defaultValue = SubType_BEACON

toMaybe'Enum :: Prelude'.Int -> P'.Maybe SubType
toMaybe'Enum 1 = Prelude'.Just SubType_BEACON
toMaybe'Enum 2 = Prelude'.Just SubType_PROBERESP
toMaybe'Enum 3 = Prelude'.Just SubType_PROBEREQ
toMaybe'Enum 4 = Prelude'.Just SubType_DEAUTH
toMaybe'Enum 5 = Prelude'.Just SubType_DISAS
toMaybe'Enum 6 = Prelude'.Just SubType_ATIM
toMaybe'Enum 7 = Prelude'.Just SubType_ASSOREQ
toMaybe'Enum 8 = Prelude'.Just SubType_ASSORESP
toMaybe'Enum 9 = Prelude'.Just SubType_REASSOREQ
toMaybe'Enum 10 = Prelude'.Just SubType_REASSORESP
toMaybe'Enum _ = Prelude'.Nothing

instance Prelude'.Enum SubType where
  fromEnum SubType_BEACON = 1
  fromEnum SubType_PROBERESP = 2
  fromEnum SubType_PROBEREQ = 3
  fromEnum SubType_DEAUTH = 4
  fromEnum SubType_DISAS = 5
  fromEnum SubType_ATIM = 6
  fromEnum SubType_ASSOREQ = 7
  fromEnum SubType_ASSORESP = 8
  fromEnum SubType_REASSOREQ = 9
  fromEnum SubType_REASSORESP = 10
  toEnum
   = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type Gyrid.WiFi_DataRaw.Mgmt.SubType") . toMaybe'Enum
  succ SubType_BEACON = SubType_PROBERESP
  succ SubType_PROBERESP = SubType_PROBEREQ
  succ SubType_PROBEREQ = SubType_DEAUTH
  succ SubType_DEAUTH = SubType_DISAS
  succ SubType_DISAS = SubType_ATIM
  succ SubType_ATIM = SubType_ASSOREQ
  succ SubType_ASSOREQ = SubType_ASSORESP
  succ SubType_ASSORESP = SubType_REASSOREQ
  succ SubType_REASSOREQ = SubType_REASSORESP
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Gyrid.WiFi_DataRaw.Mgmt.SubType"
  pred SubType_PROBERESP = SubType_BEACON
  pred SubType_PROBEREQ = SubType_PROBERESP
  pred SubType_DEAUTH = SubType_PROBEREQ
  pred SubType_DISAS = SubType_DEAUTH
  pred SubType_ATIM = SubType_DISAS
  pred SubType_ASSOREQ = SubType_ATIM
  pred SubType_ASSORESP = SubType_ASSOREQ
  pred SubType_REASSOREQ = SubType_ASSORESP
  pred SubType_REASSORESP = SubType_REASSOREQ
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Gyrid.WiFi_DataRaw.Mgmt.SubType"

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
  reflectEnum
   = [(1, "SubType_BEACON", SubType_BEACON), (2, "SubType_PROBERESP", SubType_PROBERESP), (3, "SubType_PROBEREQ", SubType_PROBEREQ),
      (4, "SubType_DEAUTH", SubType_DEAUTH), (5, "SubType_DISAS", SubType_DISAS), (6, "SubType_ATIM", SubType_ATIM),
      (7, "SubType_ASSOREQ", SubType_ASSOREQ), (8, "SubType_ASSORESP", SubType_ASSORESP),
      (9, "SubType_REASSOREQ", SubType_REASSOREQ), (10, "SubType_REASSORESP", SubType_REASSORESP)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".Gyrid.WiFi_DataRaw.Mgmt.SubType") [] ["Gyrid", "WiFi_DataRaw", "Mgmt"] "SubType")
      ["Gyrid", "WiFi_DataRaw", "Mgmt", "SubType.hs"]
      [(1, "SubType_BEACON"), (2, "SubType_PROBERESP"), (3, "SubType_PROBEREQ"), (4, "SubType_DEAUTH"), (5, "SubType_DISAS"),
       (6, "SubType_ATIM"), (7, "SubType_ASSOREQ"), (8, "SubType_ASSORESP"), (9, "SubType_REASSOREQ"), (10, "SubType_REASSORESP")]

instance P'.TextType SubType where
  tellT = P'.tellShow
  getT = P'.getRead