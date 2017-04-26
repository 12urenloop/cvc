{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Gyrid.Msg.Type (Type(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data Type = Type_ACK
          | Type_KEEPALIVE
          | Type_BLUETOOTH_DATAIO
          | Type_BLUETOOTH_DATARAW
          | Type_BLUETOOTH_STATE_INQUIRY
          | Type_WIFI_STATE_FREQUENCY
          | Type_WIFI_STATE_FREQUENCYLOOP
          | Type_WIFI_DATARAW
          | Type_WIFI_DATADEVRAW
          | Type_WIFI_DATAIO
          | Type_INFO
          | Type_STATE_SCANNING
          | Type_STATE_GYRID
          | Type_STATE_ANTENNA
          | Type_UPTIME
          | Type_REQUEST_HOSTNAME
          | Type_REQUEST_KEEPALIVE
          | Type_REQUEST_UPTIME
          | Type_REQUEST_CACHING
          | Type_REQUEST_STARTDATA
          | Type_REQUEST_STATE
          | Type_HOSTNAME
          | Type_ANTENNA_TURN
          | Type_SCAN_PATTERN
          deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable Type

instance Prelude'.Bounded Type where
  minBound = Type_ACK
  maxBound = Type_SCAN_PATTERN

instance P'.Default Type where
  defaultValue = Type_ACK

toMaybe'Enum :: Prelude'.Int -> P'.Maybe Type
toMaybe'Enum 1 = Prelude'.Just Type_ACK
toMaybe'Enum 2 = Prelude'.Just Type_KEEPALIVE
toMaybe'Enum 3 = Prelude'.Just Type_BLUETOOTH_DATAIO
toMaybe'Enum 4 = Prelude'.Just Type_BLUETOOTH_DATARAW
toMaybe'Enum 5 = Prelude'.Just Type_BLUETOOTH_STATE_INQUIRY
toMaybe'Enum 6 = Prelude'.Just Type_WIFI_STATE_FREQUENCY
toMaybe'Enum 7 = Prelude'.Just Type_WIFI_STATE_FREQUENCYLOOP
toMaybe'Enum 8 = Prelude'.Just Type_WIFI_DATARAW
toMaybe'Enum 9 = Prelude'.Just Type_WIFI_DATADEVRAW
toMaybe'Enum 10 = Prelude'.Just Type_WIFI_DATAIO
toMaybe'Enum 11 = Prelude'.Just Type_INFO
toMaybe'Enum 12 = Prelude'.Just Type_STATE_SCANNING
toMaybe'Enum 13 = Prelude'.Just Type_STATE_GYRID
toMaybe'Enum 14 = Prelude'.Just Type_STATE_ANTENNA
toMaybe'Enum 15 = Prelude'.Just Type_UPTIME
toMaybe'Enum 16 = Prelude'.Just Type_REQUEST_HOSTNAME
toMaybe'Enum 17 = Prelude'.Just Type_REQUEST_KEEPALIVE
toMaybe'Enum 18 = Prelude'.Just Type_REQUEST_UPTIME
toMaybe'Enum 19 = Prelude'.Just Type_REQUEST_CACHING
toMaybe'Enum 20 = Prelude'.Just Type_REQUEST_STARTDATA
toMaybe'Enum 21 = Prelude'.Just Type_REQUEST_STATE
toMaybe'Enum 22 = Prelude'.Just Type_HOSTNAME
toMaybe'Enum 23 = Prelude'.Just Type_ANTENNA_TURN
toMaybe'Enum 24 = Prelude'.Just Type_SCAN_PATTERN
toMaybe'Enum _ = Prelude'.Nothing

instance Prelude'.Enum Type where
  fromEnum Type_ACK = 1
  fromEnum Type_KEEPALIVE = 2
  fromEnum Type_BLUETOOTH_DATAIO = 3
  fromEnum Type_BLUETOOTH_DATARAW = 4
  fromEnum Type_BLUETOOTH_STATE_INQUIRY = 5
  fromEnum Type_WIFI_STATE_FREQUENCY = 6
  fromEnum Type_WIFI_STATE_FREQUENCYLOOP = 7
  fromEnum Type_WIFI_DATARAW = 8
  fromEnum Type_WIFI_DATADEVRAW = 9
  fromEnum Type_WIFI_DATAIO = 10
  fromEnum Type_INFO = 11
  fromEnum Type_STATE_SCANNING = 12
  fromEnum Type_STATE_GYRID = 13
  fromEnum Type_STATE_ANTENNA = 14
  fromEnum Type_UPTIME = 15
  fromEnum Type_REQUEST_HOSTNAME = 16
  fromEnum Type_REQUEST_KEEPALIVE = 17
  fromEnum Type_REQUEST_UPTIME = 18
  fromEnum Type_REQUEST_CACHING = 19
  fromEnum Type_REQUEST_STARTDATA = 20
  fromEnum Type_REQUEST_STATE = 21
  fromEnum Type_HOSTNAME = 22
  fromEnum Type_ANTENNA_TURN = 23
  fromEnum Type_SCAN_PATTERN = 24
  toEnum = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type Gyrid.Msg.Type") . toMaybe'Enum
  succ Type_ACK = Type_KEEPALIVE
  succ Type_KEEPALIVE = Type_BLUETOOTH_DATAIO
  succ Type_BLUETOOTH_DATAIO = Type_BLUETOOTH_DATARAW
  succ Type_BLUETOOTH_DATARAW = Type_BLUETOOTH_STATE_INQUIRY
  succ Type_BLUETOOTH_STATE_INQUIRY = Type_WIFI_STATE_FREQUENCY
  succ Type_WIFI_STATE_FREQUENCY = Type_WIFI_STATE_FREQUENCYLOOP
  succ Type_WIFI_STATE_FREQUENCYLOOP = Type_WIFI_DATARAW
  succ Type_WIFI_DATARAW = Type_WIFI_DATADEVRAW
  succ Type_WIFI_DATADEVRAW = Type_WIFI_DATAIO
  succ Type_WIFI_DATAIO = Type_INFO
  succ Type_INFO = Type_STATE_SCANNING
  succ Type_STATE_SCANNING = Type_STATE_GYRID
  succ Type_STATE_GYRID = Type_STATE_ANTENNA
  succ Type_STATE_ANTENNA = Type_UPTIME
  succ Type_UPTIME = Type_REQUEST_HOSTNAME
  succ Type_REQUEST_HOSTNAME = Type_REQUEST_KEEPALIVE
  succ Type_REQUEST_KEEPALIVE = Type_REQUEST_UPTIME
  succ Type_REQUEST_UPTIME = Type_REQUEST_CACHING
  succ Type_REQUEST_CACHING = Type_REQUEST_STARTDATA
  succ Type_REQUEST_STARTDATA = Type_REQUEST_STATE
  succ Type_REQUEST_STATE = Type_HOSTNAME
  succ Type_HOSTNAME = Type_ANTENNA_TURN
  succ Type_ANTENNA_TURN = Type_SCAN_PATTERN
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Gyrid.Msg.Type"
  pred Type_KEEPALIVE = Type_ACK
  pred Type_BLUETOOTH_DATAIO = Type_KEEPALIVE
  pred Type_BLUETOOTH_DATARAW = Type_BLUETOOTH_DATAIO
  pred Type_BLUETOOTH_STATE_INQUIRY = Type_BLUETOOTH_DATARAW
  pred Type_WIFI_STATE_FREQUENCY = Type_BLUETOOTH_STATE_INQUIRY
  pred Type_WIFI_STATE_FREQUENCYLOOP = Type_WIFI_STATE_FREQUENCY
  pred Type_WIFI_DATARAW = Type_WIFI_STATE_FREQUENCYLOOP
  pred Type_WIFI_DATADEVRAW = Type_WIFI_DATARAW
  pred Type_WIFI_DATAIO = Type_WIFI_DATADEVRAW
  pred Type_INFO = Type_WIFI_DATAIO
  pred Type_STATE_SCANNING = Type_INFO
  pred Type_STATE_GYRID = Type_STATE_SCANNING
  pred Type_STATE_ANTENNA = Type_STATE_GYRID
  pred Type_UPTIME = Type_STATE_ANTENNA
  pred Type_REQUEST_HOSTNAME = Type_UPTIME
  pred Type_REQUEST_KEEPALIVE = Type_REQUEST_HOSTNAME
  pred Type_REQUEST_UPTIME = Type_REQUEST_KEEPALIVE
  pred Type_REQUEST_CACHING = Type_REQUEST_UPTIME
  pred Type_REQUEST_STARTDATA = Type_REQUEST_CACHING
  pred Type_REQUEST_STATE = Type_REQUEST_STARTDATA
  pred Type_HOSTNAME = Type_REQUEST_STATE
  pred Type_ANTENNA_TURN = Type_HOSTNAME
  pred Type_SCAN_PATTERN = Type_ANTENNA_TURN
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Gyrid.Msg.Type"

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
  reflectEnum
   = [(1, "Type_ACK", Type_ACK), (2, "Type_KEEPALIVE", Type_KEEPALIVE), (3, "Type_BLUETOOTH_DATAIO", Type_BLUETOOTH_DATAIO),
      (4, "Type_BLUETOOTH_DATARAW", Type_BLUETOOTH_DATARAW), (5, "Type_BLUETOOTH_STATE_INQUIRY", Type_BLUETOOTH_STATE_INQUIRY),
      (6, "Type_WIFI_STATE_FREQUENCY", Type_WIFI_STATE_FREQUENCY),
      (7, "Type_WIFI_STATE_FREQUENCYLOOP", Type_WIFI_STATE_FREQUENCYLOOP), (8, "Type_WIFI_DATARAW", Type_WIFI_DATARAW),
      (9, "Type_WIFI_DATADEVRAW", Type_WIFI_DATADEVRAW), (10, "Type_WIFI_DATAIO", Type_WIFI_DATAIO), (11, "Type_INFO", Type_INFO),
      (12, "Type_STATE_SCANNING", Type_STATE_SCANNING), (13, "Type_STATE_GYRID", Type_STATE_GYRID),
      (14, "Type_STATE_ANTENNA", Type_STATE_ANTENNA), (15, "Type_UPTIME", Type_UPTIME),
      (16, "Type_REQUEST_HOSTNAME", Type_REQUEST_HOSTNAME), (17, "Type_REQUEST_KEEPALIVE", Type_REQUEST_KEEPALIVE),
      (18, "Type_REQUEST_UPTIME", Type_REQUEST_UPTIME), (19, "Type_REQUEST_CACHING", Type_REQUEST_CACHING),
      (20, "Type_REQUEST_STARTDATA", Type_REQUEST_STARTDATA), (21, "Type_REQUEST_STATE", Type_REQUEST_STATE),
      (22, "Type_HOSTNAME", Type_HOSTNAME), (23, "Type_ANTENNA_TURN", Type_ANTENNA_TURN),
      (24, "Type_SCAN_PATTERN", Type_SCAN_PATTERN)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".Gyrid.Msg.Type") [] ["Gyrid", "Msg"] "Type") ["Gyrid", "Msg", "Type.hs"]
      [(1, "Type_ACK"), (2, "Type_KEEPALIVE"), (3, "Type_BLUETOOTH_DATAIO"), (4, "Type_BLUETOOTH_DATARAW"),
       (5, "Type_BLUETOOTH_STATE_INQUIRY"), (6, "Type_WIFI_STATE_FREQUENCY"), (7, "Type_WIFI_STATE_FREQUENCYLOOP"),
       (8, "Type_WIFI_DATARAW"), (9, "Type_WIFI_DATADEVRAW"), (10, "Type_WIFI_DATAIO"), (11, "Type_INFO"),
       (12, "Type_STATE_SCANNING"), (13, "Type_STATE_GYRID"), (14, "Type_STATE_ANTENNA"), (15, "Type_UPTIME"),
       (16, "Type_REQUEST_HOSTNAME"), (17, "Type_REQUEST_KEEPALIVE"), (18, "Type_REQUEST_UPTIME"), (19, "Type_REQUEST_CACHING"),
       (20, "Type_REQUEST_STARTDATA"), (21, "Type_REQUEST_STATE"), (22, "Type_HOSTNAME"), (23, "Type_ANTENNA_TURN"),
       (24, "Type_SCAN_PATTERN")]

instance P'.TextType Type where
  tellT = P'.tellShow
  getT = P'.getRead