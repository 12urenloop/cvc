{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Gyrid.Bluetooth_DataRaw (Bluetooth_DataRaw(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data Bluetooth_DataRaw = Bluetooth_DataRaw{timestamp :: !(P'.Maybe P'.Double), sensorMac :: !(P'.Maybe P'.ByteString),
                                           hwid :: !(P'.Maybe P'.ByteString), deviceclass :: !(P'.Maybe P'.Word32),
                                           rssi :: !(P'.Maybe P'.Int32), angle :: !(P'.Maybe P'.Word32)}
                       deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable Bluetooth_DataRaw where
  mergeAppend (Bluetooth_DataRaw x'1 x'2 x'3 x'4 x'5 x'6) (Bluetooth_DataRaw y'1 y'2 y'3 y'4 y'5 y'6)
   = Bluetooth_DataRaw (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
 
instance P'.Default Bluetooth_DataRaw where
  defaultValue = Bluetooth_DataRaw P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire Bluetooth_DataRaw where
  wireSize ft' self'@(Bluetooth_DataRaw x'1 x'2 x'3 x'4 x'5 x'6)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeOpt 1 1 x'1 + P'.wireSizeOpt 1 12 x'2 + P'.wireSizeOpt 1 12 x'3 + P'.wireSizeOpt 1 13 x'4 +
             P'.wireSizeOpt 1 17 x'5
             + P'.wireSizeOpt 1 13 x'6)
  wirePut ft' self'@(Bluetooth_DataRaw x'1 x'2 x'3 x'4 x'5 x'6)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutOpt 9 1 x'1
             P'.wirePutOpt 18 12 x'2
             P'.wirePutOpt 26 12 x'3
             P'.wirePutOpt 32 13 x'4
             P'.wirePutOpt 40 17 x'5
             P'.wirePutOpt 48 13 x'6
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             9 -> Prelude'.fmap (\ !new'Field -> old'Self{timestamp = Prelude'.Just new'Field}) (P'.wireGet 1)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{sensorMac = Prelude'.Just new'Field}) (P'.wireGet 12)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{hwid = Prelude'.Just new'Field}) (P'.wireGet 12)
             32 -> Prelude'.fmap (\ !new'Field -> old'Self{deviceclass = Prelude'.Just new'Field}) (P'.wireGet 13)
             40 -> Prelude'.fmap (\ !new'Field -> old'Self{rssi = Prelude'.Just new'Field}) (P'.wireGet 17)
             48 -> Prelude'.fmap (\ !new'Field -> old'Self{angle = Prelude'.Just new'Field}) (P'.wireGet 13)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> Bluetooth_DataRaw) Bluetooth_DataRaw where
  getVal m' f' = f' m'
 
instance P'.GPB Bluetooth_DataRaw
 
instance P'.ReflectDescriptor Bluetooth_DataRaw where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [9, 18, 26, 32, 40, 48])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Gyrid.Bluetooth_DataRaw\", haskellPrefix = [], parentModule = [MName \"Gyrid\"], baseName = MName \"Bluetooth_DataRaw\"}, descFilePath = [\"Gyrid\",\"Bluetooth_DataRaw.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.Bluetooth_DataRaw.timestamp\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"Bluetooth_DataRaw\"], baseName' = FName \"timestamp\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 9}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.Bluetooth_DataRaw.sensorMac\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"Bluetooth_DataRaw\"], baseName' = FName \"sensorMac\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.Bluetooth_DataRaw.hwid\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"Bluetooth_DataRaw\"], baseName' = FName \"hwid\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.Bluetooth_DataRaw.deviceclass\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"Bluetooth_DataRaw\"], baseName' = FName \"deviceclass\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 32}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.Bluetooth_DataRaw.rssi\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"Bluetooth_DataRaw\"], baseName' = FName \"rssi\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 40}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 17}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.Bluetooth_DataRaw.angle\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"Bluetooth_DataRaw\"], baseName' = FName \"angle\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 48}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"