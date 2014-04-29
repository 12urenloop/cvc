{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Gyrid.Bluetooth_DataIO (Bluetooth_DataIO(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Gyrid.Bluetooth_DataIO.Move as Gyrid.Bluetooth_DataIO (Move)
 
data Bluetooth_DataIO = Bluetooth_DataIO{timestamp :: !(P'.Maybe P'.Double), sensorMac :: !(P'.Maybe P'.ByteString),
                                         hwid :: !(P'.Maybe P'.ByteString), deviceclass :: !(P'.Maybe P'.Word32),
                                         move :: !(P'.Maybe Gyrid.Bluetooth_DataIO.Move)}
                      deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable Bluetooth_DataIO where
  mergeAppend (Bluetooth_DataIO x'1 x'2 x'3 x'4 x'5) (Bluetooth_DataIO y'1 y'2 y'3 y'4 y'5)
   = Bluetooth_DataIO (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
 
instance P'.Default Bluetooth_DataIO where
  defaultValue = Bluetooth_DataIO P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire Bluetooth_DataIO where
  wireSize ft' self'@(Bluetooth_DataIO x'1 x'2 x'3 x'4 x'5)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeOpt 1 1 x'1 + P'.wireSizeOpt 1 12 x'2 + P'.wireSizeOpt 1 12 x'3 + P'.wireSizeOpt 1 13 x'4 +
             P'.wireSizeOpt 1 14 x'5)
  wirePut ft' self'@(Bluetooth_DataIO x'1 x'2 x'3 x'4 x'5)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutOpt 17 1 x'1
             P'.wirePutOpt 26 12 x'2
             P'.wirePutOpt 34 12 x'3
             P'.wirePutOpt 40 13 x'4
             P'.wirePutOpt 48 14 x'5
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             17 -> Prelude'.fmap (\ !new'Field -> old'Self{timestamp = Prelude'.Just new'Field}) (P'.wireGet 1)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{sensorMac = Prelude'.Just new'Field}) (P'.wireGet 12)
             34 -> Prelude'.fmap (\ !new'Field -> old'Self{hwid = Prelude'.Just new'Field}) (P'.wireGet 12)
             40 -> Prelude'.fmap (\ !new'Field -> old'Self{deviceclass = Prelude'.Just new'Field}) (P'.wireGet 13)
             48 -> Prelude'.fmap (\ !new'Field -> old'Self{move = Prelude'.Just new'Field}) (P'.wireGet 14)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> Bluetooth_DataIO) Bluetooth_DataIO where
  getVal m' f' = f' m'
 
instance P'.GPB Bluetooth_DataIO
 
instance P'.ReflectDescriptor Bluetooth_DataIO where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [17, 26, 34, 40, 48])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Gyrid.Bluetooth_DataIO\", haskellPrefix = [], parentModule = [MName \"Gyrid\"], baseName = MName \"Bluetooth_DataIO\"}, descFilePath = [\"Gyrid\",\"Bluetooth_DataIO.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.Bluetooth_DataIO.timestamp\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"Bluetooth_DataIO\"], baseName' = FName \"timestamp\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 17}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.Bluetooth_DataIO.sensorMac\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"Bluetooth_DataIO\"], baseName' = FName \"sensorMac\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.Bluetooth_DataIO.hwid\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"Bluetooth_DataIO\"], baseName' = FName \"hwid\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.Bluetooth_DataIO.deviceclass\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"Bluetooth_DataIO\"], baseName' = FName \"deviceclass\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 40}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.Bluetooth_DataIO.move\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"Bluetooth_DataIO\"], baseName' = FName \"move\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 48}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".Gyrid.Bluetooth_DataIO.Move\", haskellPrefix = [], parentModule = [MName \"Gyrid\",MName \"Bluetooth_DataIO\"], baseName = MName \"Move\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"