{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Gyrid.WiFi_DataDevRaw (WiFi_DataDevRaw(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data WiFi_DataDevRaw = WiFi_DataDevRaw{timestamp :: !(P'.Maybe P'.Double), sensorMac :: !(P'.Maybe P'.ByteString),
                                       hwid :: !(P'.Maybe P'.ByteString), frequency :: !(P'.Maybe P'.Word32),
                                       ssi :: !(P'.Maybe P'.Int32)}
                     deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable WiFi_DataDevRaw where
  mergeAppend (WiFi_DataDevRaw x'1 x'2 x'3 x'4 x'5) (WiFi_DataDevRaw y'1 y'2 y'3 y'4 y'5)
   = WiFi_DataDevRaw (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)

instance P'.Default WiFi_DataDevRaw where
  defaultValue = WiFi_DataDevRaw P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue

instance P'.Wire WiFi_DataDevRaw where
  wireSize ft' self'@(WiFi_DataDevRaw x'1 x'2 x'3 x'4 x'5)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeOpt 1 1 x'1 + P'.wireSizeOpt 1 12 x'2 + P'.wireSizeOpt 1 12 x'3 + P'.wireSizeOpt 1 13 x'4 +
             P'.wireSizeOpt 1 17 x'5)
  wirePut ft' self'@(WiFi_DataDevRaw x'1 x'2 x'3 x'4 x'5)
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
             32 -> Prelude'.fmap (\ !new'Field -> old'Self{frequency = Prelude'.Just new'Field}) (P'.wireGet 13)
             40 -> Prelude'.fmap (\ !new'Field -> old'Self{ssi = Prelude'.Just new'Field}) (P'.wireGet 17)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> WiFi_DataDevRaw) WiFi_DataDevRaw where
  getVal m' f' = f' m'

instance P'.GPB WiFi_DataDevRaw

instance P'.ReflectDescriptor WiFi_DataDevRaw where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [9, 18, 26, 32, 40])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Gyrid.WiFi_DataDevRaw\", haskellPrefix = [], parentModule = [MName \"Gyrid\"], baseName = MName \"WiFi_DataDevRaw\"}, descFilePath = [\"Gyrid\",\"WiFi_DataDevRaw.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.WiFi_DataDevRaw.timestamp\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"WiFi_DataDevRaw\"], baseName' = FName \"timestamp\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 9}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.WiFi_DataDevRaw.sensorMac\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"WiFi_DataDevRaw\"], baseName' = FName \"sensorMac\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.WiFi_DataDevRaw.hwid\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"WiFi_DataDevRaw\"], baseName' = FName \"hwid\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.WiFi_DataDevRaw.frequency\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"WiFi_DataDevRaw\"], baseName' = FName \"frequency\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 32}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.WiFi_DataDevRaw.ssi\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"WiFi_DataDevRaw\"], baseName' = FName \"ssi\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 40}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 17}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType WiFi_DataDevRaw where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg WiFi_DataDevRaw where
  textPut msg
   = do
       P'.tellT "timestamp" (timestamp msg)
       P'.tellT "sensorMac" (sensorMac msg)
       P'.tellT "hwid" (hwid msg)
       P'.tellT "frequency" (frequency msg)
       P'.tellT "ssi" (ssi msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'timestamp, parse'sensorMac, parse'hwid, parse'frequency, parse'ssi]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'timestamp
         = P'.try
            (do
               v <- P'.getT "timestamp"
               Prelude'.return (\ o -> o{timestamp = v}))
        parse'sensorMac
         = P'.try
            (do
               v <- P'.getT "sensorMac"
               Prelude'.return (\ o -> o{sensorMac = v}))
        parse'hwid
         = P'.try
            (do
               v <- P'.getT "hwid"
               Prelude'.return (\ o -> o{hwid = v}))
        parse'frequency
         = P'.try
            (do
               v <- P'.getT "frequency"
               Prelude'.return (\ o -> o{frequency = v}))
        parse'ssi
         = P'.try
            (do
               v <- P'.getT "ssi"
               Prelude'.return (\ o -> o{ssi = v}))