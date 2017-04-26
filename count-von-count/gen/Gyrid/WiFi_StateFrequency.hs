{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Gyrid.WiFi_StateFrequency (WiFi_StateFrequency(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data WiFi_StateFrequency = WiFi_StateFrequency{timestamp :: !(P'.Maybe P'.Double), sensorMac :: !(P'.Maybe P'.ByteString),
                                               frequency :: !(P'.Maybe P'.Word32), duration :: !(P'.Maybe P'.Word32)}
                         deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable WiFi_StateFrequency where
  mergeAppend (WiFi_StateFrequency x'1 x'2 x'3 x'4) (WiFi_StateFrequency y'1 y'2 y'3 y'4)
   = WiFi_StateFrequency (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)

instance P'.Default WiFi_StateFrequency where
  defaultValue = WiFi_StateFrequency P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue

instance P'.Wire WiFi_StateFrequency where
  wireSize ft' self'@(WiFi_StateFrequency x'1 x'2 x'3 x'4)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 1 x'1 + P'.wireSizeOpt 1 12 x'2 + P'.wireSizeOpt 1 13 x'3 + P'.wireSizeOpt 1 13 x'4)
  wirePut ft' self'@(WiFi_StateFrequency x'1 x'2 x'3 x'4)
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
             P'.wirePutOpt 24 13 x'3
             P'.wirePutOpt 32 13 x'4
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
             24 -> Prelude'.fmap (\ !new'Field -> old'Self{frequency = Prelude'.Just new'Field}) (P'.wireGet 13)
             32 -> Prelude'.fmap (\ !new'Field -> old'Self{duration = Prelude'.Just new'Field}) (P'.wireGet 13)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> WiFi_StateFrequency) WiFi_StateFrequency where
  getVal m' f' = f' m'

instance P'.GPB WiFi_StateFrequency

instance P'.ReflectDescriptor WiFi_StateFrequency where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [9, 18, 24, 32])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Gyrid.WiFi_StateFrequency\", haskellPrefix = [], parentModule = [MName \"Gyrid\"], baseName = MName \"WiFi_StateFrequency\"}, descFilePath = [\"Gyrid\",\"WiFi_StateFrequency.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.WiFi_StateFrequency.timestamp\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"WiFi_StateFrequency\"], baseName' = FName \"timestamp\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 9}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.WiFi_StateFrequency.sensorMac\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"WiFi_StateFrequency\"], baseName' = FName \"sensorMac\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.WiFi_StateFrequency.frequency\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"WiFi_StateFrequency\"], baseName' = FName \"frequency\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.WiFi_StateFrequency.duration\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"WiFi_StateFrequency\"], baseName' = FName \"duration\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 32}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType WiFi_StateFrequency where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg WiFi_StateFrequency where
  textPut msg
   = do
       P'.tellT "timestamp" (timestamp msg)
       P'.tellT "sensorMac" (sensorMac msg)
       P'.tellT "frequency" (frequency msg)
       P'.tellT "duration" (duration msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'timestamp, parse'sensorMac, parse'frequency, parse'duration]) P'.spaces
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
        parse'frequency
         = P'.try
            (do
               v <- P'.getT "frequency"
               Prelude'.return (\ o -> o{frequency = v}))
        parse'duration
         = P'.try
            (do
               v <- P'.getT "duration"
               Prelude'.return (\ o -> o{duration = v}))