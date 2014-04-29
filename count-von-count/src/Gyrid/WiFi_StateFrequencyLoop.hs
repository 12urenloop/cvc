{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Gyrid.WiFi_StateFrequencyLoop (WiFi_StateFrequencyLoop(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data WiFi_StateFrequencyLoop = WiFi_StateFrequencyLoop{timestamp :: !(P'.Maybe P'.Double), sensorMac :: !(P'.Maybe P'.ByteString),
                                                       duration :: !(P'.Maybe P'.Word32), frequency :: !(P'.Seq P'.Word32)}
                             deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable WiFi_StateFrequencyLoop where
  mergeAppend (WiFi_StateFrequencyLoop x'1 x'2 x'3 x'4) (WiFi_StateFrequencyLoop y'1 y'2 y'3 y'4)
   = WiFi_StateFrequencyLoop (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
 
instance P'.Default WiFi_StateFrequencyLoop where
  defaultValue = WiFi_StateFrequencyLoop P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire WiFi_StateFrequencyLoop where
  wireSize ft' self'@(WiFi_StateFrequencyLoop x'1 x'2 x'3 x'4)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 1 x'1 + P'.wireSizeOpt 1 12 x'2 + P'.wireSizeOpt 1 13 x'3 + P'.wireSizeRep 1 13 x'4)
  wirePut ft' self'@(WiFi_StateFrequencyLoop x'1 x'2 x'3 x'4)
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
             P'.wirePutRep 32 13 x'4
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
             24 -> Prelude'.fmap (\ !new'Field -> old'Self{duration = Prelude'.Just new'Field}) (P'.wireGet 13)
             32 -> Prelude'.fmap (\ !new'Field -> old'Self{frequency = P'.append (frequency old'Self) new'Field}) (P'.wireGet 13)
             34 -> Prelude'.fmap (\ !new'Field -> old'Self{frequency = P'.mergeAppend (frequency old'Self) new'Field})
                    (P'.wireGetPacked 13)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> WiFi_StateFrequencyLoop) WiFi_StateFrequencyLoop where
  getVal m' f' = f' m'
 
instance P'.GPB WiFi_StateFrequencyLoop
 
instance P'.ReflectDescriptor WiFi_StateFrequencyLoop where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [9, 18, 24, 32, 34])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Gyrid.WiFi_StateFrequencyLoop\", haskellPrefix = [], parentModule = [MName \"Gyrid\"], baseName = MName \"WiFi_StateFrequencyLoop\"}, descFilePath = [\"Gyrid\",\"WiFi_StateFrequencyLoop.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.WiFi_StateFrequencyLoop.timestamp\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"WiFi_StateFrequencyLoop\"], baseName' = FName \"timestamp\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 9}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.WiFi_StateFrequencyLoop.sensorMac\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"WiFi_StateFrequencyLoop\"], baseName' = FName \"sensorMac\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.WiFi_StateFrequencyLoop.duration\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"WiFi_StateFrequencyLoop\"], baseName' = FName \"duration\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.WiFi_StateFrequencyLoop.frequency\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"WiFi_StateFrequencyLoop\"], baseName' = FName \"frequency\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 32}, packedTag = Just (WireTag {getWireTag = 32},WireTag {getWireTag = 34}), wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"