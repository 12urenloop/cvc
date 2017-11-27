{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Gyrid.AntennaTurn (AntennaTurn(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data AntennaTurn = AntennaTurn{sensorMac :: !(P'.Maybe P'.ByteString), timestamp :: !(P'.Maybe P'.Float),
                               angle :: !(P'.Maybe P'.Float), fixedTimeBuffer :: !(P'.Maybe P'.Word32)}
                 deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable AntennaTurn where
  mergeAppend (AntennaTurn x'1 x'2 x'3 x'4) (AntennaTurn y'1 y'2 y'3 y'4)
   = AntennaTurn (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)

instance P'.Default AntennaTurn where
  defaultValue = AntennaTurn P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue

instance P'.Wire AntennaTurn where
  wireSize ft' self'@(AntennaTurn x'1 x'2 x'3 x'4)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 12 x'1 + P'.wireSizeOpt 1 2 x'2 + P'.wireSizeOpt 1 2 x'3 + P'.wireSizeOpt 1 13 x'4)
  wirePut ft' self'@(AntennaTurn x'1 x'2 x'3 x'4)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutOpt 10 12 x'1
             P'.wirePutOpt 21 2 x'2
             P'.wirePutOpt 29 2 x'3
             P'.wirePutOpt 32 13 x'4
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{sensorMac = Prelude'.Just new'Field}) (P'.wireGet 12)
             21 -> Prelude'.fmap (\ !new'Field -> old'Self{timestamp = Prelude'.Just new'Field}) (P'.wireGet 2)
             29 -> Prelude'.fmap (\ !new'Field -> old'Self{angle = Prelude'.Just new'Field}) (P'.wireGet 2)
             32 -> Prelude'.fmap (\ !new'Field -> old'Self{fixedTimeBuffer = Prelude'.Just new'Field}) (P'.wireGet 13)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> AntennaTurn) AntennaTurn where
  getVal m' f' = f' m'

instance P'.GPB AntennaTurn

instance P'.ReflectDescriptor AntennaTurn where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 21, 29, 32])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Gyrid.AntennaTurn\", haskellPrefix = [], parentModule = [MName \"Gyrid\"], baseName = MName \"AntennaTurn\"}, descFilePath = [\"Gyrid\",\"AntennaTurn.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.AntennaTurn.sensorMac\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"AntennaTurn\"], baseName' = FName \"sensorMac\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.AntennaTurn.timestamp\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"AntennaTurn\"], baseName' = FName \"timestamp\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 21}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 2}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.AntennaTurn.angle\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"AntennaTurn\"], baseName' = FName \"angle\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 29}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 2}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.AntennaTurn.fixedTimeBuffer\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"AntennaTurn\"], baseName' = FName \"fixedTimeBuffer\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 32}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType AntennaTurn where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg AntennaTurn where
  textPut msg
   = do
       P'.tellT "sensorMac" (sensorMac msg)
       P'.tellT "timestamp" (timestamp msg)
       P'.tellT "angle" (angle msg)
       P'.tellT "fixedTimeBuffer" (fixedTimeBuffer msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'sensorMac, parse'timestamp, parse'angle, parse'fixedTimeBuffer]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'sensorMac
         = P'.try
            (do
               v <- P'.getT "sensorMac"
               Prelude'.return (\ o -> o{sensorMac = v}))
        parse'timestamp
         = P'.try
            (do
               v <- P'.getT "timestamp"
               Prelude'.return (\ o -> o{timestamp = v}))
        parse'angle
         = P'.try
            (do
               v <- P'.getT "angle"
               Prelude'.return (\ o -> o{angle = v}))
        parse'fixedTimeBuffer
         = P'.try
            (do
               v <- P'.getT "fixedTimeBuffer"
               Prelude'.return (\ o -> o{fixedTimeBuffer = v}))