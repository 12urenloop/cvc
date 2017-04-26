{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Gyrid.ScanPattern (ScanPattern(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Gyrid.ScanPattern.Action as Gyrid.ScanPattern (Action)

data ScanPattern = ScanPattern{action :: !(P'.Maybe Gyrid.ScanPattern.Action), sensorMac :: !(P'.Maybe P'.ByteString),
                               startTime :: !(P'.Maybe P'.Word32), stopTime :: !(P'.Maybe P'.Word32),
                               startAngle :: !(P'.Maybe P'.Word32), stopAngle :: !(P'.Maybe P'.Word32),
                               scanAngle :: !(P'.Maybe P'.Word32), inquiryLength :: !(P'.Maybe P'.Word32),
                               bufferTime :: !(P'.Maybe P'.Word32), turnResolution :: !(P'.Maybe P'.Word32)}
                 deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable ScanPattern where
  mergeAppend (ScanPattern x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10) (ScanPattern y'1 y'2 y'3 y'4 y'5 y'6 y'7 y'8 y'9 y'10)
   = ScanPattern (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
      (P'.mergeAppend x'7 y'7)
      (P'.mergeAppend x'8 y'8)
      (P'.mergeAppend x'9 y'9)
      (P'.mergeAppend x'10 y'10)

instance P'.Default ScanPattern where
  defaultValue
   = ScanPattern P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue

instance P'.Wire ScanPattern where
  wireSize ft' self'@(ScanPattern x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeOpt 1 14 x'1 + P'.wireSizeOpt 1 12 x'2 + P'.wireSizeOpt 1 13 x'3 + P'.wireSizeOpt 1 13 x'4 +
             P'.wireSizeOpt 1 13 x'5
             + P'.wireSizeOpt 1 13 x'6
             + P'.wireSizeOpt 1 13 x'7
             + P'.wireSizeOpt 1 13 x'8
             + P'.wireSizeOpt 1 13 x'9
             + P'.wireSizeOpt 1 13 x'10)
  wirePut ft' self'@(ScanPattern x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutOpt 8 14 x'1
             P'.wirePutOpt 18 12 x'2
             P'.wirePutOpt 24 13 x'3
             P'.wirePutOpt 32 13 x'4
             P'.wirePutOpt 40 13 x'5
             P'.wirePutOpt 48 13 x'6
             P'.wirePutOpt 56 13 x'7
             P'.wirePutOpt 64 13 x'8
             P'.wirePutOpt 72 13 x'9
             P'.wirePutOpt 80 13 x'10
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{action = Prelude'.Just new'Field}) (P'.wireGet 14)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{sensorMac = Prelude'.Just new'Field}) (P'.wireGet 12)
             24 -> Prelude'.fmap (\ !new'Field -> old'Self{startTime = Prelude'.Just new'Field}) (P'.wireGet 13)
             32 -> Prelude'.fmap (\ !new'Field -> old'Self{stopTime = Prelude'.Just new'Field}) (P'.wireGet 13)
             40 -> Prelude'.fmap (\ !new'Field -> old'Self{startAngle = Prelude'.Just new'Field}) (P'.wireGet 13)
             48 -> Prelude'.fmap (\ !new'Field -> old'Self{stopAngle = Prelude'.Just new'Field}) (P'.wireGet 13)
             56 -> Prelude'.fmap (\ !new'Field -> old'Self{scanAngle = Prelude'.Just new'Field}) (P'.wireGet 13)
             64 -> Prelude'.fmap (\ !new'Field -> old'Self{inquiryLength = Prelude'.Just new'Field}) (P'.wireGet 13)
             72 -> Prelude'.fmap (\ !new'Field -> old'Self{bufferTime = Prelude'.Just new'Field}) (P'.wireGet 13)
             80 -> Prelude'.fmap (\ !new'Field -> old'Self{turnResolution = Prelude'.Just new'Field}) (P'.wireGet 13)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> ScanPattern) ScanPattern where
  getVal m' f' = f' m'

instance P'.GPB ScanPattern

instance P'.ReflectDescriptor ScanPattern where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [8, 18, 24, 32, 40, 48, 56, 64, 72, 80])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Gyrid.ScanPattern\", haskellPrefix = [], parentModule = [MName \"Gyrid\"], baseName = MName \"ScanPattern\"}, descFilePath = [\"Gyrid\",\"ScanPattern.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.ScanPattern.action\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"ScanPattern\"], baseName' = FName \"action\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".Gyrid.ScanPattern.Action\", haskellPrefix = [], parentModule = [MName \"Gyrid\",MName \"ScanPattern\"], baseName = MName \"Action\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.ScanPattern.sensorMac\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"ScanPattern\"], baseName' = FName \"sensorMac\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.ScanPattern.startTime\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"ScanPattern\"], baseName' = FName \"startTime\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.ScanPattern.stopTime\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"ScanPattern\"], baseName' = FName \"stopTime\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 32}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.ScanPattern.startAngle\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"ScanPattern\"], baseName' = FName \"startAngle\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 40}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.ScanPattern.stopAngle\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"ScanPattern\"], baseName' = FName \"stopAngle\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 48}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.ScanPattern.scanAngle\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"ScanPattern\"], baseName' = FName \"scanAngle\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 7}, wireTag = WireTag {getWireTag = 56}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.ScanPattern.inquiryLength\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"ScanPattern\"], baseName' = FName \"inquiryLength\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 8}, wireTag = WireTag {getWireTag = 64}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.ScanPattern.bufferTime\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"ScanPattern\"], baseName' = FName \"bufferTime\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 9}, wireTag = WireTag {getWireTag = 72}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.ScanPattern.turnResolution\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"ScanPattern\"], baseName' = FName \"turnResolution\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 10}, wireTag = WireTag {getWireTag = 80}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType ScanPattern where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg ScanPattern where
  textPut msg
   = do
       P'.tellT "action" (action msg)
       P'.tellT "sensorMac" (sensorMac msg)
       P'.tellT "startTime" (startTime msg)
       P'.tellT "stopTime" (stopTime msg)
       P'.tellT "startAngle" (startAngle msg)
       P'.tellT "stopAngle" (stopAngle msg)
       P'.tellT "scanAngle" (scanAngle msg)
       P'.tellT "inquiryLength" (inquiryLength msg)
       P'.tellT "bufferTime" (bufferTime msg)
       P'.tellT "turnResolution" (turnResolution msg)
  textGet
   = do
       mods <- P'.sepEndBy
                (P'.choice
                  [parse'action, parse'sensorMac, parse'startTime, parse'stopTime, parse'startAngle, parse'stopAngle,
                   parse'scanAngle, parse'inquiryLength, parse'bufferTime, parse'turnResolution])
                P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'action
         = P'.try
            (do
               v <- P'.getT "action"
               Prelude'.return (\ o -> o{action = v}))
        parse'sensorMac
         = P'.try
            (do
               v <- P'.getT "sensorMac"
               Prelude'.return (\ o -> o{sensorMac = v}))
        parse'startTime
         = P'.try
            (do
               v <- P'.getT "startTime"
               Prelude'.return (\ o -> o{startTime = v}))
        parse'stopTime
         = P'.try
            (do
               v <- P'.getT "stopTime"
               Prelude'.return (\ o -> o{stopTime = v}))
        parse'startAngle
         = P'.try
            (do
               v <- P'.getT "startAngle"
               Prelude'.return (\ o -> o{startAngle = v}))
        parse'stopAngle
         = P'.try
            (do
               v <- P'.getT "stopAngle"
               Prelude'.return (\ o -> o{stopAngle = v}))
        parse'scanAngle
         = P'.try
            (do
               v <- P'.getT "scanAngle"
               Prelude'.return (\ o -> o{scanAngle = v}))
        parse'inquiryLength
         = P'.try
            (do
               v <- P'.getT "inquiryLength"
               Prelude'.return (\ o -> o{inquiryLength = v}))
        parse'bufferTime
         = P'.try
            (do
               v <- P'.getT "bufferTime"
               Prelude'.return (\ o -> o{bufferTime = v}))
        parse'turnResolution
         = P'.try
            (do
               v <- P'.getT "turnResolution"
               Prelude'.return (\ o -> o{turnResolution = v}))