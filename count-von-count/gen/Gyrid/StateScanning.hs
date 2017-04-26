{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Gyrid.StateScanning (StateScanning(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Gyrid.StateScanning.HwType as Gyrid.StateScanning (HwType)
import qualified Gyrid.StateScanning.Type as Gyrid.StateScanning (Type)

data StateScanning = StateScanning{type' :: !(P'.Maybe Gyrid.StateScanning.Type), timestamp :: !(P'.Maybe P'.Double),
                                   sensorMac :: !(P'.Maybe P'.ByteString), hwType :: !(P'.Maybe Gyrid.StateScanning.HwType)}
                   deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable StateScanning where
  mergeAppend (StateScanning x'1 x'2 x'3 x'4) (StateScanning y'1 y'2 y'3 y'4)
   = StateScanning (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)

instance P'.Default StateScanning where
  defaultValue = StateScanning P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue

instance P'.Wire StateScanning where
  wireSize ft' self'@(StateScanning x'1 x'2 x'3 x'4)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 14 x'1 + P'.wireSizeOpt 1 1 x'2 + P'.wireSizeOpt 1 12 x'3 + P'.wireSizeOpt 1 14 x'4)
  wirePut ft' self'@(StateScanning x'1 x'2 x'3 x'4)
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
             P'.wirePutOpt 17 1 x'2
             P'.wirePutOpt 26 12 x'3
             P'.wirePutOpt 32 14 x'4
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{type' = Prelude'.Just new'Field}) (P'.wireGet 14)
             17 -> Prelude'.fmap (\ !new'Field -> old'Self{timestamp = Prelude'.Just new'Field}) (P'.wireGet 1)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{sensorMac = Prelude'.Just new'Field}) (P'.wireGet 12)
             32 -> Prelude'.fmap (\ !new'Field -> old'Self{hwType = Prelude'.Just new'Field}) (P'.wireGet 14)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> StateScanning) StateScanning where
  getVal m' f' = f' m'

instance P'.GPB StateScanning

instance P'.ReflectDescriptor StateScanning where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [8, 17, 26, 32])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Gyrid.StateScanning\", haskellPrefix = [], parentModule = [MName \"Gyrid\"], baseName = MName \"StateScanning\"}, descFilePath = [\"Gyrid\",\"StateScanning.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.StateScanning.type\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"StateScanning\"], baseName' = FName \"type'\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".Gyrid.StateScanning.Type\", haskellPrefix = [], parentModule = [MName \"Gyrid\",MName \"StateScanning\"], baseName = MName \"Type\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.StateScanning.timestamp\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"StateScanning\"], baseName' = FName \"timestamp\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 17}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.StateScanning.sensorMac\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"StateScanning\"], baseName' = FName \"sensorMac\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.StateScanning.hwType\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"StateScanning\"], baseName' = FName \"hwType\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 32}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".Gyrid.StateScanning.HwType\", haskellPrefix = [], parentModule = [MName \"Gyrid\",MName \"StateScanning\"], baseName = MName \"HwType\"}), hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType StateScanning where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg StateScanning where
  textPut msg
   = do
       P'.tellT "type" (type' msg)
       P'.tellT "timestamp" (timestamp msg)
       P'.tellT "sensorMac" (sensorMac msg)
       P'.tellT "hwType" (hwType msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'type', parse'timestamp, parse'sensorMac, parse'hwType]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'type'
         = P'.try
            (do
               v <- P'.getT "type"
               Prelude'.return (\ o -> o{type' = v}))
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
        parse'hwType
         = P'.try
            (do
               v <- P'.getT "hwType"
               Prelude'.return (\ o -> o{hwType = v}))