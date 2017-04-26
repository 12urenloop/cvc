{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Gyrid.WiFi_DataRaw (WiFi_DataRaw(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Gyrid.WiFi_DataRaw.Ctrl as Gyrid.WiFi_DataRaw (Ctrl)
import qualified Gyrid.WiFi_DataRaw.Data as Gyrid.WiFi_DataRaw (Data)
import qualified Gyrid.WiFi_DataRaw.FrameType as Gyrid.WiFi_DataRaw (FrameType)
import qualified Gyrid.WiFi_DataRaw.Mgmt as Gyrid.WiFi_DataRaw (Mgmt)

data WiFi_DataRaw = WiFi_DataRaw{timestamp :: !(P'.Maybe P'.Double), sensorMac :: !(P'.Maybe P'.ByteString),
                                 frequency :: !(P'.Maybe P'.Word32), pw_mgmt :: !(P'.Maybe P'.Bool), retry :: !(P'.Maybe P'.Bool),
                                 frametype :: !(P'.Maybe Gyrid.WiFi_DataRaw.FrameType), mgmt :: !(P'.Maybe Gyrid.WiFi_DataRaw.Mgmt),
                                 ctrl :: !(P'.Maybe Gyrid.WiFi_DataRaw.Ctrl), data' :: !(P'.Maybe Gyrid.WiFi_DataRaw.Data),
                                 hwid1 :: !(P'.Maybe P'.ByteString), hwid2 :: !(P'.Maybe P'.ByteString),
                                 ssi :: !(P'.Maybe P'.Int32)}
                  deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable WiFi_DataRaw where
  mergeAppend (WiFi_DataRaw x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11 x'12)
   (WiFi_DataRaw y'1 y'2 y'3 y'4 y'5 y'6 y'7 y'8 y'9 y'10 y'11 y'12)
   = WiFi_DataRaw (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
      (P'.mergeAppend x'7 y'7)
      (P'.mergeAppend x'8 y'8)
      (P'.mergeAppend x'9 y'9)
      (P'.mergeAppend x'10 y'10)
      (P'.mergeAppend x'11 y'11)
      (P'.mergeAppend x'12 y'12)

instance P'.Default WiFi_DataRaw where
  defaultValue
   = WiFi_DataRaw P'.defaultValue P'.defaultValue P'.defaultValue (Prelude'.Just Prelude'.False) (Prelude'.Just Prelude'.False)
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue

instance P'.Wire WiFi_DataRaw where
  wireSize ft' self'@(WiFi_DataRaw x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11 x'12)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeOpt 1 1 x'1 + P'.wireSizeOpt 1 12 x'2 + P'.wireSizeOpt 1 13 x'3 + P'.wireSizeOpt 1 8 x'4 +
             P'.wireSizeOpt 1 8 x'5
             + P'.wireSizeOpt 1 14 x'6
             + P'.wireSizeOpt 1 11 x'7
             + P'.wireSizeOpt 1 11 x'8
             + P'.wireSizeOpt 1 11 x'9
             + P'.wireSizeOpt 1 12 x'10
             + P'.wireSizeOpt 1 12 x'11
             + P'.wireSizeOpt 1 17 x'12)
  wirePut ft' self'@(WiFi_DataRaw x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11 x'12)
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
             P'.wirePutOpt 32 8 x'4
             P'.wirePutOpt 40 8 x'5
             P'.wirePutOpt 48 14 x'6
             P'.wirePutOpt 58 11 x'7
             P'.wirePutOpt 66 11 x'8
             P'.wirePutOpt 74 11 x'9
             P'.wirePutOpt 82 12 x'10
             P'.wirePutOpt 90 12 x'11
             P'.wirePutOpt 96 17 x'12
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
             32 -> Prelude'.fmap (\ !new'Field -> old'Self{pw_mgmt = Prelude'.Just new'Field}) (P'.wireGet 8)
             40 -> Prelude'.fmap (\ !new'Field -> old'Self{retry = Prelude'.Just new'Field}) (P'.wireGet 8)
             48 -> Prelude'.fmap (\ !new'Field -> old'Self{frametype = Prelude'.Just new'Field}) (P'.wireGet 14)
             58 -> Prelude'.fmap (\ !new'Field -> old'Self{mgmt = P'.mergeAppend (mgmt old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             66 -> Prelude'.fmap (\ !new'Field -> old'Self{ctrl = P'.mergeAppend (ctrl old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             74 -> Prelude'.fmap (\ !new'Field -> old'Self{data' = P'.mergeAppend (data' old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             82 -> Prelude'.fmap (\ !new'Field -> old'Self{hwid1 = Prelude'.Just new'Field}) (P'.wireGet 12)
             90 -> Prelude'.fmap (\ !new'Field -> old'Self{hwid2 = Prelude'.Just new'Field}) (P'.wireGet 12)
             96 -> Prelude'.fmap (\ !new'Field -> old'Self{ssi = Prelude'.Just new'Field}) (P'.wireGet 17)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> WiFi_DataRaw) WiFi_DataRaw where
  getVal m' f' = f' m'

instance P'.GPB WiFi_DataRaw

instance P'.ReflectDescriptor WiFi_DataRaw where
  getMessageInfo _
   = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [9, 18, 24, 32, 40, 48, 58, 66, 74, 82, 90, 96])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Gyrid.WiFi_DataRaw\", haskellPrefix = [], parentModule = [MName \"Gyrid\"], baseName = MName \"WiFi_DataRaw\"}, descFilePath = [\"Gyrid\",\"WiFi_DataRaw.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.WiFi_DataRaw.timestamp\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"WiFi_DataRaw\"], baseName' = FName \"timestamp\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 9}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.WiFi_DataRaw.sensorMac\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"WiFi_DataRaw\"], baseName' = FName \"sensorMac\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.WiFi_DataRaw.frequency\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"WiFi_DataRaw\"], baseName' = FName \"frequency\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.WiFi_DataRaw.pw_mgmt\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"WiFi_DataRaw\"], baseName' = FName \"pw_mgmt\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 32}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just \"false\", hsDefault = Just (HsDef'Bool False)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.WiFi_DataRaw.retry\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"WiFi_DataRaw\"], baseName' = FName \"retry\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 40}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just \"false\", hsDefault = Just (HsDef'Bool False)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.WiFi_DataRaw.frametype\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"WiFi_DataRaw\"], baseName' = FName \"frametype\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 48}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".Gyrid.WiFi_DataRaw.FrameType\", haskellPrefix = [], parentModule = [MName \"Gyrid\",MName \"WiFi_DataRaw\"], baseName = MName \"FrameType\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.WiFi_DataRaw.mgmt\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"WiFi_DataRaw\"], baseName' = FName \"mgmt\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 7}, wireTag = WireTag {getWireTag = 58}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Gyrid.WiFi_DataRaw.Mgmt\", haskellPrefix = [], parentModule = [MName \"Gyrid\",MName \"WiFi_DataRaw\"], baseName = MName \"Mgmt\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.WiFi_DataRaw.ctrl\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"WiFi_DataRaw\"], baseName' = FName \"ctrl\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 8}, wireTag = WireTag {getWireTag = 66}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Gyrid.WiFi_DataRaw.Ctrl\", haskellPrefix = [], parentModule = [MName \"Gyrid\",MName \"WiFi_DataRaw\"], baseName = MName \"Ctrl\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.WiFi_DataRaw.data\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"WiFi_DataRaw\"], baseName' = FName \"data'\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 9}, wireTag = WireTag {getWireTag = 74}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Gyrid.WiFi_DataRaw.Data\", haskellPrefix = [], parentModule = [MName \"Gyrid\",MName \"WiFi_DataRaw\"], baseName = MName \"Data\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.WiFi_DataRaw.hwid1\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"WiFi_DataRaw\"], baseName' = FName \"hwid1\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 10}, wireTag = WireTag {getWireTag = 82}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.WiFi_DataRaw.hwid2\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"WiFi_DataRaw\"], baseName' = FName \"hwid2\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 11}, wireTag = WireTag {getWireTag = 90}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.WiFi_DataRaw.ssi\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"WiFi_DataRaw\"], baseName' = FName \"ssi\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 12}, wireTag = WireTag {getWireTag = 96}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 17}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType WiFi_DataRaw where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg WiFi_DataRaw where
  textPut msg
   = do
       P'.tellT "timestamp" (timestamp msg)
       P'.tellT "sensorMac" (sensorMac msg)
       P'.tellT "frequency" (frequency msg)
       P'.tellT "pw_mgmt" (pw_mgmt msg)
       P'.tellT "retry" (retry msg)
       P'.tellT "frametype" (frametype msg)
       P'.tellT "mgmt" (mgmt msg)
       P'.tellT "ctrl" (ctrl msg)
       P'.tellT "data" (data' msg)
       P'.tellT "hwid1" (hwid1 msg)
       P'.tellT "hwid2" (hwid2 msg)
       P'.tellT "ssi" (ssi msg)
  textGet
   = do
       mods <- P'.sepEndBy
                (P'.choice
                  [parse'timestamp, parse'sensorMac, parse'frequency, parse'pw_mgmt, parse'retry, parse'frametype, parse'mgmt,
                   parse'ctrl, parse'data', parse'hwid1, parse'hwid2, parse'ssi])
                P'.spaces
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
        parse'pw_mgmt
         = P'.try
            (do
               v <- P'.getT "pw_mgmt"
               Prelude'.return (\ o -> o{pw_mgmt = v}))
        parse'retry
         = P'.try
            (do
               v <- P'.getT "retry"
               Prelude'.return (\ o -> o{retry = v}))
        parse'frametype
         = P'.try
            (do
               v <- P'.getT "frametype"
               Prelude'.return (\ o -> o{frametype = v}))
        parse'mgmt
         = P'.try
            (do
               v <- P'.getT "mgmt"
               Prelude'.return (\ o -> o{mgmt = v}))
        parse'ctrl
         = P'.try
            (do
               v <- P'.getT "ctrl"
               Prelude'.return (\ o -> o{ctrl = v}))
        parse'data'
         = P'.try
            (do
               v <- P'.getT "data"
               Prelude'.return (\ o -> o{data' = v}))
        parse'hwid1
         = P'.try
            (do
               v <- P'.getT "hwid1"
               Prelude'.return (\ o -> o{hwid1 = v}))
        parse'hwid2
         = P'.try
            (do
               v <- P'.getT "hwid2"
               Prelude'.return (\ o -> o{hwid2 = v}))
        parse'ssi
         = P'.try
            (do
               v <- P'.getT "ssi"
               Prelude'.return (\ o -> o{ssi = v}))