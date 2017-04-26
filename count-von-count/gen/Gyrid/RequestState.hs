{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Gyrid.RequestState (RequestState(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data RequestState = RequestState{bluetooth_enableInquiry :: !(P'.Maybe P'.Bool), enableScanning :: !(P'.Maybe P'.Bool),
                                 wifi_enableFrequency :: !(P'.Maybe P'.Bool), wifi_enableFrequencyLoop :: !(P'.Maybe P'.Bool),
                                 enableAntenna :: !(P'.Maybe P'.Bool)}
                  deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable RequestState where
  mergeAppend (RequestState x'1 x'2 x'3 x'4 x'5) (RequestState y'1 y'2 y'3 y'4 y'5)
   = RequestState (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)

instance P'.Default RequestState where
  defaultValue
   = RequestState (Prelude'.Just Prelude'.False) (Prelude'.Just Prelude'.True) (Prelude'.Just Prelude'.False)
      (Prelude'.Just Prelude'.False)
      (Prelude'.Just Prelude'.False)

instance P'.Wire RequestState where
  wireSize ft' self'@(RequestState x'1 x'2 x'3 x'4 x'5)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeOpt 1 8 x'1 + P'.wireSizeOpt 1 8 x'2 + P'.wireSizeOpt 1 8 x'3 + P'.wireSizeOpt 1 8 x'4 +
             P'.wireSizeOpt 1 8 x'5)
  wirePut ft' self'@(RequestState x'1 x'2 x'3 x'4 x'5)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutOpt 8 8 x'1
             P'.wirePutOpt 16 8 x'2
             P'.wirePutOpt 24 8 x'3
             P'.wirePutOpt 32 8 x'4
             P'.wirePutOpt 40 8 x'5
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{bluetooth_enableInquiry = Prelude'.Just new'Field}) (P'.wireGet 8)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{enableScanning = Prelude'.Just new'Field}) (P'.wireGet 8)
             24 -> Prelude'.fmap (\ !new'Field -> old'Self{wifi_enableFrequency = Prelude'.Just new'Field}) (P'.wireGet 8)
             32 -> Prelude'.fmap (\ !new'Field -> old'Self{wifi_enableFrequencyLoop = Prelude'.Just new'Field}) (P'.wireGet 8)
             40 -> Prelude'.fmap (\ !new'Field -> old'Self{enableAntenna = Prelude'.Just new'Field}) (P'.wireGet 8)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> RequestState) RequestState where
  getVal m' f' = f' m'

instance P'.GPB RequestState

instance P'.ReflectDescriptor RequestState where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [8, 16, 24, 32, 40])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Gyrid.RequestState\", haskellPrefix = [], parentModule = [MName \"Gyrid\"], baseName = MName \"RequestState\"}, descFilePath = [\"Gyrid\",\"RequestState.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.RequestState.bluetooth_enableInquiry\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"RequestState\"], baseName' = FName \"bluetooth_enableInquiry\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just \"false\", hsDefault = Just (HsDef'Bool False)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.RequestState.enableScanning\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"RequestState\"], baseName' = FName \"enableScanning\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just \"true\", hsDefault = Just (HsDef'Bool True)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.RequestState.wifi_enableFrequency\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"RequestState\"], baseName' = FName \"wifi_enableFrequency\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just \"false\", hsDefault = Just (HsDef'Bool False)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.RequestState.wifi_enableFrequencyLoop\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"RequestState\"], baseName' = FName \"wifi_enableFrequencyLoop\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 32}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just \"false\", hsDefault = Just (HsDef'Bool False)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.RequestState.enableAntenna\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"RequestState\"], baseName' = FName \"enableAntenna\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 40}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just \"false\", hsDefault = Just (HsDef'Bool False)}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType RequestState where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg RequestState where
  textPut msg
   = do
       P'.tellT "bluetooth_enableInquiry" (bluetooth_enableInquiry msg)
       P'.tellT "enableScanning" (enableScanning msg)
       P'.tellT "wifi_enableFrequency" (wifi_enableFrequency msg)
       P'.tellT "wifi_enableFrequencyLoop" (wifi_enableFrequencyLoop msg)
       P'.tellT "enableAntenna" (enableAntenna msg)
  textGet
   = do
       mods <- P'.sepEndBy
                (P'.choice
                  [parse'bluetooth_enableInquiry, parse'enableScanning, parse'wifi_enableFrequency, parse'wifi_enableFrequencyLoop,
                   parse'enableAntenna])
                P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'bluetooth_enableInquiry
         = P'.try
            (do
               v <- P'.getT "bluetooth_enableInquiry"
               Prelude'.return (\ o -> o{bluetooth_enableInquiry = v}))
        parse'enableScanning
         = P'.try
            (do
               v <- P'.getT "enableScanning"
               Prelude'.return (\ o -> o{enableScanning = v}))
        parse'wifi_enableFrequency
         = P'.try
            (do
               v <- P'.getT "wifi_enableFrequency"
               Prelude'.return (\ o -> o{wifi_enableFrequency = v}))
        parse'wifi_enableFrequencyLoop
         = P'.try
            (do
               v <- P'.getT "wifi_enableFrequencyLoop"
               Prelude'.return (\ o -> o{wifi_enableFrequencyLoop = v}))
        parse'enableAntenna
         = P'.try
            (do
               v <- P'.getT "enableAntenna"
               Prelude'.return (\ o -> o{enableAntenna = v}))