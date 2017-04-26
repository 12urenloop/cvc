{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Gyrid.WiFi_DataRaw.Data (Data(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data Data = Data{from_ds :: !(P'.Maybe P'.Bool), to_ds :: !(P'.Maybe P'.Bool)}
          deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable Data where
  mergeAppend (Data x'1 x'2) (Data y'1 y'2) = Data (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)

instance P'.Default Data where
  defaultValue = Data P'.defaultValue P'.defaultValue

instance P'.Wire Data where
  wireSize ft' self'@(Data x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 8 x'1 + P'.wireSizeOpt 1 8 x'2)
  wirePut ft' self'@(Data x'1 x'2)
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
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{from_ds = Prelude'.Just new'Field}) (P'.wireGet 8)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{to_ds = Prelude'.Just new'Field}) (P'.wireGet 8)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> Data) Data where
  getVal m' f' = f' m'

instance P'.GPB Data

instance P'.ReflectDescriptor Data where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [8, 16])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Gyrid.WiFi_DataRaw.Data\", haskellPrefix = [], parentModule = [MName \"Gyrid\",MName \"WiFi_DataRaw\"], baseName = MName \"Data\"}, descFilePath = [\"Gyrid\",\"WiFi_DataRaw\",\"Data.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.WiFi_DataRaw.Data.from_ds\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"WiFi_DataRaw\",MName \"Data\"], baseName' = FName \"from_ds\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.WiFi_DataRaw.Data.to_ds\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"WiFi_DataRaw\",MName \"Data\"], baseName' = FName \"to_ds\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType Data where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg Data where
  textPut msg
   = do
       P'.tellT "from_ds" (from_ds msg)
       P'.tellT "to_ds" (to_ds msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'from_ds, parse'to_ds]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'from_ds
         = P'.try
            (do
               v <- P'.getT "from_ds"
               Prelude'.return (\ o -> o{from_ds = v}))
        parse'to_ds
         = P'.try
            (do
               v <- P'.getT "to_ds"
               Prelude'.return (\ o -> o{to_ds = v}))