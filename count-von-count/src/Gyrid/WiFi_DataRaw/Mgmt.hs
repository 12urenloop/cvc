{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Gyrid.WiFi_DataRaw.Mgmt (Mgmt(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Gyrid.WiFi_DataRaw.Mgmt.Beacon as Gyrid.WiFi_DataRaw.Mgmt (Beacon)
import qualified Gyrid.WiFi_DataRaw.Mgmt.ProbeReq as Gyrid.WiFi_DataRaw.Mgmt (ProbeReq)
import qualified Gyrid.WiFi_DataRaw.Mgmt.SubType as Gyrid.WiFi_DataRaw.Mgmt (SubType)
 
data Mgmt = Mgmt{subType :: !(P'.Maybe Gyrid.WiFi_DataRaw.Mgmt.SubType), beacon :: !(P'.Maybe Gyrid.WiFi_DataRaw.Mgmt.Beacon),
                 probeReq :: !(P'.Maybe Gyrid.WiFi_DataRaw.Mgmt.ProbeReq)}
          deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable Mgmt where
  mergeAppend (Mgmt x'1 x'2 x'3) (Mgmt y'1 y'2 y'3)
   = Mgmt (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3)
 
instance P'.Default Mgmt where
  defaultValue = Mgmt P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire Mgmt where
  wireSize ft' self'@(Mgmt x'1 x'2 x'3)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 14 x'1 + P'.wireSizeOpt 1 11 x'2 + P'.wireSizeOpt 1 11 x'3)
  wirePut ft' self'@(Mgmt x'1 x'2 x'3)
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
             P'.wirePutOpt 18 11 x'2
             P'.wirePutOpt 26 11 x'3
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{subType = Prelude'.Just new'Field}) (P'.wireGet 14)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{beacon = P'.mergeAppend (beacon old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{probeReq = P'.mergeAppend (probeReq old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> Mgmt) Mgmt where
  getVal m' f' = f' m'
 
instance P'.GPB Mgmt
 
instance P'.ReflectDescriptor Mgmt where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [8, 18, 26])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Gyrid.WiFi_DataRaw.Mgmt\", haskellPrefix = [], parentModule = [MName \"Gyrid\",MName \"WiFi_DataRaw\"], baseName = MName \"Mgmt\"}, descFilePath = [\"Gyrid\",\"WiFi_DataRaw\",\"Mgmt.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.WiFi_DataRaw.Mgmt.subType\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"WiFi_DataRaw\",MName \"Mgmt\"], baseName' = FName \"subType\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".Gyrid.WiFi_DataRaw.Mgmt.SubType\", haskellPrefix = [], parentModule = [MName \"Gyrid\",MName \"WiFi_DataRaw\",MName \"Mgmt\"], baseName = MName \"SubType\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.WiFi_DataRaw.Mgmt.beacon\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"WiFi_DataRaw\",MName \"Mgmt\"], baseName' = FName \"beacon\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Gyrid.WiFi_DataRaw.Mgmt.Beacon\", haskellPrefix = [], parentModule = [MName \"Gyrid\",MName \"WiFi_DataRaw\",MName \"Mgmt\"], baseName = MName \"Beacon\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.WiFi_DataRaw.Mgmt.probeReq\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"WiFi_DataRaw\",MName \"Mgmt\"], baseName' = FName \"probeReq\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Gyrid.WiFi_DataRaw.Mgmt.ProbeReq\", haskellPrefix = [], parentModule = [MName \"Gyrid\",MName \"WiFi_DataRaw\",MName \"Mgmt\"], baseName = MName \"ProbeReq\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"