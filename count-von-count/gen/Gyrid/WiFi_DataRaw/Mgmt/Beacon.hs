{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Gyrid.WiFi_DataRaw.Mgmt.Beacon (Beacon(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Gyrid.WiFi_DataRaw.Mgmt.Beacon.Type as Gyrid.WiFi_DataRaw.Mgmt.Beacon (Type)

data Beacon = Beacon{type' :: !(P'.Maybe Gyrid.WiFi_DataRaw.Mgmt.Beacon.Type)}
            deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable Beacon where
  mergeAppend (Beacon x'1) (Beacon y'1) = Beacon (P'.mergeAppend x'1 y'1)

instance P'.Default Beacon where
  defaultValue = Beacon P'.defaultValue

instance P'.Wire Beacon where
  wireSize ft' self'@(Beacon x'1)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 14 x'1)
  wirePut ft' self'@(Beacon x'1)
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
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{type' = Prelude'.Just new'Field}) (P'.wireGet 14)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> Beacon) Beacon where
  getVal m' f' = f' m'

instance P'.GPB Beacon

instance P'.ReflectDescriptor Beacon where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [8])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Gyrid.WiFi_DataRaw.Mgmt.Beacon\", haskellPrefix = [], parentModule = [MName \"Gyrid\",MName \"WiFi_DataRaw\",MName \"Mgmt\"], baseName = MName \"Beacon\"}, descFilePath = [\"Gyrid\",\"WiFi_DataRaw\",\"Mgmt\",\"Beacon.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.WiFi_DataRaw.Mgmt.Beacon.type\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"WiFi_DataRaw\",MName \"Mgmt\",MName \"Beacon\"], baseName' = FName \"type'\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".Gyrid.WiFi_DataRaw.Mgmt.Beacon.Type\", haskellPrefix = [], parentModule = [MName \"Gyrid\",MName \"WiFi_DataRaw\",MName \"Mgmt\",MName \"Beacon\"], baseName = MName \"Type\"}), hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType Beacon where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg Beacon where
  textPut msg
   = do
       P'.tellT "type" (type' msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'type']) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'type'
         = P'.try
            (do
               v <- P'.getT "type"
               Prelude'.return (\ o -> o{type' = v}))