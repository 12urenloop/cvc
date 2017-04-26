{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Gyrid.WiFi_DataRaw.Mgmt.ProbeReq (ProbeReq(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data ProbeReq = ProbeReq{hSsid :: !(P'.Maybe P'.ByteString)}
              deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable ProbeReq where
  mergeAppend (ProbeReq x'1) (ProbeReq y'1) = ProbeReq (P'.mergeAppend x'1 y'1)

instance P'.Default ProbeReq where
  defaultValue = ProbeReq P'.defaultValue

instance P'.Wire ProbeReq where
  wireSize ft' self'@(ProbeReq x'1)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 12 x'1)
  wirePut ft' self'@(ProbeReq x'1)
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
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{hSsid = Prelude'.Just new'Field}) (P'.wireGet 12)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> ProbeReq) ProbeReq where
  getVal m' f' = f' m'

instance P'.GPB ProbeReq

instance P'.ReflectDescriptor ProbeReq where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Gyrid.WiFi_DataRaw.Mgmt.ProbeReq\", haskellPrefix = [], parentModule = [MName \"Gyrid\",MName \"WiFi_DataRaw\",MName \"Mgmt\"], baseName = MName \"ProbeReq\"}, descFilePath = [\"Gyrid\",\"WiFi_DataRaw\",\"Mgmt\",\"ProbeReq.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.WiFi_DataRaw.Mgmt.ProbeReq.hSsid\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"WiFi_DataRaw\",MName \"Mgmt\",MName \"ProbeReq\"], baseName' = FName \"hSsid\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType ProbeReq where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg ProbeReq where
  textPut msg
   = do
       P'.tellT "hSsid" (hSsid msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'hSsid]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'hSsid
         = P'.try
            (do
               v <- P'.getT "hSsid"
               Prelude'.return (\ o -> o{hSsid = v}))