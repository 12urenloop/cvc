{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Gyrid.Uptime (Uptime(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data Uptime = Uptime{gyridStartup :: !(P'.Maybe P'.Double), systemStartup :: !(P'.Maybe P'.Double)}
            deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable Uptime where
  mergeAppend (Uptime x'1 x'2) (Uptime y'1 y'2) = Uptime (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)

instance P'.Default Uptime where
  defaultValue = Uptime P'.defaultValue P'.defaultValue

instance P'.Wire Uptime where
  wireSize ft' self'@(Uptime x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 1 x'1 + P'.wireSizeOpt 1 1 x'2)
  wirePut ft' self'@(Uptime x'1 x'2)
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
             P'.wirePutOpt 17 1 x'2
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             9 -> Prelude'.fmap (\ !new'Field -> old'Self{gyridStartup = Prelude'.Just new'Field}) (P'.wireGet 1)
             17 -> Prelude'.fmap (\ !new'Field -> old'Self{systemStartup = Prelude'.Just new'Field}) (P'.wireGet 1)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> Uptime) Uptime where
  getVal m' f' = f' m'

instance P'.GPB Uptime

instance P'.ReflectDescriptor Uptime where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [9, 17])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Gyrid.Uptime\", haskellPrefix = [], parentModule = [MName \"Gyrid\"], baseName = MName \"Uptime\"}, descFilePath = [\"Gyrid\",\"Uptime.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.Uptime.gyridStartup\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"Uptime\"], baseName' = FName \"gyridStartup\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 9}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.Uptime.systemStartup\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"Uptime\"], baseName' = FName \"systemStartup\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 17}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType Uptime where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg Uptime where
  textPut msg
   = do
       P'.tellT "gyridStartup" (gyridStartup msg)
       P'.tellT "systemStartup" (systemStartup msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'gyridStartup, parse'systemStartup]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'gyridStartup
         = P'.try
            (do
               v <- P'.getT "gyridStartup"
               Prelude'.return (\ o -> o{gyridStartup = v}))
        parse'systemStartup
         = P'.try
            (do
               v <- P'.getT "systemStartup"
               Prelude'.return (\ o -> o{systemStartup = v}))