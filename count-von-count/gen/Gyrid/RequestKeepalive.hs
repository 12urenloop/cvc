{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Gyrid.RequestKeepalive (RequestKeepalive(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data RequestKeepalive = RequestKeepalive{interval :: !(P'.Maybe P'.Word32), enable :: !(P'.Maybe P'.Bool)}
                      deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable RequestKeepalive where
  mergeAppend (RequestKeepalive x'1 x'2) (RequestKeepalive y'1 y'2)
   = RequestKeepalive (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)

instance P'.Default RequestKeepalive where
  defaultValue = RequestKeepalive (Prelude'.Just 60) (Prelude'.Just Prelude'.True)

instance P'.Wire RequestKeepalive where
  wireSize ft' self'@(RequestKeepalive x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 13 x'1 + P'.wireSizeOpt 1 8 x'2)
  wirePut ft' self'@(RequestKeepalive x'1 x'2)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutOpt 8 13 x'1
             P'.wirePutOpt 16 8 x'2
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{interval = Prelude'.Just new'Field}) (P'.wireGet 13)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{enable = Prelude'.Just new'Field}) (P'.wireGet 8)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> RequestKeepalive) RequestKeepalive where
  getVal m' f' = f' m'

instance P'.GPB RequestKeepalive

instance P'.ReflectDescriptor RequestKeepalive where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [8, 16])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Gyrid.RequestKeepalive\", haskellPrefix = [], parentModule = [MName \"Gyrid\"], baseName = MName \"RequestKeepalive\"}, descFilePath = [\"Gyrid\",\"RequestKeepalive.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.RequestKeepalive.interval\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"RequestKeepalive\"], baseName' = FName \"interval\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Just \"60\", hsDefault = Just (HsDef'Integer 60)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.RequestKeepalive.enable\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"RequestKeepalive\"], baseName' = FName \"enable\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just \"true\", hsDefault = Just (HsDef'Bool True)}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType RequestKeepalive where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg RequestKeepalive where
  textPut msg
   = do
       P'.tellT "interval" (interval msg)
       P'.tellT "enable" (enable msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'interval, parse'enable]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'interval
         = P'.try
            (do
               v <- P'.getT "interval"
               Prelude'.return (\ o -> o{interval = v}))
        parse'enable
         = P'.try
            (do
               v <- P'.getT "enable"
               Prelude'.return (\ o -> o{enable = v}))