{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Gyrid.WiFi_DataRaw.Ctrl (Ctrl(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Gyrid.WiFi_DataRaw.Ctrl.SubType as Gyrid.WiFi_DataRaw.Ctrl (SubType)

data Ctrl = Ctrl{subType :: !(P'.Maybe Gyrid.WiFi_DataRaw.Ctrl.SubType)}
          deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable Ctrl where
  mergeAppend (Ctrl x'1) (Ctrl y'1) = Ctrl (P'.mergeAppend x'1 y'1)

instance P'.Default Ctrl where
  defaultValue = Ctrl P'.defaultValue

instance P'.Wire Ctrl where
  wireSize ft' self'@(Ctrl x'1)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 14 x'1)
  wirePut ft' self'@(Ctrl x'1)
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
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{subType = Prelude'.Just new'Field}) (P'.wireGet 14)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> Ctrl) Ctrl where
  getVal m' f' = f' m'

instance P'.GPB Ctrl

instance P'.ReflectDescriptor Ctrl where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [8])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Gyrid.WiFi_DataRaw.Ctrl\", haskellPrefix = [], parentModule = [MName \"Gyrid\",MName \"WiFi_DataRaw\"], baseName = MName \"Ctrl\"}, descFilePath = [\"Gyrid\",\"WiFi_DataRaw\",\"Ctrl.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.WiFi_DataRaw.Ctrl.subType\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"WiFi_DataRaw\",MName \"Ctrl\"], baseName' = FName \"subType\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".Gyrid.WiFi_DataRaw.Ctrl.SubType\", haskellPrefix = [], parentModule = [MName \"Gyrid\",MName \"WiFi_DataRaw\",MName \"Ctrl\"], baseName = MName \"SubType\"}), hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType Ctrl where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg Ctrl where
  textPut msg
   = do
       P'.tellT "subType" (subType msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'subType]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'subType
         = P'.try
            (do
               v <- P'.getT "subType"
               Prelude'.return (\ o -> o{subType = v}))