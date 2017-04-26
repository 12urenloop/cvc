{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Gyrid.Msg (Msg(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Gyrid.AntennaTurn as Gyrid (AntennaTurn)
import qualified Gyrid.Bluetooth_DataIO as Gyrid (Bluetooth_DataIO)
import qualified Gyrid.Bluetooth_DataRaw as Gyrid (Bluetooth_DataRaw)
import qualified Gyrid.Bluetooth_StateInquiry as Gyrid (Bluetooth_StateInquiry)
import qualified Gyrid.Info as Gyrid (Info)
import qualified Gyrid.Msg.Type as Gyrid.Msg (Type)
import qualified Gyrid.RequestCaching as Gyrid (RequestCaching)
import qualified Gyrid.RequestKeepalive as Gyrid (RequestKeepalive)
import qualified Gyrid.RequestStartdata as Gyrid (RequestStartdata)
import qualified Gyrid.RequestState as Gyrid (RequestState)
import qualified Gyrid.ScanPattern as Gyrid (ScanPattern)
import qualified Gyrid.StateAntenna as Gyrid (StateAntenna)
import qualified Gyrid.StateGyrid as Gyrid (StateGyrid)
import qualified Gyrid.StateScanning as Gyrid (StateScanning)
import qualified Gyrid.Uptime as Gyrid (Uptime)
import qualified Gyrid.WiFi_DataDevRaw as Gyrid (WiFi_DataDevRaw)
import qualified Gyrid.WiFi_DataIO as Gyrid (WiFi_DataIO)
import qualified Gyrid.WiFi_DataRaw as Gyrid (WiFi_DataRaw)
import qualified Gyrid.WiFi_StateFrequency as Gyrid (WiFi_StateFrequency)
import qualified Gyrid.WiFi_StateFrequencyLoop as Gyrid (WiFi_StateFrequencyLoop)

data Msg = Msg{type' :: !(Gyrid.Msg.Type), ack :: !(P'.Maybe P'.ByteString), cached :: !(P'.Maybe P'.Bool),
               bluetooth_dataIO :: !(P'.Maybe Gyrid.Bluetooth_DataIO), bluetooth_dataRaw :: !(P'.Maybe Gyrid.Bluetooth_DataRaw),
               bluetooth_stateInquiry :: !(P'.Maybe Gyrid.Bluetooth_StateInquiry),
               wifi_stateFrequency :: !(P'.Maybe Gyrid.WiFi_StateFrequency),
               wifi_stateFrequencyLoop :: !(P'.Maybe Gyrid.WiFi_StateFrequencyLoop), wifi_dataRaw :: !(P'.Maybe Gyrid.WiFi_DataRaw),
               wifi_dataDevRaw :: !(P'.Maybe Gyrid.WiFi_DataDevRaw), wifi_dataIO :: !(P'.Maybe Gyrid.WiFi_DataIO),
               info :: !(P'.Maybe Gyrid.Info), stateScanning :: !(P'.Maybe Gyrid.StateScanning),
               stateGyrid :: !(P'.Maybe Gyrid.StateGyrid), stateAntenna :: !(P'.Maybe Gyrid.StateAntenna),
               uptime :: !(P'.Maybe Gyrid.Uptime), requestKeepalive :: !(P'.Maybe Gyrid.RequestKeepalive),
               requestUptime :: !(P'.Maybe P'.Bool), requestCaching :: !(P'.Maybe Gyrid.RequestCaching),
               requestStartdata :: !(P'.Maybe Gyrid.RequestStartdata), requestState :: !(P'.Maybe Gyrid.RequestState),
               hostname :: !(P'.Maybe P'.Utf8), antennaTurn :: !(P'.Maybe Gyrid.AntennaTurn),
               scanPattern :: !(P'.Maybe Gyrid.ScanPattern), success :: !(P'.Maybe P'.Bool)}
         deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable Msg where
  mergeAppend
   (Msg x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11 x'12 x'13 x'14 x'15 x'16 x'17 x'18 x'19 x'20 x'21 x'22 x'23 x'24 x'25)
   (Msg y'1 y'2 y'3 y'4 y'5 y'6 y'7 y'8 y'9 y'10 y'11 y'12 y'13 y'14 y'15 y'16 y'17 y'18 y'19 y'20 y'21 y'22 y'23 y'24 y'25)
   = Msg (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
      (P'.mergeAppend x'7 y'7)
      (P'.mergeAppend x'8 y'8)
      (P'.mergeAppend x'9 y'9)
      (P'.mergeAppend x'10 y'10)
      (P'.mergeAppend x'11 y'11)
      (P'.mergeAppend x'12 y'12)
      (P'.mergeAppend x'13 y'13)
      (P'.mergeAppend x'14 y'14)
      (P'.mergeAppend x'15 y'15)
      (P'.mergeAppend x'16 y'16)
      (P'.mergeAppend x'17 y'17)
      (P'.mergeAppend x'18 y'18)
      (P'.mergeAppend x'19 y'19)
      (P'.mergeAppend x'20 y'20)
      (P'.mergeAppend x'21 y'21)
      (P'.mergeAppend x'22 y'22)
      (P'.mergeAppend x'23 y'23)
      (P'.mergeAppend x'24 y'24)
      (P'.mergeAppend x'25 y'25)

instance P'.Default Msg where
  defaultValue
   = Msg P'.defaultValue P'.defaultValue (Prelude'.Just Prelude'.False) P'.defaultValue P'.defaultValue P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      (Prelude'.Just Prelude'.True)
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue

instance P'.Wire Msg where
  wireSize ft'
   self'@(Msg x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11 x'12 x'13 x'14 x'15 x'16 x'17 x'18 x'19 x'20 x'21 x'22 x'23 x'24 x'25)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeReq 1 14 x'1 + P'.wireSizeOpt 1 12 x'2 + P'.wireSizeOpt 1 8 x'3 + P'.wireSizeOpt 1 11 x'4 +
             P'.wireSizeOpt 1 11 x'5
             + P'.wireSizeOpt 1 11 x'6
             + P'.wireSizeOpt 1 11 x'7
             + P'.wireSizeOpt 1 11 x'8
             + P'.wireSizeOpt 1 11 x'9
             + P'.wireSizeOpt 1 11 x'10
             + P'.wireSizeOpt 1 11 x'11
             + P'.wireSizeOpt 1 11 x'12
             + P'.wireSizeOpt 1 11 x'13
             + P'.wireSizeOpt 1 11 x'14
             + P'.wireSizeOpt 1 11 x'15
             + P'.wireSizeOpt 2 11 x'16
             + P'.wireSizeOpt 2 11 x'17
             + P'.wireSizeOpt 2 8 x'18
             + P'.wireSizeOpt 2 11 x'19
             + P'.wireSizeOpt 2 11 x'20
             + P'.wireSizeOpt 2 11 x'21
             + P'.wireSizeOpt 2 9 x'22
             + P'.wireSizeOpt 2 11 x'23
             + P'.wireSizeOpt 2 11 x'24
             + P'.wireSizeOpt 2 8 x'25)
  wirePut ft'
   self'@(Msg x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11 x'12 x'13 x'14 x'15 x'16 x'17 x'18 x'19 x'20 x'21 x'22 x'23 x'24 x'25)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutReq 8 14 x'1
             P'.wirePutOpt 18 12 x'2
             P'.wirePutOpt 24 8 x'3
             P'.wirePutOpt 34 11 x'4
             P'.wirePutOpt 42 11 x'5
             P'.wirePutOpt 50 11 x'6
             P'.wirePutOpt 58 11 x'7
             P'.wirePutOpt 66 11 x'8
             P'.wirePutOpt 74 11 x'9
             P'.wirePutOpt 82 11 x'10
             P'.wirePutOpt 90 11 x'11
             P'.wirePutOpt 98 11 x'12
             P'.wirePutOpt 106 11 x'13
             P'.wirePutOpt 114 11 x'14
             P'.wirePutOpt 122 11 x'15
             P'.wirePutOpt 130 11 x'16
             P'.wirePutOpt 138 11 x'17
             P'.wirePutOpt 144 8 x'18
             P'.wirePutOpt 154 11 x'19
             P'.wirePutOpt 162 11 x'20
             P'.wirePutOpt 170 11 x'21
             P'.wirePutOpt 178 9 x'22
             P'.wirePutOpt 186 11 x'23
             P'.wirePutOpt 194 11 x'24
             P'.wirePutOpt 200 8 x'25
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{type' = new'Field}) (P'.wireGet 14)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{ack = Prelude'.Just new'Field}) (P'.wireGet 12)
             24 -> Prelude'.fmap (\ !new'Field -> old'Self{cached = Prelude'.Just new'Field}) (P'.wireGet 8)
             34 -> Prelude'.fmap
                    (\ !new'Field ->
                      old'Self{bluetooth_dataIO = P'.mergeAppend (bluetooth_dataIO old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             42 -> Prelude'.fmap
                    (\ !new'Field ->
                      old'Self{bluetooth_dataRaw = P'.mergeAppend (bluetooth_dataRaw old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             50 -> Prelude'.fmap
                    (\ !new'Field ->
                      old'Self{bluetooth_stateInquiry = P'.mergeAppend (bluetooth_stateInquiry old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             58 -> Prelude'.fmap
                    (\ !new'Field ->
                      old'Self{wifi_stateFrequency = P'.mergeAppend (wifi_stateFrequency old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             66 -> Prelude'.fmap
                    (\ !new'Field ->
                      old'Self{wifi_stateFrequencyLoop =
                                P'.mergeAppend (wifi_stateFrequencyLoop old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             74 -> Prelude'.fmap
                    (\ !new'Field -> old'Self{wifi_dataRaw = P'.mergeAppend (wifi_dataRaw old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             82 -> Prelude'.fmap
                    (\ !new'Field ->
                      old'Self{wifi_dataDevRaw = P'.mergeAppend (wifi_dataDevRaw old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             90 -> Prelude'.fmap
                    (\ !new'Field -> old'Self{wifi_dataIO = P'.mergeAppend (wifi_dataIO old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             98 -> Prelude'.fmap (\ !new'Field -> old'Self{info = P'.mergeAppend (info old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             106 -> Prelude'.fmap
                     (\ !new'Field -> old'Self{stateScanning = P'.mergeAppend (stateScanning old'Self) (Prelude'.Just new'Field)})
                     (P'.wireGet 11)
             114 -> Prelude'.fmap
                     (\ !new'Field -> old'Self{stateGyrid = P'.mergeAppend (stateGyrid old'Self) (Prelude'.Just new'Field)})
                     (P'.wireGet 11)
             122 -> Prelude'.fmap
                     (\ !new'Field -> old'Self{stateAntenna = P'.mergeAppend (stateAntenna old'Self) (Prelude'.Just new'Field)})
                     (P'.wireGet 11)
             130 -> Prelude'.fmap (\ !new'Field -> old'Self{uptime = P'.mergeAppend (uptime old'Self) (Prelude'.Just new'Field)})
                     (P'.wireGet 11)
             138 -> Prelude'.fmap
                     (\ !new'Field ->
                       old'Self{requestKeepalive = P'.mergeAppend (requestKeepalive old'Self) (Prelude'.Just new'Field)})
                     (P'.wireGet 11)
             144 -> Prelude'.fmap (\ !new'Field -> old'Self{requestUptime = Prelude'.Just new'Field}) (P'.wireGet 8)
             154 -> Prelude'.fmap
                     (\ !new'Field -> old'Self{requestCaching = P'.mergeAppend (requestCaching old'Self) (Prelude'.Just new'Field)})
                     (P'.wireGet 11)
             162 -> Prelude'.fmap
                     (\ !new'Field ->
                       old'Self{requestStartdata = P'.mergeAppend (requestStartdata old'Self) (Prelude'.Just new'Field)})
                     (P'.wireGet 11)
             170 -> Prelude'.fmap
                     (\ !new'Field -> old'Self{requestState = P'.mergeAppend (requestState old'Self) (Prelude'.Just new'Field)})
                     (P'.wireGet 11)
             178 -> Prelude'.fmap (\ !new'Field -> old'Self{hostname = Prelude'.Just new'Field}) (P'.wireGet 9)
             186 -> Prelude'.fmap
                     (\ !new'Field -> old'Self{antennaTurn = P'.mergeAppend (antennaTurn old'Self) (Prelude'.Just new'Field)})
                     (P'.wireGet 11)
             194 -> Prelude'.fmap
                     (\ !new'Field -> old'Self{scanPattern = P'.mergeAppend (scanPattern old'Self) (Prelude'.Just new'Field)})
                     (P'.wireGet 11)
             200 -> Prelude'.fmap (\ !new'Field -> old'Self{success = Prelude'.Just new'Field}) (P'.wireGet 8)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> Msg) Msg where
  getVal m' f' = f' m'

instance P'.GPB Msg

instance P'.ReflectDescriptor Msg where
  getMessageInfo _
   = P'.GetMessageInfo (P'.fromDistinctAscList [8])
      (P'.fromDistinctAscList
        [8, 18, 24, 34, 42, 50, 58, 66, 74, 82, 90, 98, 106, 114, 122, 130, 138, 144, 154, 162, 170, 178, 186, 194, 200])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Gyrid.Msg\", haskellPrefix = [], parentModule = [MName \"Gyrid\"], baseName = MName \"Msg\"}, descFilePath = [\"Gyrid\",\"Msg.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.Msg.type\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"Msg\"], baseName' = FName \"type'\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".Gyrid.Msg.Type\", haskellPrefix = [], parentModule = [MName \"Gyrid\",MName \"Msg\"], baseName = MName \"Type\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.Msg.ack\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"Msg\"], baseName' = FName \"ack\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.Msg.cached\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"Msg\"], baseName' = FName \"cached\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just \"false\", hsDefault = Just (HsDef'Bool False)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.Msg.bluetooth_dataIO\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"Msg\"], baseName' = FName \"bluetooth_dataIO\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Gyrid.Bluetooth_DataIO\", haskellPrefix = [], parentModule = [MName \"Gyrid\"], baseName = MName \"Bluetooth_DataIO\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.Msg.bluetooth_dataRaw\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"Msg\"], baseName' = FName \"bluetooth_dataRaw\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 42}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Gyrid.Bluetooth_DataRaw\", haskellPrefix = [], parentModule = [MName \"Gyrid\"], baseName = MName \"Bluetooth_DataRaw\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.Msg.bluetooth_stateInquiry\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"Msg\"], baseName' = FName \"bluetooth_stateInquiry\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 50}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Gyrid.Bluetooth_StateInquiry\", haskellPrefix = [], parentModule = [MName \"Gyrid\"], baseName = MName \"Bluetooth_StateInquiry\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.Msg.wifi_stateFrequency\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"Msg\"], baseName' = FName \"wifi_stateFrequency\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 7}, wireTag = WireTag {getWireTag = 58}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Gyrid.WiFi_StateFrequency\", haskellPrefix = [], parentModule = [MName \"Gyrid\"], baseName = MName \"WiFi_StateFrequency\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.Msg.wifi_stateFrequencyLoop\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"Msg\"], baseName' = FName \"wifi_stateFrequencyLoop\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 8}, wireTag = WireTag {getWireTag = 66}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Gyrid.WiFi_StateFrequencyLoop\", haskellPrefix = [], parentModule = [MName \"Gyrid\"], baseName = MName \"WiFi_StateFrequencyLoop\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.Msg.wifi_dataRaw\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"Msg\"], baseName' = FName \"wifi_dataRaw\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 9}, wireTag = WireTag {getWireTag = 74}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Gyrid.WiFi_DataRaw\", haskellPrefix = [], parentModule = [MName \"Gyrid\"], baseName = MName \"WiFi_DataRaw\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.Msg.wifi_dataDevRaw\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"Msg\"], baseName' = FName \"wifi_dataDevRaw\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 10}, wireTag = WireTag {getWireTag = 82}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Gyrid.WiFi_DataDevRaw\", haskellPrefix = [], parentModule = [MName \"Gyrid\"], baseName = MName \"WiFi_DataDevRaw\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.Msg.wifi_dataIO\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"Msg\"], baseName' = FName \"wifi_dataIO\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 11}, wireTag = WireTag {getWireTag = 90}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Gyrid.WiFi_DataIO\", haskellPrefix = [], parentModule = [MName \"Gyrid\"], baseName = MName \"WiFi_DataIO\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.Msg.info\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"Msg\"], baseName' = FName \"info\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 12}, wireTag = WireTag {getWireTag = 98}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Gyrid.Info\", haskellPrefix = [], parentModule = [MName \"Gyrid\"], baseName = MName \"Info\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.Msg.stateScanning\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"Msg\"], baseName' = FName \"stateScanning\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 13}, wireTag = WireTag {getWireTag = 106}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Gyrid.StateScanning\", haskellPrefix = [], parentModule = [MName \"Gyrid\"], baseName = MName \"StateScanning\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.Msg.stateGyrid\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"Msg\"], baseName' = FName \"stateGyrid\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 14}, wireTag = WireTag {getWireTag = 114}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Gyrid.StateGyrid\", haskellPrefix = [], parentModule = [MName \"Gyrid\"], baseName = MName \"StateGyrid\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.Msg.stateAntenna\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"Msg\"], baseName' = FName \"stateAntenna\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 15}, wireTag = WireTag {getWireTag = 122}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Gyrid.StateAntenna\", haskellPrefix = [], parentModule = [MName \"Gyrid\"], baseName = MName \"StateAntenna\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.Msg.uptime\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"Msg\"], baseName' = FName \"uptime\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 16}, wireTag = WireTag {getWireTag = 130}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Gyrid.Uptime\", haskellPrefix = [], parentModule = [MName \"Gyrid\"], baseName = MName \"Uptime\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.Msg.requestKeepalive\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"Msg\"], baseName' = FName \"requestKeepalive\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 17}, wireTag = WireTag {getWireTag = 138}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Gyrid.RequestKeepalive\", haskellPrefix = [], parentModule = [MName \"Gyrid\"], baseName = MName \"RequestKeepalive\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.Msg.requestUptime\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"Msg\"], baseName' = FName \"requestUptime\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 18}, wireTag = WireTag {getWireTag = 144}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just \"true\", hsDefault = Just (HsDef'Bool True)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.Msg.requestCaching\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"Msg\"], baseName' = FName \"requestCaching\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 19}, wireTag = WireTag {getWireTag = 154}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Gyrid.RequestCaching\", haskellPrefix = [], parentModule = [MName \"Gyrid\"], baseName = MName \"RequestCaching\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.Msg.requestStartdata\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"Msg\"], baseName' = FName \"requestStartdata\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 20}, wireTag = WireTag {getWireTag = 162}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Gyrid.RequestStartdata\", haskellPrefix = [], parentModule = [MName \"Gyrid\"], baseName = MName \"RequestStartdata\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.Msg.requestState\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"Msg\"], baseName' = FName \"requestState\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 21}, wireTag = WireTag {getWireTag = 170}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Gyrid.RequestState\", haskellPrefix = [], parentModule = [MName \"Gyrid\"], baseName = MName \"RequestState\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.Msg.hostname\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"Msg\"], baseName' = FName \"hostname\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 22}, wireTag = WireTag {getWireTag = 178}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.Msg.antennaTurn\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"Msg\"], baseName' = FName \"antennaTurn\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 23}, wireTag = WireTag {getWireTag = 186}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Gyrid.AntennaTurn\", haskellPrefix = [], parentModule = [MName \"Gyrid\"], baseName = MName \"AntennaTurn\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.Msg.scanPattern\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"Msg\"], baseName' = FName \"scanPattern\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 24}, wireTag = WireTag {getWireTag = 194}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Gyrid.ScanPattern\", haskellPrefix = [], parentModule = [MName \"Gyrid\"], baseName = MName \"ScanPattern\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Gyrid.Msg.success\", haskellPrefix' = [], parentModule' = [MName \"Gyrid\",MName \"Msg\"], baseName' = FName \"success\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 25}, wireTag = WireTag {getWireTag = 200}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType Msg where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg Msg where
  textPut msg
   = do
       P'.tellT "type" (type' msg)
       P'.tellT "ack" (ack msg)
       P'.tellT "cached" (cached msg)
       P'.tellT "bluetooth_dataIO" (bluetooth_dataIO msg)
       P'.tellT "bluetooth_dataRaw" (bluetooth_dataRaw msg)
       P'.tellT "bluetooth_stateInquiry" (bluetooth_stateInquiry msg)
       P'.tellT "wifi_stateFrequency" (wifi_stateFrequency msg)
       P'.tellT "wifi_stateFrequencyLoop" (wifi_stateFrequencyLoop msg)
       P'.tellT "wifi_dataRaw" (wifi_dataRaw msg)
       P'.tellT "wifi_dataDevRaw" (wifi_dataDevRaw msg)
       P'.tellT "wifi_dataIO" (wifi_dataIO msg)
       P'.tellT "info" (info msg)
       P'.tellT "stateScanning" (stateScanning msg)
       P'.tellT "stateGyrid" (stateGyrid msg)
       P'.tellT "stateAntenna" (stateAntenna msg)
       P'.tellT "uptime" (uptime msg)
       P'.tellT "requestKeepalive" (requestKeepalive msg)
       P'.tellT "requestUptime" (requestUptime msg)
       P'.tellT "requestCaching" (requestCaching msg)
       P'.tellT "requestStartdata" (requestStartdata msg)
       P'.tellT "requestState" (requestState msg)
       P'.tellT "hostname" (hostname msg)
       P'.tellT "antennaTurn" (antennaTurn msg)
       P'.tellT "scanPattern" (scanPattern msg)
       P'.tellT "success" (success msg)
  textGet
   = do
       mods <- P'.sepEndBy
                (P'.choice
                  [parse'type', parse'ack, parse'cached, parse'bluetooth_dataIO, parse'bluetooth_dataRaw,
                   parse'bluetooth_stateInquiry, parse'wifi_stateFrequency, parse'wifi_stateFrequencyLoop, parse'wifi_dataRaw,
                   parse'wifi_dataDevRaw, parse'wifi_dataIO, parse'info, parse'stateScanning, parse'stateGyrid, parse'stateAntenna,
                   parse'uptime, parse'requestKeepalive, parse'requestUptime, parse'requestCaching, parse'requestStartdata,
                   parse'requestState, parse'hostname, parse'antennaTurn, parse'scanPattern, parse'success])
                P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'type'
         = P'.try
            (do
               v <- P'.getT "type"
               Prelude'.return (\ o -> o{type' = v}))
        parse'ack
         = P'.try
            (do
               v <- P'.getT "ack"
               Prelude'.return (\ o -> o{ack = v}))
        parse'cached
         = P'.try
            (do
               v <- P'.getT "cached"
               Prelude'.return (\ o -> o{cached = v}))
        parse'bluetooth_dataIO
         = P'.try
            (do
               v <- P'.getT "bluetooth_dataIO"
               Prelude'.return (\ o -> o{bluetooth_dataIO = v}))
        parse'bluetooth_dataRaw
         = P'.try
            (do
               v <- P'.getT "bluetooth_dataRaw"
               Prelude'.return (\ o -> o{bluetooth_dataRaw = v}))
        parse'bluetooth_stateInquiry
         = P'.try
            (do
               v <- P'.getT "bluetooth_stateInquiry"
               Prelude'.return (\ o -> o{bluetooth_stateInquiry = v}))
        parse'wifi_stateFrequency
         = P'.try
            (do
               v <- P'.getT "wifi_stateFrequency"
               Prelude'.return (\ o -> o{wifi_stateFrequency = v}))
        parse'wifi_stateFrequencyLoop
         = P'.try
            (do
               v <- P'.getT "wifi_stateFrequencyLoop"
               Prelude'.return (\ o -> o{wifi_stateFrequencyLoop = v}))
        parse'wifi_dataRaw
         = P'.try
            (do
               v <- P'.getT "wifi_dataRaw"
               Prelude'.return (\ o -> o{wifi_dataRaw = v}))
        parse'wifi_dataDevRaw
         = P'.try
            (do
               v <- P'.getT "wifi_dataDevRaw"
               Prelude'.return (\ o -> o{wifi_dataDevRaw = v}))
        parse'wifi_dataIO
         = P'.try
            (do
               v <- P'.getT "wifi_dataIO"
               Prelude'.return (\ o -> o{wifi_dataIO = v}))
        parse'info
         = P'.try
            (do
               v <- P'.getT "info"
               Prelude'.return (\ o -> o{info = v}))
        parse'stateScanning
         = P'.try
            (do
               v <- P'.getT "stateScanning"
               Prelude'.return (\ o -> o{stateScanning = v}))
        parse'stateGyrid
         = P'.try
            (do
               v <- P'.getT "stateGyrid"
               Prelude'.return (\ o -> o{stateGyrid = v}))
        parse'stateAntenna
         = P'.try
            (do
               v <- P'.getT "stateAntenna"
               Prelude'.return (\ o -> o{stateAntenna = v}))
        parse'uptime
         = P'.try
            (do
               v <- P'.getT "uptime"
               Prelude'.return (\ o -> o{uptime = v}))
        parse'requestKeepalive
         = P'.try
            (do
               v <- P'.getT "requestKeepalive"
               Prelude'.return (\ o -> o{requestKeepalive = v}))
        parse'requestUptime
         = P'.try
            (do
               v <- P'.getT "requestUptime"
               Prelude'.return (\ o -> o{requestUptime = v}))
        parse'requestCaching
         = P'.try
            (do
               v <- P'.getT "requestCaching"
               Prelude'.return (\ o -> o{requestCaching = v}))
        parse'requestStartdata
         = P'.try
            (do
               v <- P'.getT "requestStartdata"
               Prelude'.return (\ o -> o{requestStartdata = v}))
        parse'requestState
         = P'.try
            (do
               v <- P'.getT "requestState"
               Prelude'.return (\ o -> o{requestState = v}))
        parse'hostname
         = P'.try
            (do
               v <- P'.getT "hostname"
               Prelude'.return (\ o -> o{hostname = v}))
        parse'antennaTurn
         = P'.try
            (do
               v <- P'.getT "antennaTurn"
               Prelude'.return (\ o -> o{antennaTurn = v}))
        parse'scanPattern
         = P'.try
            (do
               v <- P'.getT "scanPattern"
               Prelude'.return (\ o -> o{scanPattern = v}))
        parse'success
         = P'.try
            (do
               v <- P'.getT "success"
               Prelude'.return (\ o -> o{success = v}))