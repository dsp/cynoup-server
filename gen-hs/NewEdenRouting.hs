{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-----------------------------------------------------------------
-- Autogenerated by Thrift Compiler (1.0.0-dev)                      --
--                                                             --
-- DO NOT EDIT UNLESS YOU ARE SURE YOU KNOW WHAT YOU ARE DOING --
-----------------------------------------------------------------

module NewEdenRouting where
import Prelude (($), (.), (>>=), (==), (++))
import qualified Prelude as P
import qualified Control.Exception as X
import qualified Control.Monad as M ( liftM, ap, when )
import Data.Functor ( (<$>) )
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Hashable as H
import qualified Data.Int as I
import qualified Data.Maybe as M (catMaybes)
import qualified Data.Text.Lazy.Encoding as E ( decodeUtf8, encodeUtf8 )
import qualified Data.Text.Lazy as LT
import qualified GHC.Generics as G (Generic)
import qualified Data.Typeable as TY ( Typeable )
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import qualified Data.Vector as Vector
import qualified Test.QuickCheck.Arbitrary as QC ( Arbitrary(..) )
import qualified Test.QuickCheck as QC ( elements )

import qualified Thrift as T
import qualified Thrift.Types as T
import qualified Thrift.Arbitraries as T

import qualified NewEden_Types


import NewEdenRoutingService_Types
import qualified NewEdenRouting_Iface as Iface
-- HELPER FUNCTIONS AND STRUCTURES --

data Route_args = Route_args  { route_args_fromSolarSystemId :: I.Int32
  , route_args_toSolarSystemId :: I.Int32
  , route_args_connections :: (Vector.Vector NewEden_Types.Connection)
  , route_args_opts :: I.Int8
  } deriving (P.Show,P.Eq,G.Generic,TY.Typeable)
instance H.Hashable Route_args where
  hashWithSalt salt record = salt   `H.hashWithSalt` route_args_fromSolarSystemId record   `H.hashWithSalt` route_args_toSolarSystemId record   `H.hashWithSalt` route_args_connections record   `H.hashWithSalt` route_args_opts record  
instance QC.Arbitrary Route_args where 
  arbitrary = M.liftM Route_args (QC.arbitrary)
          `M.ap`(QC.arbitrary)
          `M.ap`(QC.arbitrary)
          `M.ap`(QC.arbitrary)
  shrink obj | obj == default_Route_args = []
             | P.otherwise = M.catMaybes
    [ if obj == default_Route_args{route_args_fromSolarSystemId = route_args_fromSolarSystemId obj} then P.Nothing else P.Just $ default_Route_args{route_args_fromSolarSystemId = route_args_fromSolarSystemId obj}
    , if obj == default_Route_args{route_args_toSolarSystemId = route_args_toSolarSystemId obj} then P.Nothing else P.Just $ default_Route_args{route_args_toSolarSystemId = route_args_toSolarSystemId obj}
    , if obj == default_Route_args{route_args_connections = route_args_connections obj} then P.Nothing else P.Just $ default_Route_args{route_args_connections = route_args_connections obj}
    , if obj == default_Route_args{route_args_opts = route_args_opts obj} then P.Nothing else P.Just $ default_Route_args{route_args_opts = route_args_opts obj}
    ]
from_Route_args :: Route_args -> T.ThriftVal
from_Route_args record = T.TStruct $ Map.fromList $ M.catMaybes
  [ (\_v2 -> P.Just (1, ("fromSolarSystemId",T.TI32 _v2))) $ route_args_fromSolarSystemId record
  , (\_v2 -> P.Just (2, ("toSolarSystemId",T.TI32 _v2))) $ route_args_toSolarSystemId record
  , (\_v2 -> P.Just (3, ("connections",T.TList (T.T_STRUCT NewEden_Types.typemap_Connection) $ P.map (\_v4 -> NewEden_Types.from_Connection _v4) $ Vector.toList _v2))) $ route_args_connections record
  , (\_v2 -> P.Just (4, ("opts",T.TByte _v2))) $ route_args_opts record
  ]
write_Route_args :: (T.Protocol p, T.Transport t) => p t -> Route_args -> P.IO ()
write_Route_args oprot record = T.writeVal oprot $ from_Route_args record
encode_Route_args :: (T.Protocol p, T.Transport t) => p t -> Route_args -> LBS.ByteString
encode_Route_args oprot record = T.serializeVal oprot $ from_Route_args record
to_Route_args :: T.ThriftVal -> Route_args
to_Route_args (T.TStruct fields) = Route_args{
  route_args_fromSolarSystemId = P.maybe (route_args_fromSolarSystemId default_Route_args) (\(_,_val6) -> (case _val6 of {T.TI32 _val7 -> _val7; _ -> P.error "wrong type"})) (Map.lookup (1) fields),
  route_args_toSolarSystemId = P.maybe (route_args_toSolarSystemId default_Route_args) (\(_,_val6) -> (case _val6 of {T.TI32 _val8 -> _val8; _ -> P.error "wrong type"})) (Map.lookup (2) fields),
  route_args_connections = P.maybe (route_args_connections default_Route_args) (\(_,_val6) -> (case _val6 of {T.TList _ _val9 -> (Vector.fromList $ P.map (\_v10 -> (case _v10 of {T.TStruct _val11 -> (NewEden_Types.to_Connection (T.TStruct _val11)); _ -> P.error "wrong type"})) _val9); _ -> P.error "wrong type"})) (Map.lookup (3) fields),
  route_args_opts = P.maybe (route_args_opts default_Route_args) (\(_,_val6) -> (case _val6 of {T.TByte _val12 -> _val12; _ -> P.error "wrong type"})) (Map.lookup (4) fields)
  }
to_Route_args _ = P.error "not a struct"
read_Route_args :: (T.Transport t, T.Protocol p) => p t -> P.IO Route_args
read_Route_args iprot = to_Route_args <$> T.readVal iprot (T.T_STRUCT typemap_Route_args)
decode_Route_args :: (T.Protocol p, T.Transport t) => p t -> LBS.ByteString -> Route_args
decode_Route_args iprot bs = to_Route_args $ T.deserializeVal iprot (T.T_STRUCT typemap_Route_args) bs
typemap_Route_args :: T.TypeMap
typemap_Route_args = Map.fromList [(1,("fromSolarSystemId",T.T_I32)),(2,("toSolarSystemId",T.T_I32)),(3,("connections",(T.T_LIST (T.T_STRUCT NewEden_Types.typemap_Connection)))),(4,("opts",T.T_BYTE))]
default_Route_args :: Route_args
default_Route_args = Route_args{
  route_args_fromSolarSystemId = 0,
  route_args_toSolarSystemId = 0,
  route_args_connections = Vector.empty,
  route_args_opts = (1)}
data Route_result = Route_result  { route_result_success :: (Vector.Vector I.Int32)
  , route_result_le :: P.Maybe NewEden_Types.LogicalError
  } deriving (P.Show,P.Eq,G.Generic,TY.Typeable)
instance H.Hashable Route_result where
  hashWithSalt salt record = salt   `H.hashWithSalt` route_result_success record   `H.hashWithSalt` route_result_le record  
instance QC.Arbitrary Route_result where 
  arbitrary = M.liftM Route_result (QC.arbitrary)
          `M.ap`(M.liftM P.Just QC.arbitrary)
  shrink obj | obj == default_Route_result = []
             | P.otherwise = M.catMaybes
    [ if obj == default_Route_result{route_result_success = route_result_success obj} then P.Nothing else P.Just $ default_Route_result{route_result_success = route_result_success obj}
    , if obj == default_Route_result{route_result_le = route_result_le obj} then P.Nothing else P.Just $ default_Route_result{route_result_le = route_result_le obj}
    ]
from_Route_result :: Route_result -> T.ThriftVal
from_Route_result record = T.TStruct $ Map.fromList 
  (let exns = M.catMaybes [ (\_v15 -> (1, ("le",NewEden_Types.from_LogicalError _v15))) <$> route_result_le record]
  in if P.not (P.null exns) then exns else M.catMaybes
    [ (\_v15 -> P.Just (0, ("success",T.TList T.T_I32 $ P.map (\_v17 -> T.TI32 _v17) $ Vector.toList _v15))) $ route_result_success record
    , (\_v15 -> (1, ("le",NewEden_Types.from_LogicalError _v15))) <$> route_result_le record
    ]
    )
write_Route_result :: (T.Protocol p, T.Transport t) => p t -> Route_result -> P.IO ()
write_Route_result oprot record = T.writeVal oprot $ from_Route_result record
encode_Route_result :: (T.Protocol p, T.Transport t) => p t -> Route_result -> LBS.ByteString
encode_Route_result oprot record = T.serializeVal oprot $ from_Route_result record
to_Route_result :: T.ThriftVal -> Route_result
to_Route_result (T.TStruct fields) = Route_result{
  route_result_success = P.maybe (route_result_success default_Route_result) (\(_,_val19) -> (case _val19 of {T.TList _ _val20 -> (Vector.fromList $ P.map (\_v21 -> (case _v21 of {T.TI32 _val22 -> _val22; _ -> P.error "wrong type"})) _val20); _ -> P.error "wrong type"})) (Map.lookup (0) fields),
  route_result_le = P.maybe (P.Nothing) (\(_,_val19) -> P.Just (case _val19 of {T.TStruct _val23 -> (NewEden_Types.to_LogicalError (T.TStruct _val23)); _ -> P.error "wrong type"})) (Map.lookup (1) fields)
  }
to_Route_result _ = P.error "not a struct"
read_Route_result :: (T.Transport t, T.Protocol p) => p t -> P.IO Route_result
read_Route_result iprot = to_Route_result <$> T.readVal iprot (T.T_STRUCT typemap_Route_result)
decode_Route_result :: (T.Protocol p, T.Transport t) => p t -> LBS.ByteString -> Route_result
decode_Route_result iprot bs = to_Route_result $ T.deserializeVal iprot (T.T_STRUCT typemap_Route_result) bs
typemap_Route_result :: T.TypeMap
typemap_Route_result = Map.fromList [(0,("success",(T.T_LIST T.T_I32))),(1,("le",(T.T_STRUCT NewEden_Types.typemap_LogicalError)))]
default_Route_result :: Route_result
default_Route_result = Route_result{
  route_result_success = Vector.empty,
  route_result_le = P.Nothing}
data Jumps_args = Jumps_args  { jumps_args_fromSolarSystemId :: I.Int32
  , jumps_args_toSolarSystemId :: I.Int32
  , jumps_args_rangeInLightyears :: P.Double
  , jumps_args_opts :: I.Int8
  } deriving (P.Show,P.Eq,G.Generic,TY.Typeable)
instance H.Hashable Jumps_args where
  hashWithSalt salt record = salt   `H.hashWithSalt` jumps_args_fromSolarSystemId record   `H.hashWithSalt` jumps_args_toSolarSystemId record   `H.hashWithSalt` jumps_args_rangeInLightyears record   `H.hashWithSalt` jumps_args_opts record  
instance QC.Arbitrary Jumps_args where 
  arbitrary = M.liftM Jumps_args (QC.arbitrary)
          `M.ap`(QC.arbitrary)
          `M.ap`(QC.arbitrary)
          `M.ap`(QC.arbitrary)
  shrink obj | obj == default_Jumps_args = []
             | P.otherwise = M.catMaybes
    [ if obj == default_Jumps_args{jumps_args_fromSolarSystemId = jumps_args_fromSolarSystemId obj} then P.Nothing else P.Just $ default_Jumps_args{jumps_args_fromSolarSystemId = jumps_args_fromSolarSystemId obj}
    , if obj == default_Jumps_args{jumps_args_toSolarSystemId = jumps_args_toSolarSystemId obj} then P.Nothing else P.Just $ default_Jumps_args{jumps_args_toSolarSystemId = jumps_args_toSolarSystemId obj}
    , if obj == default_Jumps_args{jumps_args_rangeInLightyears = jumps_args_rangeInLightyears obj} then P.Nothing else P.Just $ default_Jumps_args{jumps_args_rangeInLightyears = jumps_args_rangeInLightyears obj}
    , if obj == default_Jumps_args{jumps_args_opts = jumps_args_opts obj} then P.Nothing else P.Just $ default_Jumps_args{jumps_args_opts = jumps_args_opts obj}
    ]
from_Jumps_args :: Jumps_args -> T.ThriftVal
from_Jumps_args record = T.TStruct $ Map.fromList $ M.catMaybes
  [ (\_v26 -> P.Just (1, ("fromSolarSystemId",T.TI32 _v26))) $ jumps_args_fromSolarSystemId record
  , (\_v26 -> P.Just (2, ("toSolarSystemId",T.TI32 _v26))) $ jumps_args_toSolarSystemId record
  , (\_v26 -> P.Just (4, ("rangeInLightyears",T.TDouble _v26))) $ jumps_args_rangeInLightyears record
  , (\_v26 -> P.Just (5, ("opts",T.TByte _v26))) $ jumps_args_opts record
  ]
write_Jumps_args :: (T.Protocol p, T.Transport t) => p t -> Jumps_args -> P.IO ()
write_Jumps_args oprot record = T.writeVal oprot $ from_Jumps_args record
encode_Jumps_args :: (T.Protocol p, T.Transport t) => p t -> Jumps_args -> LBS.ByteString
encode_Jumps_args oprot record = T.serializeVal oprot $ from_Jumps_args record
to_Jumps_args :: T.ThriftVal -> Jumps_args
to_Jumps_args (T.TStruct fields) = Jumps_args{
  jumps_args_fromSolarSystemId = P.maybe (jumps_args_fromSolarSystemId default_Jumps_args) (\(_,_val28) -> (case _val28 of {T.TI32 _val29 -> _val29; _ -> P.error "wrong type"})) (Map.lookup (1) fields),
  jumps_args_toSolarSystemId = P.maybe (jumps_args_toSolarSystemId default_Jumps_args) (\(_,_val28) -> (case _val28 of {T.TI32 _val30 -> _val30; _ -> P.error "wrong type"})) (Map.lookup (2) fields),
  jumps_args_rangeInLightyears = P.maybe (jumps_args_rangeInLightyears default_Jumps_args) (\(_,_val28) -> (case _val28 of {T.TDouble _val31 -> _val31; _ -> P.error "wrong type"})) (Map.lookup (4) fields),
  jumps_args_opts = P.maybe (jumps_args_opts default_Jumps_args) (\(_,_val28) -> (case _val28 of {T.TByte _val32 -> _val32; _ -> P.error "wrong type"})) (Map.lookup (5) fields)
  }
to_Jumps_args _ = P.error "not a struct"
read_Jumps_args :: (T.Transport t, T.Protocol p) => p t -> P.IO Jumps_args
read_Jumps_args iprot = to_Jumps_args <$> T.readVal iprot (T.T_STRUCT typemap_Jumps_args)
decode_Jumps_args :: (T.Protocol p, T.Transport t) => p t -> LBS.ByteString -> Jumps_args
decode_Jumps_args iprot bs = to_Jumps_args $ T.deserializeVal iprot (T.T_STRUCT typemap_Jumps_args) bs
typemap_Jumps_args :: T.TypeMap
typemap_Jumps_args = Map.fromList [(1,("fromSolarSystemId",T.T_I32)),(2,("toSolarSystemId",T.T_I32)),(4,("rangeInLightyears",T.T_DOUBLE)),(5,("opts",T.T_BYTE))]
default_Jumps_args :: Jumps_args
default_Jumps_args = Jumps_args{
  jumps_args_fromSolarSystemId = 0,
  jumps_args_toSolarSystemId = 0,
  jumps_args_rangeInLightyears = 0,
  jumps_args_opts = (1)}
data Jumps_result = Jumps_result  { jumps_result_success :: (Vector.Vector I.Int32)
  , jumps_result_le :: P.Maybe NewEden_Types.LogicalError
  } deriving (P.Show,P.Eq,G.Generic,TY.Typeable)
instance H.Hashable Jumps_result where
  hashWithSalt salt record = salt   `H.hashWithSalt` jumps_result_success record   `H.hashWithSalt` jumps_result_le record  
instance QC.Arbitrary Jumps_result where 
  arbitrary = M.liftM Jumps_result (QC.arbitrary)
          `M.ap`(M.liftM P.Just QC.arbitrary)
  shrink obj | obj == default_Jumps_result = []
             | P.otherwise = M.catMaybes
    [ if obj == default_Jumps_result{jumps_result_success = jumps_result_success obj} then P.Nothing else P.Just $ default_Jumps_result{jumps_result_success = jumps_result_success obj}
    , if obj == default_Jumps_result{jumps_result_le = jumps_result_le obj} then P.Nothing else P.Just $ default_Jumps_result{jumps_result_le = jumps_result_le obj}
    ]
from_Jumps_result :: Jumps_result -> T.ThriftVal
from_Jumps_result record = T.TStruct $ Map.fromList 
  (let exns = M.catMaybes [ (\_v35 -> (1, ("le",NewEden_Types.from_LogicalError _v35))) <$> jumps_result_le record]
  in if P.not (P.null exns) then exns else M.catMaybes
    [ (\_v35 -> P.Just (0, ("success",T.TList T.T_I32 $ P.map (\_v37 -> T.TI32 _v37) $ Vector.toList _v35))) $ jumps_result_success record
    , (\_v35 -> (1, ("le",NewEden_Types.from_LogicalError _v35))) <$> jumps_result_le record
    ]
    )
write_Jumps_result :: (T.Protocol p, T.Transport t) => p t -> Jumps_result -> P.IO ()
write_Jumps_result oprot record = T.writeVal oprot $ from_Jumps_result record
encode_Jumps_result :: (T.Protocol p, T.Transport t) => p t -> Jumps_result -> LBS.ByteString
encode_Jumps_result oprot record = T.serializeVal oprot $ from_Jumps_result record
to_Jumps_result :: T.ThriftVal -> Jumps_result
to_Jumps_result (T.TStruct fields) = Jumps_result{
  jumps_result_success = P.maybe (jumps_result_success default_Jumps_result) (\(_,_val39) -> (case _val39 of {T.TList _ _val40 -> (Vector.fromList $ P.map (\_v41 -> (case _v41 of {T.TI32 _val42 -> _val42; _ -> P.error "wrong type"})) _val40); _ -> P.error "wrong type"})) (Map.lookup (0) fields),
  jumps_result_le = P.maybe (P.Nothing) (\(_,_val39) -> P.Just (case _val39 of {T.TStruct _val43 -> (NewEden_Types.to_LogicalError (T.TStruct _val43)); _ -> P.error "wrong type"})) (Map.lookup (1) fields)
  }
to_Jumps_result _ = P.error "not a struct"
read_Jumps_result :: (T.Transport t, T.Protocol p) => p t -> P.IO Jumps_result
read_Jumps_result iprot = to_Jumps_result <$> T.readVal iprot (T.T_STRUCT typemap_Jumps_result)
decode_Jumps_result :: (T.Protocol p, T.Transport t) => p t -> LBS.ByteString -> Jumps_result
decode_Jumps_result iprot bs = to_Jumps_result $ T.deserializeVal iprot (T.T_STRUCT typemap_Jumps_result) bs
typemap_Jumps_result :: T.TypeMap
typemap_Jumps_result = Map.fromList [(0,("success",(T.T_LIST T.T_I32))),(1,("le",(T.T_STRUCT NewEden_Types.typemap_LogicalError)))]
default_Jumps_result :: Jumps_result
default_Jumps_result = Jumps_result{
  jumps_result_success = Vector.empty,
  jumps_result_le = P.Nothing}
process_route (seqid, iprot, oprot, handler) = do
  args <- read_Route_args iprot
  (X.catch
    (X.catch
      (do
        val <- Iface.route handler (route_args_fromSolarSystemId args) (route_args_toSolarSystemId args) (route_args_connections args) (route_args_opts args)
        let res = default_Route_result{route_result_success = val}
        T.writeMessageBegin oprot ("route", T.M_REPLY, seqid)
        write_Route_result oprot res
        T.writeMessageEnd oprot
        T.tFlush (T.getTransport oprot))
      (\e  -> do
        let res = default_Route_result{route_result_le = P.Just e}
        T.writeMessageBegin oprot ("route", T.M_REPLY, seqid)
        write_Route_result oprot res
        T.writeMessageEnd oprot
        T.tFlush (T.getTransport oprot)))
    ((\_ -> do
      T.writeMessageBegin oprot ("route", T.M_EXCEPTION, seqid)
      T.writeAppExn oprot (T.AppExn T.AE_UNKNOWN "")
      T.writeMessageEnd oprot
      T.tFlush (T.getTransport oprot)) :: X.SomeException -> P.IO ()))
process_jumps (seqid, iprot, oprot, handler) = do
  args <- read_Jumps_args iprot
  (X.catch
    (X.catch
      (do
        val <- Iface.jumps handler (jumps_args_fromSolarSystemId args) (jumps_args_toSolarSystemId args) (jumps_args_rangeInLightyears args) (jumps_args_opts args)
        let res = default_Jumps_result{jumps_result_success = val}
        T.writeMessageBegin oprot ("jumps", T.M_REPLY, seqid)
        write_Jumps_result oprot res
        T.writeMessageEnd oprot
        T.tFlush (T.getTransport oprot))
      (\e  -> do
        let res = default_Jumps_result{jumps_result_le = P.Just e}
        T.writeMessageBegin oprot ("jumps", T.M_REPLY, seqid)
        write_Jumps_result oprot res
        T.writeMessageEnd oprot
        T.tFlush (T.getTransport oprot)))
    ((\_ -> do
      T.writeMessageBegin oprot ("jumps", T.M_EXCEPTION, seqid)
      T.writeAppExn oprot (T.AppExn T.AE_UNKNOWN "")
      T.writeMessageEnd oprot
      T.tFlush (T.getTransport oprot)) :: X.SomeException -> P.IO ()))
proc_ handler (iprot,oprot) (name,typ,seqid) = case name of
  "route" -> process_route (seqid,iprot,oprot,handler)
  "jumps" -> process_jumps (seqid,iprot,oprot,handler)
  _ -> do
    _ <- T.readVal iprot (T.T_STRUCT Map.empty)
    T.writeMessageBegin oprot (name,T.M_EXCEPTION,seqid)
    T.writeAppExn oprot (T.AppExn T.AE_UNKNOWN_METHOD ("Unknown function " ++ LT.unpack name))
    T.writeMessageEnd oprot
    T.tFlush (T.getTransport oprot)
process handler (iprot, oprot) = do
  (name, typ, seqid) <- T.readMessageBegin iprot
  proc_ handler (iprot,oprot) (name,typ,seqid)
  T.readMessageEnd iprot
  P.return P.True
