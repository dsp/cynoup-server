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

module NewEdenRouting_Client(route,jumps) where
import qualified Data.IORef as R
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
import NewEdenRouting
seqid = R.newIORef 0
route (ip,op) arg_fromSolarSystemId arg_toSolarSystemId arg_connections arg_opts = do
  send_route op arg_fromSolarSystemId arg_toSolarSystemId arg_connections arg_opts
  recv_route ip
send_route op arg_fromSolarSystemId arg_toSolarSystemId arg_connections arg_opts = do
  seq <- seqid
  seqn <- R.readIORef seq
  T.writeMessageBegin op ("route", T.M_CALL, seqn)
  write_Route_args op (Route_args{route_args_fromSolarSystemId=arg_fromSolarSystemId,route_args_toSolarSystemId=arg_toSolarSystemId,route_args_connections=arg_connections,route_args_opts=arg_opts})
  T.writeMessageEnd op
  T.tFlush (T.getTransport op)
recv_route ip = do
  (fname, mtype, rseqid) <- T.readMessageBegin ip
  M.when (mtype == T.M_EXCEPTION) $ do { exn <- T.readAppExn ip ; T.readMessageEnd ip ; X.throw exn }
  res <- read_Route_result ip
  T.readMessageEnd ip
  P.maybe (P.return ()) X.throw (route_result_le res)
  P.return $ route_result_success res
jumps (ip,op) arg_fromSolarSystemId arg_toSolarSystemId arg_systems arg_reachInLightyears arg_opts arg_limit = do
  send_jumps op arg_fromSolarSystemId arg_toSolarSystemId arg_systems arg_reachInLightyears arg_opts arg_limit
  recv_jumps ip
send_jumps op arg_fromSolarSystemId arg_toSolarSystemId arg_systems arg_reachInLightyears arg_opts arg_limit = do
  seq <- seqid
  seqn <- R.readIORef seq
  T.writeMessageBegin op ("jumps", T.M_CALL, seqn)
  write_Jumps_args op (Jumps_args{jumps_args_fromSolarSystemId=arg_fromSolarSystemId,jumps_args_toSolarSystemId=arg_toSolarSystemId,jumps_args_systems=arg_systems,jumps_args_reachInLightyears=arg_reachInLightyears,jumps_args_opts=arg_opts,jumps_args_limit=arg_limit})
  T.writeMessageEnd op
  T.tFlush (T.getTransport op)
recv_jumps ip = do
  (fname, mtype, rseqid) <- T.readMessageBegin ip
  M.when (mtype == T.M_EXCEPTION) $ do { exn <- T.readAppExn ip ; T.readMessageEnd ip ; X.throw exn }
  res <- read_Jumps_result ip
  T.readMessageEnd ip
  P.maybe (P.return ()) X.throw (jumps_result_le res)
  P.return $ jumps_result_success res
