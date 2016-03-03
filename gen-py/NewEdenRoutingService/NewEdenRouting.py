#
# Autogenerated by Thrift Compiler (1.0.0-dev)
#
# DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING
#
#  options string: py
#

from thrift.Thrift import TType, TMessageType, TFrozenDict, TException, TApplicationException
import sys
import logging
from .ttypes import *
from thrift.Thrift import TProcessor
from thrift.transport import TTransport


class Iface(object):
    def route(self, fromSolarSystemId, toSolarSystemId, connections, opts):
        """
        <p>Find the shortest routes between two solar system. Additional
        connections between solar systems can be placed. Available
        options are <i>OPTION_PREFER_SHORTEST</i>, <i>OPTION_PREFER_SAFER</i> or
        <i>OPTION_PREFER_HIGHSEC</i>.</p>

        <p>Additional connections can contain a 'weight'. Every connection has a weight
        of 1 by default. Lower values will result in the connection being preferred.
        Higher numbers will make it less preferred. E.g. weight 2 means if your shortet
        standard route is 3 jumps we are picking the additional provided connection,
        if it's 2 we pick the original connection.</p>

        Parameters:
         - fromSolarSystemId: The system id to start from
         - toSolarSystemId: The system id to go to
         - connections: A list of additional connections, not found in the static dump (e.g. wormholes).
         - opts: Options on how to choose routes
        """
        pass

    def jumps(self, fromSolarSystemId, toSolarSystemId, rangeInLightyears, opts):
        """
        Find the shorteest jump routes.

        Parameters:
         - fromSolarSystemId: The system id to start from
         - toSolarSystemId: The system id to go to
         - rangeInLightyears: Jump range in lightyears (e.g. 5.0 for a Nyx). Max value is 10 LY
         - opts: Options on how to choose routes
        """
        pass

    def range(self, fromSolarSystemId, rangeInLightyears):
        """
        Find all systems in range.

        Parameters:
         - fromSolarSystemId: The system id to start from
         - rangeInLightyears: Jump range in lightyears (e.g. 5.0 for a Nyx). Max value is 10 LY
        """
        pass


class Client(Iface):
    def __init__(self, iprot, oprot=None):
        self._iprot = self._oprot = iprot
        if oprot is not None:
            self._oprot = oprot
        self._seqid = 0

    def route(self, fromSolarSystemId, toSolarSystemId, connections, opts):
        """
        <p>Find the shortest routes between two solar system. Additional
        connections between solar systems can be placed. Available
        options are <i>OPTION_PREFER_SHORTEST</i>, <i>OPTION_PREFER_SAFER</i> or
        <i>OPTION_PREFER_HIGHSEC</i>.</p>

        <p>Additional connections can contain a 'weight'. Every connection has a weight
        of 1 by default. Lower values will result in the connection being preferred.
        Higher numbers will make it less preferred. E.g. weight 2 means if your shortet
        standard route is 3 jumps we are picking the additional provided connection,
        if it's 2 we pick the original connection.</p>

        Parameters:
         - fromSolarSystemId: The system id to start from
         - toSolarSystemId: The system id to go to
         - connections: A list of additional connections, not found in the static dump (e.g. wormholes).
         - opts: Options on how to choose routes
        """
        self.send_route(fromSolarSystemId, toSolarSystemId, connections, opts)
        return self.recv_route()

    def send_route(self, fromSolarSystemId, toSolarSystemId, connections, opts):
        self._oprot.writeMessageBegin('route', TMessageType.CALL, self._seqid)
        args = route_args()
        args.fromSolarSystemId = fromSolarSystemId
        args.toSolarSystemId = toSolarSystemId
        args.connections = connections
        args.opts = opts
        args.write(self._oprot)
        self._oprot.writeMessageEnd()
        self._oprot.trans.flush()

    def recv_route(self):
        iprot = self._iprot
        (fname, mtype, rseqid) = iprot.readMessageBegin()
        if mtype == TMessageType.EXCEPTION:
            x = TApplicationException()
            x.read(iprot)
            iprot.readMessageEnd()
            raise x
        result = route_result()
        result.read(iprot)
        iprot.readMessageEnd()
        if result.success is not None:
            return result.success
        if result.le is not None:
            raise result.le
        raise TApplicationException(TApplicationException.MISSING_RESULT, "route failed: unknown result")

    def jumps(self, fromSolarSystemId, toSolarSystemId, rangeInLightyears, opts):
        """
        Find the shorteest jump routes.

        Parameters:
         - fromSolarSystemId: The system id to start from
         - toSolarSystemId: The system id to go to
         - rangeInLightyears: Jump range in lightyears (e.g. 5.0 for a Nyx). Max value is 10 LY
         - opts: Options on how to choose routes
        """
        self.send_jumps(fromSolarSystemId, toSolarSystemId, rangeInLightyears, opts)
        return self.recv_jumps()

    def send_jumps(self, fromSolarSystemId, toSolarSystemId, rangeInLightyears, opts):
        self._oprot.writeMessageBegin('jumps', TMessageType.CALL, self._seqid)
        args = jumps_args()
        args.fromSolarSystemId = fromSolarSystemId
        args.toSolarSystemId = toSolarSystemId
        args.rangeInLightyears = rangeInLightyears
        args.opts = opts
        args.write(self._oprot)
        self._oprot.writeMessageEnd()
        self._oprot.trans.flush()

    def recv_jumps(self):
        iprot = self._iprot
        (fname, mtype, rseqid) = iprot.readMessageBegin()
        if mtype == TMessageType.EXCEPTION:
            x = TApplicationException()
            x.read(iprot)
            iprot.readMessageEnd()
            raise x
        result = jumps_result()
        result.read(iprot)
        iprot.readMessageEnd()
        if result.success is not None:
            return result.success
        if result.le is not None:
            raise result.le
        if result.ia is not None:
            raise result.ia
        raise TApplicationException(TApplicationException.MISSING_RESULT, "jumps failed: unknown result")

    def range(self, fromSolarSystemId, rangeInLightyears):
        """
        Find all systems in range.

        Parameters:
         - fromSolarSystemId: The system id to start from
         - rangeInLightyears: Jump range in lightyears (e.g. 5.0 for a Nyx). Max value is 10 LY
        """
        self.send_range(fromSolarSystemId, rangeInLightyears)
        return self.recv_range()

    def send_range(self, fromSolarSystemId, rangeInLightyears):
        self._oprot.writeMessageBegin('range', TMessageType.CALL, self._seqid)
        args = range_args()
        args.fromSolarSystemId = fromSolarSystemId
        args.rangeInLightyears = rangeInLightyears
        args.write(self._oprot)
        self._oprot.writeMessageEnd()
        self._oprot.trans.flush()

    def recv_range(self):
        iprot = self._iprot
        (fname, mtype, rseqid) = iprot.readMessageBegin()
        if mtype == TMessageType.EXCEPTION:
            x = TApplicationException()
            x.read(iprot)
            iprot.readMessageEnd()
            raise x
        result = range_result()
        result.read(iprot)
        iprot.readMessageEnd()
        if result.success is not None:
            return result.success
        if result.le is not None:
            raise result.le
        if result.ia is not None:
            raise result.ia
        raise TApplicationException(TApplicationException.MISSING_RESULT, "range failed: unknown result")


class Processor(Iface, TProcessor):
    def __init__(self, handler):
        self._handler = handler
        self._processMap = {}
        self._processMap["route"] = Processor.process_route
        self._processMap["jumps"] = Processor.process_jumps
        self._processMap["range"] = Processor.process_range

    def process(self, iprot, oprot):
        (name, type, seqid) = iprot.readMessageBegin()
        if name not in self._processMap:
            iprot.skip(TType.STRUCT)
            iprot.readMessageEnd()
            x = TApplicationException(TApplicationException.UNKNOWN_METHOD, 'Unknown function %s' % (name))
            oprot.writeMessageBegin(name, TMessageType.EXCEPTION, seqid)
            x.write(oprot)
            oprot.writeMessageEnd()
            oprot.trans.flush()
            return
        else:
            self._processMap[name](self, seqid, iprot, oprot)
        return True

    def process_route(self, seqid, iprot, oprot):
        args = route_args()
        args.read(iprot)
        iprot.readMessageEnd()
        result = route_result()
        try:
            result.success = self._handler.route(args.fromSolarSystemId, args.toSolarSystemId, args.connections, args.opts)
            msg_type = TMessageType.REPLY
        except (TTransport.TTransportException, KeyboardInterrupt, SystemExit):
            raise
        except NewEden.ttypes.LogicalError as le:
            msg_type = TMessageType.REPLY
            result.le = le
        except Exception as ex:
            msg_type = TMessageType.EXCEPTION
            logging.exception(ex)
            result = TApplicationException(TApplicationException.INTERNAL_ERROR, 'Internal error')
        oprot.writeMessageBegin("route", msg_type, seqid)
        result.write(oprot)
        oprot.writeMessageEnd()
        oprot.trans.flush()

    def process_jumps(self, seqid, iprot, oprot):
        args = jumps_args()
        args.read(iprot)
        iprot.readMessageEnd()
        result = jumps_result()
        try:
            result.success = self._handler.jumps(args.fromSolarSystemId, args.toSolarSystemId, args.rangeInLightyears, args.opts)
            msg_type = TMessageType.REPLY
        except (TTransport.TTransportException, KeyboardInterrupt, SystemExit):
            raise
        except NewEden.ttypes.LogicalError as le:
            msg_type = TMessageType.REPLY
            result.le = le
        except NewEden.ttypes.InvalidArgument as ia:
            msg_type = TMessageType.REPLY
            result.ia = ia
        except Exception as ex:
            msg_type = TMessageType.EXCEPTION
            logging.exception(ex)
            result = TApplicationException(TApplicationException.INTERNAL_ERROR, 'Internal error')
        oprot.writeMessageBegin("jumps", msg_type, seqid)
        result.write(oprot)
        oprot.writeMessageEnd()
        oprot.trans.flush()

    def process_range(self, seqid, iprot, oprot):
        args = range_args()
        args.read(iprot)
        iprot.readMessageEnd()
        result = range_result()
        try:
            result.success = self._handler.range(args.fromSolarSystemId, args.rangeInLightyears)
            msg_type = TMessageType.REPLY
        except (TTransport.TTransportException, KeyboardInterrupt, SystemExit):
            raise
        except NewEden.ttypes.LogicalError as le:
            msg_type = TMessageType.REPLY
            result.le = le
        except NewEden.ttypes.InvalidArgument as ia:
            msg_type = TMessageType.REPLY
            result.ia = ia
        except Exception as ex:
            msg_type = TMessageType.EXCEPTION
            logging.exception(ex)
            result = TApplicationException(TApplicationException.INTERNAL_ERROR, 'Internal error')
        oprot.writeMessageBegin("range", msg_type, seqid)
        result.write(oprot)
        oprot.writeMessageEnd()
        oprot.trans.flush()

# HELPER FUNCTIONS AND STRUCTURES


class route_args(object):
    """
    Attributes:
     - fromSolarSystemId: The system id to start from
     - toSolarSystemId: The system id to go to
     - connections: A list of additional connections, not found in the static dump (e.g. wormholes).
     - opts: Options on how to choose routes
    """

    thrift_spec = (
        None,  # 0
        (1, TType.I32, 'fromSolarSystemId', None, None, ),  # 1
        (2, TType.I32, 'toSolarSystemId', None, None, ),  # 2
        (3, TType.LIST, 'connections', (TType.STRUCT, (NewEden.ttypes.Connection, NewEden.ttypes.Connection.thrift_spec), False), None, ),  # 3
        (4, TType.BYTE, 'opts', None, 1, ),  # 4
    )

    def __init__(self, fromSolarSystemId=None, toSolarSystemId=None, connections=None, opts=thrift_spec[4][4],):
        self.fromSolarSystemId = fromSolarSystemId
        self.toSolarSystemId = toSolarSystemId
        self.connections = connections
        self.opts = opts

    def read(self, iprot):
        if iprot._fast_decode is not None and isinstance(iprot.trans, TTransport.CReadableTransport) and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, (self.__class__, self.thrift_spec))
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.I32:
                    self.fromSolarSystemId = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.I32:
                    self.toSolarSystemId = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 3:
                if ftype == TType.LIST:
                    self.connections = []
                    (_etype3, _size0) = iprot.readListBegin()
                    for _i4 in range(_size0):
                        _elem5 = NewEden.ttypes.Connection()
                        _elem5.read(iprot)
                        self.connections.append(_elem5)
                    iprot.readListEnd()
                else:
                    iprot.skip(ftype)
            elif fid == 4:
                if ftype == TType.BYTE:
                    self.opts = iprot.readByte()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, (self.__class__, self.thrift_spec)))
            return
        oprot.writeStructBegin('route_args')
        if self.fromSolarSystemId is not None:
            oprot.writeFieldBegin('fromSolarSystemId', TType.I32, 1)
            oprot.writeI32(self.fromSolarSystemId)
            oprot.writeFieldEnd()
        if self.toSolarSystemId is not None:
            oprot.writeFieldBegin('toSolarSystemId', TType.I32, 2)
            oprot.writeI32(self.toSolarSystemId)
            oprot.writeFieldEnd()
        if self.connections is not None:
            oprot.writeFieldBegin('connections', TType.LIST, 3)
            oprot.writeListBegin(TType.STRUCT, len(self.connections))
            for iter6 in self.connections:
                iter6.write(oprot)
            oprot.writeListEnd()
            oprot.writeFieldEnd()
        if self.opts is not None:
            oprot.writeFieldBegin('opts', TType.BYTE, 4)
            oprot.writeByte(self.opts)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

    def validate(self):
        return

    def __repr__(self):
        L = ['%s=%r' % (key, value)
             for key, value in self.__dict__.items()]
        return '%s(%s)' % (self.__class__.__name__, ', '.join(L))

    def __eq__(self, other):
        return isinstance(other, self.__class__) and self.__dict__ == other.__dict__

    def __ne__(self, other):
        return not (self == other)


class route_result(object):
    """
    Attributes:
     - success
     - le
    """

    thrift_spec = (
        (0, TType.LIST, 'success', (TType.I32, None, False), None, ),  # 0
        (1, TType.STRUCT, 'le', (NewEden.ttypes.LogicalError, NewEden.ttypes.LogicalError.thrift_spec), None, ),  # 1
    )

    def __init__(self, success=None, le=None,):
        self.success = success
        self.le = le

    def read(self, iprot):
        if iprot._fast_decode is not None and isinstance(iprot.trans, TTransport.CReadableTransport) and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, (self.__class__, self.thrift_spec))
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 0:
                if ftype == TType.LIST:
                    self.success = []
                    (_etype10, _size7) = iprot.readListBegin()
                    for _i11 in range(_size7):
                        _elem12 = iprot.readI32()
                        self.success.append(_elem12)
                    iprot.readListEnd()
                else:
                    iprot.skip(ftype)
            elif fid == 1:
                if ftype == TType.STRUCT:
                    self.le = NewEden.ttypes.LogicalError()
                    self.le.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, (self.__class__, self.thrift_spec)))
            return
        oprot.writeStructBegin('route_result')
        if self.success is not None:
            oprot.writeFieldBegin('success', TType.LIST, 0)
            oprot.writeListBegin(TType.I32, len(self.success))
            for iter13 in self.success:
                oprot.writeI32(iter13)
            oprot.writeListEnd()
            oprot.writeFieldEnd()
        if self.le is not None:
            oprot.writeFieldBegin('le', TType.STRUCT, 1)
            self.le.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

    def validate(self):
        return

    def __repr__(self):
        L = ['%s=%r' % (key, value)
             for key, value in self.__dict__.items()]
        return '%s(%s)' % (self.__class__.__name__, ', '.join(L))

    def __eq__(self, other):
        return isinstance(other, self.__class__) and self.__dict__ == other.__dict__

    def __ne__(self, other):
        return not (self == other)


class jumps_args(object):
    """
    Attributes:
     - fromSolarSystemId: The system id to start from
     - toSolarSystemId: The system id to go to
     - rangeInLightyears: Jump range in lightyears (e.g. 5.0 for a Nyx). Max value is 10 LY
     - opts: Options on how to choose routes
    """

    thrift_spec = (
        None,  # 0
        (1, TType.I32, 'fromSolarSystemId', None, None, ),  # 1
        (2, TType.I32, 'toSolarSystemId', None, None, ),  # 2
        None,  # 3
        (4, TType.DOUBLE, 'rangeInLightyears', None, None, ),  # 4
        (5, TType.BYTE, 'opts', None, 1, ),  # 5
    )

    def __init__(self, fromSolarSystemId=None, toSolarSystemId=None, rangeInLightyears=None, opts=thrift_spec[5][4],):
        self.fromSolarSystemId = fromSolarSystemId
        self.toSolarSystemId = toSolarSystemId
        self.rangeInLightyears = rangeInLightyears
        self.opts = opts

    def read(self, iprot):
        if iprot._fast_decode is not None and isinstance(iprot.trans, TTransport.CReadableTransport) and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, (self.__class__, self.thrift_spec))
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.I32:
                    self.fromSolarSystemId = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.I32:
                    self.toSolarSystemId = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 4:
                if ftype == TType.DOUBLE:
                    self.rangeInLightyears = iprot.readDouble()
                else:
                    iprot.skip(ftype)
            elif fid == 5:
                if ftype == TType.BYTE:
                    self.opts = iprot.readByte()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, (self.__class__, self.thrift_spec)))
            return
        oprot.writeStructBegin('jumps_args')
        if self.fromSolarSystemId is not None:
            oprot.writeFieldBegin('fromSolarSystemId', TType.I32, 1)
            oprot.writeI32(self.fromSolarSystemId)
            oprot.writeFieldEnd()
        if self.toSolarSystemId is not None:
            oprot.writeFieldBegin('toSolarSystemId', TType.I32, 2)
            oprot.writeI32(self.toSolarSystemId)
            oprot.writeFieldEnd()
        if self.rangeInLightyears is not None:
            oprot.writeFieldBegin('rangeInLightyears', TType.DOUBLE, 4)
            oprot.writeDouble(self.rangeInLightyears)
            oprot.writeFieldEnd()
        if self.opts is not None:
            oprot.writeFieldBegin('opts', TType.BYTE, 5)
            oprot.writeByte(self.opts)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

    def validate(self):
        return

    def __repr__(self):
        L = ['%s=%r' % (key, value)
             for key, value in self.__dict__.items()]
        return '%s(%s)' % (self.__class__.__name__, ', '.join(L))

    def __eq__(self, other):
        return isinstance(other, self.__class__) and self.__dict__ == other.__dict__

    def __ne__(self, other):
        return not (self == other)


class jumps_result(object):
    """
    Attributes:
     - success
     - le
     - ia
    """

    thrift_spec = (
        (0, TType.LIST, 'success', (TType.I32, None, False), None, ),  # 0
        (1, TType.STRUCT, 'le', (NewEden.ttypes.LogicalError, NewEden.ttypes.LogicalError.thrift_spec), None, ),  # 1
        (2, TType.STRUCT, 'ia', (NewEden.ttypes.InvalidArgument, NewEden.ttypes.InvalidArgument.thrift_spec), None, ),  # 2
    )

    def __init__(self, success=None, le=None, ia=None,):
        self.success = success
        self.le = le
        self.ia = ia

    def read(self, iprot):
        if iprot._fast_decode is not None and isinstance(iprot.trans, TTransport.CReadableTransport) and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, (self.__class__, self.thrift_spec))
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 0:
                if ftype == TType.LIST:
                    self.success = []
                    (_etype17, _size14) = iprot.readListBegin()
                    for _i18 in range(_size14):
                        _elem19 = iprot.readI32()
                        self.success.append(_elem19)
                    iprot.readListEnd()
                else:
                    iprot.skip(ftype)
            elif fid == 1:
                if ftype == TType.STRUCT:
                    self.le = NewEden.ttypes.LogicalError()
                    self.le.read(iprot)
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.STRUCT:
                    self.ia = NewEden.ttypes.InvalidArgument()
                    self.ia.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, (self.__class__, self.thrift_spec)))
            return
        oprot.writeStructBegin('jumps_result')
        if self.success is not None:
            oprot.writeFieldBegin('success', TType.LIST, 0)
            oprot.writeListBegin(TType.I32, len(self.success))
            for iter20 in self.success:
                oprot.writeI32(iter20)
            oprot.writeListEnd()
            oprot.writeFieldEnd()
        if self.le is not None:
            oprot.writeFieldBegin('le', TType.STRUCT, 1)
            self.le.write(oprot)
            oprot.writeFieldEnd()
        if self.ia is not None:
            oprot.writeFieldBegin('ia', TType.STRUCT, 2)
            self.ia.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

    def validate(self):
        return

    def __repr__(self):
        L = ['%s=%r' % (key, value)
             for key, value in self.__dict__.items()]
        return '%s(%s)' % (self.__class__.__name__, ', '.join(L))

    def __eq__(self, other):
        return isinstance(other, self.__class__) and self.__dict__ == other.__dict__

    def __ne__(self, other):
        return not (self == other)


class range_args(object):
    """
    Attributes:
     - fromSolarSystemId: The system id to start from
     - rangeInLightyears: Jump range in lightyears (e.g. 5.0 for a Nyx). Max value is 10 LY
    """

    thrift_spec = (
        None,  # 0
        (1, TType.I32, 'fromSolarSystemId', None, None, ),  # 1
        None,  # 2
        None,  # 3
        (4, TType.DOUBLE, 'rangeInLightyears', None, None, ),  # 4
    )

    def __init__(self, fromSolarSystemId=None, rangeInLightyears=None,):
        self.fromSolarSystemId = fromSolarSystemId
        self.rangeInLightyears = rangeInLightyears

    def read(self, iprot):
        if iprot._fast_decode is not None and isinstance(iprot.trans, TTransport.CReadableTransport) and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, (self.__class__, self.thrift_spec))
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 1:
                if ftype == TType.I32:
                    self.fromSolarSystemId = iprot.readI32()
                else:
                    iprot.skip(ftype)
            elif fid == 4:
                if ftype == TType.DOUBLE:
                    self.rangeInLightyears = iprot.readDouble()
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, (self.__class__, self.thrift_spec)))
            return
        oprot.writeStructBegin('range_args')
        if self.fromSolarSystemId is not None:
            oprot.writeFieldBegin('fromSolarSystemId', TType.I32, 1)
            oprot.writeI32(self.fromSolarSystemId)
            oprot.writeFieldEnd()
        if self.rangeInLightyears is not None:
            oprot.writeFieldBegin('rangeInLightyears', TType.DOUBLE, 4)
            oprot.writeDouble(self.rangeInLightyears)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

    def validate(self):
        return

    def __repr__(self):
        L = ['%s=%r' % (key, value)
             for key, value in self.__dict__.items()]
        return '%s(%s)' % (self.__class__.__name__, ', '.join(L))

    def __eq__(self, other):
        return isinstance(other, self.__class__) and self.__dict__ == other.__dict__

    def __ne__(self, other):
        return not (self == other)


class range_result(object):
    """
    Attributes:
     - success
     - le
     - ia
    """

    thrift_spec = (
        (0, TType.LIST, 'success', (TType.I32, None, False), None, ),  # 0
        (1, TType.STRUCT, 'le', (NewEden.ttypes.LogicalError, NewEden.ttypes.LogicalError.thrift_spec), None, ),  # 1
        (2, TType.STRUCT, 'ia', (NewEden.ttypes.InvalidArgument, NewEden.ttypes.InvalidArgument.thrift_spec), None, ),  # 2
    )

    def __init__(self, success=None, le=None, ia=None,):
        self.success = success
        self.le = le
        self.ia = ia

    def read(self, iprot):
        if iprot._fast_decode is not None and isinstance(iprot.trans, TTransport.CReadableTransport) and self.thrift_spec is not None:
            iprot._fast_decode(self, iprot, (self.__class__, self.thrift_spec))
            return
        iprot.readStructBegin()
        while True:
            (fname, ftype, fid) = iprot.readFieldBegin()
            if ftype == TType.STOP:
                break
            if fid == 0:
                if ftype == TType.LIST:
                    self.success = []
                    (_etype24, _size21) = iprot.readListBegin()
                    for _i25 in range(_size21):
                        _elem26 = iprot.readI32()
                        self.success.append(_elem26)
                    iprot.readListEnd()
                else:
                    iprot.skip(ftype)
            elif fid == 1:
                if ftype == TType.STRUCT:
                    self.le = NewEden.ttypes.LogicalError()
                    self.le.read(iprot)
                else:
                    iprot.skip(ftype)
            elif fid == 2:
                if ftype == TType.STRUCT:
                    self.ia = NewEden.ttypes.InvalidArgument()
                    self.ia.read(iprot)
                else:
                    iprot.skip(ftype)
            else:
                iprot.skip(ftype)
            iprot.readFieldEnd()
        iprot.readStructEnd()

    def write(self, oprot):
        if oprot._fast_encode is not None and self.thrift_spec is not None:
            oprot.trans.write(oprot._fast_encode(self, (self.__class__, self.thrift_spec)))
            return
        oprot.writeStructBegin('range_result')
        if self.success is not None:
            oprot.writeFieldBegin('success', TType.LIST, 0)
            oprot.writeListBegin(TType.I32, len(self.success))
            for iter27 in self.success:
                oprot.writeI32(iter27)
            oprot.writeListEnd()
            oprot.writeFieldEnd()
        if self.le is not None:
            oprot.writeFieldBegin('le', TType.STRUCT, 1)
            self.le.write(oprot)
            oprot.writeFieldEnd()
        if self.ia is not None:
            oprot.writeFieldBegin('ia', TType.STRUCT, 2)
            self.ia.write(oprot)
            oprot.writeFieldEnd()
        oprot.writeFieldStop()
        oprot.writeStructEnd()

    def validate(self):
        return

    def __repr__(self):
        L = ['%s=%r' % (key, value)
             for key, value in self.__dict__.items()]
        return '%s(%s)' % (self.__class__.__name__, ', '.join(L))

    def __eq__(self, other):
        return isinstance(other, self.__class__) and self.__dict__ == other.__dict__

    def __ne__(self, other):
        return not (self == other)
