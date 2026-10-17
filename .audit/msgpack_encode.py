#!/usr/bin/env python3
"""Encode one flat JSON object as MessagePack, for `nats pub --force-stdin`.

The services read their request/reply packets with the process-wide wire codec,
which this deployment sets to MessagePack (ORES_NATS_WIRE_FORMAT=msgpack), so a
payload built by hand has to be packed rather than written as JSON.

    python3 .audit/msgpack_encode.py '{"a":"b"}' > out.msgpack

Only the shapes these request records use are supported: a map of string keys to
string values.
"""
import json
import sys


def pack_str(s: str) -> bytes:
    b = s.encode()
    n = len(b)
    if n < 32:
        return bytes([0xA0 | n]) + b
    if n < 256:
        return bytes([0xD9, n]) + b
    if n < 65536:
        return bytes([0xDA]) + n.to_bytes(2, "big") + b
    return bytes([0xDB]) + n.to_bytes(4, "big") + b


def pack_map(d: dict) -> bytes:
    n = len(d)
    if n > 15:
        raise SystemExit("map too large for fixmap")
    out = bytes([0x80 | n])
    for k, v in d.items():
        out += pack_str(k) + pack_str(v)
    return out


def main() -> int:
    obj = json.loads(sys.argv[1])
    sys.stdout.buffer.write(pack_map(obj))
    return 0


if __name__ == "__main__":
    sys.exit(main())
