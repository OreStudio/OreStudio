#!/usr/bin/env python3
"""Streaming PNG reader used only to inspect the oversized component renders.

Usage:
    pngtool.py overview IN OUT [WIDTH]
    pngtool.py crop IN OUT X Y W H
"""
import struct
import sys
import zlib


def read_chunks(path):
    data = open(path, 'rb').read()
    if data[:8] != b'\x89PNG\r\n\x1a\n':
        raise SystemExit('not a png')
    pos = 8
    idat = bytearray()
    header = None
    while pos < len(data):
        length = struct.unpack('>I', data[pos:pos + 4])[0]
        kind = data[pos + 4:pos + 8]
        body = data[pos + 8:pos + 8 + length]
        if kind == b'IHDR':
            header = struct.unpack('>IIBBBBB', body)
        elif kind == b'IDAT':
            idat += body
        elif kind == b'IEND':
            break
        pos += 12 + length
    return header, bytes(idat)


def paeth(a, b, c):
    p = a + b - c
    pa, pb, pc = abs(p - a), abs(p - b), abs(p - c)
    if pa <= pb and pa <= pc:
        return a
    if pb <= pc:
        return b
    return c


def rows(path):
    (width, height, depth, ctype, _, _, interlace), idat = read_chunks(path)
    assert depth == 8 and ctype in (2, 6) and interlace == 0, (depth, ctype, interlace)
    bpp = 3 if ctype == 2 else 4
    stride = width * bpp
    decomp = zlib.decompressobj()
    buf = bytearray()
    prev = bytearray(stride)
    produced = 0
    for start in range(0, len(idat), 1 << 16):
        buf += decomp.decompress(idat[start:start + (1 << 16)])
        while len(buf) >= stride + 1 and produced < height:
            ftype = buf[0]
            line = bytearray(buf[1:1 + stride])
            del buf[:1 + stride]
            if ftype == 1:
                for i in range(bpp, stride):
                    line[i] = (line[i] + line[i - bpp]) & 0xFF
            elif ftype == 2:
                for i in range(stride):
                    line[i] = (line[i] + prev[i]) & 0xFF
            elif ftype == 3:
                for i in range(stride):
                    a = line[i - bpp] if i >= bpp else 0
                    line[i] = (line[i] + ((a + prev[i]) >> 1)) & 0xFF
            elif ftype == 4:
                for i in range(stride):
                    a = line[i - bpp] if i >= bpp else 0
                    c = prev[i - bpp] if i >= bpp else 0
                    line[i] = (line[i] + paeth(a, prev[i], c)) & 0xFF
            yield bytes(line)
            prev = line
            produced += 1


def write_png(path, width, height, rows_rgb):
    def chunk(kind, body):
        return (struct.pack('>I', len(body)) + kind + body +
                struct.pack('>I', zlib.crc32(kind + body) & 0xFFFFFFFF))
    raw = b''.join(b'\x00' + row for row in rows_rgb)
    out = (b'\x89PNG\r\n\x1a\n' +
           chunk(b'IHDR', struct.pack('>IIBBBBB', width, height, 8, 2, 0, 0, 0)) +
           chunk(b'IDAT', zlib.compress(raw, 6)) + chunk(b'IEND', b''))
    open(path, 'wb').write(out)


def main():
    mode, src, dst = sys.argv[1], sys.argv[2], sys.argv[3]
    (width, height, _, ctype, _, _, _), _ = read_chunks(src)
    bpp = 3 if ctype == 2 else 4
    if mode == 'overview':
        tw = int(sys.argv[4]) if len(sys.argv) > 4 else 3000
        out = []
        for y, line in enumerate(rows(src)):
            step = width / tw
            row = bytearray()
            for x in range(tw):
                sx = min(width - 1, int(x * step))
                row += line[sx * bpp:sx * bpp + 3]
            out.append(bytes(row))
        write_png(dst, tw, height, out)
        print(dst, f'{tw}x{height}')
    elif mode == 'crop':
        x0, y0, w, h = (int(v) for v in sys.argv[4:8])
        out = []
        for y, line in enumerate(rows(src)):
            if y0 <= y < y0 + h:
                out.append(line[x0 * bpp:(x0 + w) * bpp])
        write_png(dst, w, len(out), out)
        print(dst, f'{w}x{len(out)}')


if __name__ == '__main__':
    main()
