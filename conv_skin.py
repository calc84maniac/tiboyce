#!/usr/bin/python
from PIL import Image
import lzf
import struct


def compress(data):
    return struct.pack('<H', len(data)) + lzf.compress(data)


img = Image.open('gb_skin.png')

palette = bytearray()
pal = img.getpalette()
for (r, g, b) in zip(pal[0:48:3], pal[1:48:3], pal[2:48:3]):
    color = ((g & 4) << 13) + ((b >> 3) << 10) + ((g >> 3) << 5) + (r >> 3)
    palette += struct.pack('<H', color)

data = bytearray()
last_pixel = None
for pixel in (img.getpixel((x, y)) for y in range(img.height) for x in
              range(img.width)):
    if last_pixel is None:
        last_pixel = pixel
    else:
        data.append(last_pixel * 16 + pixel)
        last_pixel = None

if last_pixel is not None:
    data.append(last_pixel * 16)

data = palette + compress(bytes(data))

print(len(data))

fout = open('skin.asm', 'w')
line = '.db '
comma = 0
for byte in data:
    to_append = ',' * comma + '$%02X' % byte
    if len(line + to_append) > 100:
        fout.write(line + '\n')
        line = '.db $%02X' % byte
    else:
        line += to_append
    comma = 1

if comma:
    fout.write(line + '\n')
fout.close()
