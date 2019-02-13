#!/usr/bin/python
from PIL import Image
import lzf


def compress(data):
    return bytes(chr(len(data) % 256), 'latin1') \
        + bytes(chr(len(data) // 256), 'latin1') \
        + lzf.compress(data)


img = Image.open('gb_skin.png')

palette = b''
pal = img.getpalette()
for (r, g, b) in zip(pal[0:48:3], pal[1:48:3], pal[2:48:3]):
    color = ((g & 4) << 13) + ((b >> 3) << 10) + ((g >> 3) << 5) + (r >> 3)
    palette += bytes(chr(color % 256), 'latin1') \
        + bytes(chr(color // 256), 'latin1')

data = b''
last_pixel = None
for pixel in (img.getpixel((x, y)) for y in range(img.height) for x in
              range(img.width)):
    if last_pixel is None:
        last_pixel = pixel
    else:
        data += bytes(chr(last_pixel * 16 + pixel), 'latin1')
        last_pixel = None

if last_pixel is not None:
    data += bytes(chr(last_pixel * 16), 'latin1')

data = palette + compress(data)

print(len(data))

fout = open('skin.asm', 'w')
line = '.db '
comma = 0
for byte in data:
    to_append = ',' * comma + '$%02X' % ord(chr(byte))
    if len(line + to_append) > 100:
        fout.write(line + '\n')
        line = '.db $%02X' % ord(chr(byte))
    else:
        line += to_append
    comma = 1

if comma:
    fout.write(line + '\n')
fout.close()
