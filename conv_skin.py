#!/usr/bin/python
from PIL import Image
import lzf


def compress(data):
    return chr(len(data) % 256) + chr(len(data) / 256) \
        + lzf.compress(data)


img = Image.open('gb_skin.png')

palette = ''
pal = img.getpalette()
for (r, g, b) in zip(pal[0:48:3], pal[1:48:3], pal[2:48:3]):
    color = ((g & 4) << 13) + ((b >> 3) << 10) + ((g >> 3) << 5) + (r >> 3)
    palette += chr(color % 256) + chr(color / 256)

data = ''
last_pixel = None
for pixel in (img.getpixel((x, y)) for y in range(img.height) for x in
              range(img.width)):
    if last_pixel is None:
        last_pixel = pixel
    else:
        data += chr(last_pixel * 16 + pixel)
        last_pixel = None

if last_pixel is not None:
    data += chr(last_pixel * 16)

data = palette + compress(data)

print(len(data))

fout = open('skin.asm', 'w')
line = '.db '
comma = 0
for byte in data:
    to_append = ',' * comma + '$%02X' % ord(byte)
    if len(line + to_append) > 100:
        fout.write(line + '\n')
        line = '.db $%02X' % ord(byte)
    else:
        line += to_append
    comma = 1

if comma:
    fout.write(line + '\n')
fout.close()
