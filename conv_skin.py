from PIL import Image

img = Image.open("gb_skin.gif")

runs = []
for pixel in (img.getpixel((x,y)) for y in range(img.height) for x in range(img.width)):
    if runs and runs[-1][0] == pixel:
        runs[-1] = (runs[-1][0], runs[-1][1] + 1)
    else:
        runs.append((pixel, 1))   

data = []
pal = img.getpalette()
for r,g,b in zip(pal[0:48:3], pal[1:48:3], pal[2:48:3]):
    color = ((g & 4) << 13) + ((b >> 3) << 10) + ((g >> 3) << 5) + (r >> 3)
    data += [color % 256, color / 256]

literal = []
for pixel,run in runs:
    if len(literal) % 2:
        literal.append(pixel)
        run -= 1
    
    emit_run = (run >= 4 and not literal) or run >= 5
    while literal and (emit_run or len(literal) >= 32):
        data_to_add = [p1*16+p2 for p1,p2 in zip(literal[0:32:2],literal[1:32:2])]
        data += [len(data_to_add) - 1] + data_to_add
        literal = literal[32:]
    
    if emit_run:
        while run > 33:
            data.append((15*16) + pixel)
            run -= 32
        if run >= 4:
            data.append((run // 2 - 1)*16 + pixel)
            run %= 2
    
    literal += [pixel] * run

assert len(literal) % 2 == 0
while literal:
    data += [len(literal) // 2 - 1] + [p1*16+p2 for p1,p2 in zip(literal[0::2],literal[1::2])]
    literal = literal[32:]

print len(data)

fout = open("skin.asm", "w")
line = ".db "
comma = 0
for byte in data:
    to_append = ","*comma + ("$%02X" % byte)
    if len(line + to_append) > 100:
        fout.write(line + "\n")
        line = ".db $%02X" % byte
    else:
        line += to_append
    comma = 1

if comma:
    fout.write(line + "\n")
fout.close()