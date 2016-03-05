import sys, subprocess

def data2db(data):
	out = ""
	line = 0
	for byte in data:
		if line == len(out):
			out += ".db "
		else:
			out += ","
		out += str(ord(byte))
		if len(out)-line > 100:
			out += "\n"
			line = len(out)
	return out


def main(argv):
	rom = open(argv[1], "rb").read()
	appvsize = 0x4000*3
	
	i = (len(rom)-1)//appvsize + 1
	digits = 0
	while i > 0:
		digits += 1
		i //= 10
	for i in range(0, len(rom), appvsize):
		fname = argv[2] + "%0*d.8xv"%(digits, i/appvsize)
		fout = open(fname, "w")
		fout.write(data2db(rom[i:i+appvsize]))
		fout.close()
		subprocess.call(["SPASM.exe", fname, fname])

main(sys.argv)