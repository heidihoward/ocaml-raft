import fileinput,string
packetStart = ""
numNodes = ""
messages = []
nodeModeChanges = []
timeouts = []
for line in fileinput.input():
	if (line.lstrip().startswith("{'source'")):
		packetStart = line

	elif (line.lstrip().startswith("'numNodes'")):
		numNodes = line

	elif (line.find("'expires'") > -1 ):
		timeouts.append(line)

	elif (line.find("'newMode'") > -1):
		nodeModeChanges.append(line)
	
	elif (line.lstrip().startswith("'arrives':")):
		messages.append(packetStart + line)
	

print (("{" + numNodes + "'messages':[" + string.join(messages,",") + "],'nodeModeChanges':[" + string.join(nodeModeChanges,",") + "],'timeouts':[" + string.join(timeouts,",") + "]}").replace("'",'"') )


