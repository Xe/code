#!/usr/bin/python

import sys, json

#checkTags = lambda tags, snoop: snoop is in tags.split(", ")

def checkTags(tags, snoop):
	tags = tags.split(", ")
#	sys.stderr.write(str(tags))
	if snoop in tags:
		sys.stderr.write("Found an image! :D\n")
		return True
	else:
		return False


assert(sys.argv[1])

fin = open(sys.argv[1])

data = fin.read()

derpiobj = json.loads(data)

if checkTags(derpiobj["tags"], "explicit"):
	print "http:" + derpiobj["image"]

