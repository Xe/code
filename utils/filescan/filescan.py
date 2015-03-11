#!/usr/bin/python

"""
Copyright (c) 2013 Christine Dodrill                                               
                                                                             
This software is provided 'as-is', without any express or implied            
warranty. In no event will the authors be held liable for any damages        
arising from the use of this software.                                       
                                                                             
Permission is granted to anyone to use this software for any purpose,        
including commercial applications, and to alter it and redistribute it       
freely, subject to the following restrictions:                               
                                                                             
   1. The origin of this software must not be misrepresented; you must not   
   claim that you wrote the original software. If you use this software      
   in a product, an acknowledgment in the product documentation would be     
   appreciated but is not required.                                          
                                                                             
   2. Altered source versions must be plainly marked as such, and must not be
   misrepresented as being the original software.                            
                                                                             
   3. This notice may not be removed or altered from any source              
   distribution.                                                             

"""

#Filescan is a tool for monitoring changes in the filesystem given parameters

import os, sys, datetime, time
import pyinotify

LOGFILE="/var/log/newfiles.log"
STARTSWITHPATTERN="_"
SNOOPDIR="/home"
PIDFILE="/var/lib/filescan/filescan.pid"

#Class for inotify call handling
class FileScan(pyinotify.ProcessEvent):
	def process_IN_CREATE(self, event):
		if event.name.startswith(STARTSWITHPATTERN):
			logfout.write(str(datetime.datetime.now()) + " Create: " + os.path.join(event.path, event.name) + "\n")
			logfout.flush()

#Write PID to a file for the init script
with open(PIDFILE, "w") as f:
        f.write(str(os.getpid()))

logfout = open(LOGFILE, "a")

#Pyinotify stuff

wm = pyinotify.WatchManager()
mask = pyinotify.IN_CREATE

notifier = pyinotify.Notifier(wm, FileScan())
wdd = wm.add_watch(SNOOPDIR, mask, rec=True)
		
while True:
	notifier.process_events()
	if notifier.check_events():
		notifier.read_events()

logfout.close()

