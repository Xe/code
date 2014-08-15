#!/usr/bin/python

import cmd, os

test = None
var = []

def do_args(args):
    print args.split()

def do_driver(args):
    arg = args.split()
    test_passed = False
    
    if len(arg) > 0:
    	var1 = float(arg[0])
    	print var1
    	var2 = float(arg[1])
    	print var2
    	var3 = float(arg[2])
    	print var3
    else:
    	var1 = 0.5
    	var2 = var1
    	var3 = var1
    try:
	    import outputs, testing
	    global test
	    test = outputs.Driver(1,2,3,4)
	    print #nothing
	    print "Doing tankDriveRaw:"
	    test.tankDriveRaw(var1, var2)
	    print #nothing
	    print "Doing holonomicDriveRaw:"
	    test.holonomicDriveRaw(var1, var2, var3)
	    print #nothing
	    print "testing the fancy list-based versions, tankDrive then holonomicDrive"
	    test.tankDrive([0, var1], [0, var2])
	    test.holonomicDrive([var1, var2, 0, var3])
	    print #nothing
	    print "Don\'t forget to stop the robot"
	    test.stop()
	    test_passed = True
	
    except:
	    print "fail"

    if test_passed:
	    print "The robot is able to move, test passed"

def do_pneumatics_noid():
    testMode = 2

    #if args[0] == "single":
    #    testmode = 1
    #else:
    #    testmode = 2

    import outputs
        
    noids = None
        
    try:
        noids = outputs.dualSolenoid(1,2)
        print noids
    except:
        print "Fail at object creation"
    
    print "now trying to extend"
    
    try:
        noids.extend()
    except:
        print "fail at extend"
    
    print "trying to retract"
    
    try:
        noids.retract()
    except:
        print "fail at retract"
    
    print "now testing the state checking mechanism in the outputs.dualSonenoid class"
    
    try:
        noids.extend()
        print "extended"
        print noids.get()
        noids.retract()
        print "retract"
        print noids.get()
    except:
        print "fail at get function"
    
    print "\nLooks like its a pass!"

def do_pneumatics_relay():
    import testing as wpi
    import outputs as out
    
    print 'creating relay object'
    relay1 = None
    try:
        relay1 = wpi.Relay(1)
        print "Relay created without error"
    except:
        print "Relay creation failed"
    
    print "attempting to extend the relay"
    try:
        relay1.Set(relay1.kForward)
        print "it worked"
    except:
        print "it appears to have failed. thats not good"
    
    print "attempting to retract the relay"
    try:
        relay1.Set(relay1.kOff)
        print "it worked"
    except:
        print "it's stuck out"
    
    print "pass"
    
def do_pneumatics_relay_new():
    import outputs as out
    
    print 'creating relay object new version'
    relay1 = None
    try:
        relay1 = out.relay(1)
        print "Relay created without error"
    except:
        print "Relay creation failed"
    
    print "attempting to extend the relay"
    try:
        relay1.forward()
        print "it worked"
    except:
        print "it appears to have failed"
    
    print "attempting to retract the relay"
    try:
        relay1.off()
        print "it worked"
    except:
        print "it's stuck out"
    
    print "pass"

def do_pygameJoy():
    import testing as wpi
    test = None
    try:
        test = wpi.Joystick(1) #equiv. to first joystick
        print "You attatched a joystick and its object was created"
    except:
        print "attatch a joysyick you nimrod"
        RuntimeError("FAIL")
        return None
    
    print "testing inputs ^C to stop"
    while True:
        try:
            for n in range(0,3):
                print test.GetRawAxis(n)
        except:
            break
    
    print "Testing buttons, same to stop"
    while True:
        try:
            for n in range(0,11):
                if n:
                    print str(n) + " pressed!"
        except:
            break

def input(events): 
   for event in events: 
      if event.type == QUIT: 
         sys.exit(0) 
      else: 
         print event 

def do_pygameInit():
    global count
    import pygame
    pygame.init()
    pygame.joystick.init()
    count = pygame.joystick.get_count()
    js = pygame.joystick.Joystick(0)
    js.init()
    done = False
    while not done:
        try:
            key = pygame.key.get_pressed()
            pygame.event.pump()
            jx = js.get_axis(0)
            jy = js.get_axis(1)
            print [jx, jy]
        except:
            done = True

class tests(cmd.Cmd):
    def __init__(self):
	    cmd.Cmd.__init__(self)
	    self.prompt = "2412_debug# "
    
    def do_driver(self, args):
	    """Actually tests the driver class I wrote"""
	    return do_driver(args)
	
    def do_pneumatics(self, args):
        """
        I added in pneumatics support today (well, solenoid support :P)
        and this makes sure it works
        """
        arg1 = args.split()
        if arg1[0] == "solenoid":
            do_pneumatics_noid()
        elif arg1[0] == "relay":
            do_pneumatics_relay()
        elif arg1[0] == "new":
                do_pneumatics_relay_new()
        else:
            print arg1[0]
        
    def do_EOF(self, args):
	    import sys
	    print
	    sys.exit(0)
    
    def do_args(self, args):
	    """A dummy to test variable handling"""
	    return do_args(args)
	    
    def do_robotOut(self, args):
	    """Allows you to see robot debug outs if you are running linux, if
	    you aren't, let me know"""
	    args = args.split()
	    print "^C to quit"
	    os.popen("ncb")
    
    def do_show(self, args):
	    """A failed show command"""
	    arg = args.split()
	
	    if arg[0] == "test":
	        print test
	
	    if arg[0] == "ver":
	        print "Version 0.2, Touko"
	
	    else:
	        for n in var:
		        if arg[0] == n:
		            print var[n][arg[0]]
    
    def do_var(self, args):
	    """Sets variables, can't do much else :("""
	    args = args.split()
	
	    if args[0] == "test":
	        test = args[1]
	        print "test case"
	    else:
	        var.append({args[0]:args[1]})
	        print "other case"
	    print "variable " + args[0] + " set to " + args[1]
    
    def do_showVars(self, args):
	    """Shows all raw variables"""
	    args = args.split()
	
	    for n in var:
	        print n
    
    def do_prompt(self, args):
	    """Allows you to change your prompt"""
	    args = args.split()
	
	    self.prompt = args[0]
	
    def do_comp(self, args):
	    """
	    COMPRESSOR WORKIE?
	    """
	    import outputs
	    outputs.initComp()

    def do_testAll(self, args):
        """
        one of my most brilliant hacks yet.  it puts all of the functions
        in a list and iterates through it, executing them as needed.  the
        try is there because some take arguments and some don't
        """
        print "############################"
        for func in [self.do_comp, do_driver, do_pneumatics_noid, do_pneumatics_relay, do_pneumatics_relay_new, do_pygameJoy]:
            print func
            try:
                func("0.5 0.7 -.9")
            except:
                func()
            print "############################"
    
    def do_pygameTest(self, args):
        """
        Attatch a joystick
        """
        do_pygameInit()
    
    def do_cure_cancer(self, args):
	    """the obligatory easter egg"""
	    args = args.split()
	
	    print "sorry, my mentor told me to not make programs cure cancer :P"

main = tests()

main.cmdloop()
