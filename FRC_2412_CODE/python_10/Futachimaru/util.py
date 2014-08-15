#Utility functions for the robot

try:
    import wpilib as wpi
except:
    import testing as wpi

def readFileAndSplit_old(name, wordNumber, pattern):
    return open(name, "r").read().split()[wordNumber].lower() == pattern
    #this accomplishes the same thing as the function immediatly
    #following this one, opens a file, reads it, splits the 
    #string, gets the arbitrary word, sees if it matches the pattern,
    #and returns the result, that was easy, eh?

def readFileandSplit(name, wordNumber, pattern):
    result = open(name, "r")
    result = result.read()              #result is now string
    result = result.split()             #result is now list
    result = result[wordNumber].lower() #result is now both list and lower case
    return result == pattern   #if the shoe fits, she must wear it

def readForDog():
    return readFileAndSplit("config.ini", 0, "True")

def fail(failType):
    """use this if you want the code to stop for whatever reason"""
    if failType == 1:
        raise RuntimeError("Error-forced, see traceback for instances of util.fail(1)")
    else:
        raise RuntimeError("Error-General, PICNIC, xyzzy")

def motd():
    print("Hello, this code is now running, run for your life :P")

def tryFunc(func, printItOut = True):
    """This is a generic debug function, it can actually be useful
    for competition as well"""
    try:
        func()
        return True
    except Exception,e:
        if printItOut:
            print e.message
        return False
