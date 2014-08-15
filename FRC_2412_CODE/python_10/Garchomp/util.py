#Utility functions for the robot

try:
    import wpilib as wpi
except:
    import testing as wpi

#def readFileAndSplit_obfu(name, wordNumber, pattern):
#    return open(name, "r").read().split()[wordNumber].lower() == pattern.lower()
    #this accomplishes the same thing as the function immediatly
    #following this one, opens a file, reads it, splits the 
    #string, gets the arbitrary word, sees if it matches the pattern,
    #and returns the result, that was easy, eh?

#def readFileandSplit(name, wordNumber, pattern):
#    print("File " + name + " opened to look for " + pattern + " at index " + str(wordNumber))
#    result = open(name, "r")
#    result = result.read()              #result is now string
#    result = result.split()             #result is now list
#    result = result[wordNumber].lower() #result is now both list and lower case
#    print("found " + result)
#    return result == pattern.lower()   #if the shoe fits, she must wear it

def fail(failType = None):
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
    except:
        return False

def sep():
    print("="*40)

def enum(**enums):
    return type('Enum', (), enums)

def sum_of(n,m):
    return n + m
def diff_of(n,m):
    return n-m
def produkt_of(n,m):
    return n*m
def quoshunt_of(n,m):
    return n/m

last_out = ""

def puts(out):
    global last_out
    if out == last_out:
        pass
    else:
        print(out)
        last_out = out
