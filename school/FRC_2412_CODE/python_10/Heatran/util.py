#Utility functions for the robot

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
    print("="*60)

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

def inline_if(condition, value1, value2):
    return {True: value1, False: value2}[condition]

def max_of_4(a,b,c,d):
    return abs(max(max(abs(a),abs(b)), max(abs(c),abs(d)))) 

def reverse(n):
    """n MUST BE A LIST"""
    return n[::-1]
