from path import *

assert(test_path("/"))
assert(test_path("/./"))
assert(test_path("/bin/../"))
assert(test_path("/anything") == False)

print "Tests passed"

