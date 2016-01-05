import noddy
import state

print("1 testing state")

state.set("foo", "bar")
print("  ", state.get("foo"))
print("  ", state.get("bar"))

print("   pass")

print("2 test noddy")

n = noddy.Noddy("Rick", "James", 5)
print("  ", n.first, n.last, n.number)
print("  ", n.name())

print("   pass")

print(dir(state))
print(dir(noddy))
