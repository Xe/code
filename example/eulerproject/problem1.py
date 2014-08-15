#!/usr/bin/python

numbers = [range(1000), range(1000)]

final = filter(lambda x: x % 3 == 0, numbers[0])
final.extend(filter(lambda x: x % 5 == 0, numbers[1]))

print reduce((lambda x,y: x+y), set(final))
