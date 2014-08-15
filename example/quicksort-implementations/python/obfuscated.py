#!/usr/bin/python

# This is the simplest way I know how to implement quicksort in python.

qsort = lambda pivot=None, *sortables: [] if pivot is None else qsort([a for a in sortables if a < pivot]) + [pivot] + qsort([b for b in sortables if b >= pivot])

"""
>>> qsort([])
[]
>>> qsort([4,2,6,5])
[2,4,5,6]
"""
