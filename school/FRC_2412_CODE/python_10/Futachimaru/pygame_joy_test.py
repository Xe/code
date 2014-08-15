#!/usr/bin/python

# In this example the function joystick_transform will transform the
# square shaped joystick axis information into a circular shape.
# This will make the new joystick axis information easier to use while
# moving around a character or cursor.

# The RED dot represents the actual joystick information.

# The BLUE dot represents the transformed joystick information.

# The GREEN dot is a cursor that moves by the transformed joystick
# information in a motion relative to it's previous location.


import pygame
import math

def norm(v):
	l = length(v)
	if l != 0:
		return (v[0] / l, v[1] / l)
	return (0,0)
	
def length(v):
	return math.sqrt(v[0]*v[0] + v[1]*v[1])

# Transforms the square info of an analog joystick into circular info
def joystick_transform(j):
	# If joystick is not centered:
	if (j[0],j[1]) != (0,0):
		# Check if x axis is larger than y axis
		if abs(j[0]) > abs(j[1]):
			# Since x>y we will check for line intersection with wall
			# Get slope (m = y/x) for y = m * x (line equation)
			m = abs(j[1] / j[0])
			# At x=1.0 (intersecting right wall), y would equal m
			# scaler = length of normalized vector / length of line intersecting box
			s = 1.0 / length((1.0, m))
		else:
			# Since y>=x we will check for line intersection with ceiling
			# Get slope (m = x/y) for x = m * y (line equation)
			m = abs(j[0] / j[1])
			# At y=1.0 (intersecting ceiling), x would equal m
			# scaler = length of normalized vector / length of line intersecting box
			s = 1.0 / length((m,1.0))
	else:
		# Since the joystick is centered, the scaler will be 0
		s = 0
		
	# Simply scale the joystick axis data by the scaler
	return (j[0] * s, j[1] * s)

pygame.init()
pygame.joystick.init()

screen = pygame.display.set_mode((640,480))

js = pygame.joystick.Joystick(0)
js.init()

jx = 0
jy = 0

px = 320.0
py = 240.0

move_speed = 2.0

done = False
while not done:
	key = pygame.key.get_pressed()
	screen.fill((255,255,255))
	
	# Outer box boundry
	#pygame.draw.rect(screen, (200,200,200), ((10,10),(180,180)), 1)
	
	# Circle boundry
	#pygame.draw.circle(screen, (0,0,0), (100,100), 90, 1)
	
	# Center point
	#pygame.draw.circle(screen, (200,200,200), (100,100), 2, 1)
	
	jx = js.get_axis(0)
	jy = js.get_axis(1)
	n = norm((jx,jy))
	
	# Line representing normalized joystick information
	pygame.draw.line(screen, (200,200,200), (100,100), (100 + int(n[0] * 90.0), 100 + int(n[1] * 90.0)))
	
	# Raw joystick information
	#x = 100 + int(jx * 90.0)
	#y = 100 + int(jy * 90.0)
	#pygame.draw.circle(screen, (255,0,0), (x, y), 5)
	
	# Transformed joystick information
	tj = joystick_transform((jx,jy))
	#x = 100 + int(tj[0] * 90.0)
	#y = 100 + int(tj[1] * 90.0)
	#pygame.draw.circle(screen, (0,0,255), (x, y), 5)
	
	# Cursor moved by transformed joystick information
	px = px + tj[0] * move_speed
	py = py + tj[1] * move_speed
	pygame.draw.circle(screen, (0, 255, 0), (int(px), int(py)), 5)
	
	pygame.display.flip()
	
	for event in pygame.event.get():
		if event.type == pygame.QUIT: 
			done = True
			
		if key[pygame.K_ESCAPE]:
			done = True
