# -*- coding: utf-8 -*-
"""
Created on Mon Sep 14 13:56:32 2015

@author: jonas
"""

dots_dims = [0.5, 0.7]

from psychopy import visual, event
win = visual.Window(color='black', size=[600, 600])

# Dots
dot = visual.Circle(win, units='pixels', radius=4, fillColor='white', lineColor=None, interpolate=True)
dots_style = {'fieldShape':'sqr', 'fieldSize':(dots_dims[0]*0.9, dots_dims[1]*0.9), 'dotSize':5, 'dotLife':99999, 'speed':0, 'element':dot}
dots_right = visual.DotStim(win, fieldPos=(-dots_dims[0]/2, 0), **dots_style)
dots_left = visual.DotStim(win, fieldPos=(dots_dims[0]/2, 0), **dots_style)

# Rectangles
line_style = {'width':dots_dims[0], 'height':dots_dims[1], 'lineWidth':1, 'interpolate':False}
rect_right = visual.Rect(win, pos=(-0.25, 0), **line_style)
rect_left = visual.Rect(win, pos=(0.25, 0), **line_style)

# Images
payout_large = visual.ImageStim(win, image='2kr_75px.png', size=0.2, interpolate=True)
payout_small = visual.ImageStim(win, image='25ore_75px.png', size=0.2, interpolate=True)

# Change the number of dots
def update_dots(dotstim, nDots):
    dotstim.nDots = nDots
    dotstim._dotsDir = [0]*dotstim.nDots
    dotstim. _verticesBase = dotstim._newDotsXY(dotstim.nDots)

def draw_dots_background():
    rect_right.draw()
    rect_left.draw() 
    payout_large.draw()
    payout_small.draw()

# Draw
while True:
    # Prepare stuff
    payout_large.pos = [-dots_dims[0]/2, dots_dims[1]*1.5/2]
    payout_small.pos = [dots_dims[0]/2, dots_dims[1]*1.5/2]
    update_dots(dots_left, 15)
    update_dots(dots_right, 10)
    
    for frame in range(75):        
        # Show stuff
        draw_dots_background()
        dots_left.draw()
        dots_right.draw()
        win.flip()

    # No dots
    for frame in range(2):
        draw_dots_background()
        win.flip()
    
    if event.waitKeys()[0] == 'escape':
        break