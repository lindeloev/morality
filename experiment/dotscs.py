# -*- coding: utf-8 -*-
"""
Experiment on the influence of free Working Memory Capacity (WMC) on social
cognition. WMC is loaded using a Complex Span (CS) task and social cognition is
measured using the Public Goods Game (PGG).

This experiments support various variations: span lengths, stimulus type etc.

This script uses the psychopy python module, a powerful framework for stimulus
delivery. Read more and download it at www.psychopy.org.

This script is public domain and may be used and distributed without attribution.
Keep science open! Thanks to http://401kcalculator.org for the money-image.

Written for PsychoPy 1.80.06
Author: Jonas Lindeløv, jonas@cnru.dk, lindeloev.net

NOTES:
 * The condition "ControlCS" is a leftover from an earlier version of the script


TO DO:
 * Add progress bar

NOTE TO SELF: fix this in the template:
  * keyList=None
  * Lau's comment
"""

from __future__ import division

"""
SET VARIABLES
"""
# General variables
monDistance = 70                 # Distance between subject's eyes and monitor
monWidth = 34.5                  # Width of your monitor in cm
monitorSizePix = [1366, 768]    # Pixel-dimensions of your monitor
saveFolder = 'data'              # Log is saved to this folder. The folder is created if it does not exist.

# Trials
# See also "spans" seection below the dialogue boxes

# Timing and appearance
glucoseTime = 20 * 60  # how much CS + PGG-self time before glucose measure is taken.

encodeFrames = {'lettersGrid': 48, 'faces': 120} # for how long should the recall stimuli be displayed?

faceSizeStim = 10
recallOptionSize = 5  # size (deg) of faces images as recall options
spacingFactorRecall = 1.1  # spacing between response images relative to their sizes
recallDimensions = [4, 3]  # [x, y] for faces
recallNumber = 12  # number of letters to choose from (chould be the same as np.prod(recallDImensions))

letterHeight = 1  # height of encoding letters
letterSpacing = 2  # x-distance between recall letters
letterLabelPos = [0, -2]  # y-distance from recall letters to the sequence labels

dots_dims = [3, 4]  # dimensions of each dots rectangle
dots_radius = 0.07  # size of individual dots in degrees
dots_reference = 15  # how many dots in one of the rectangles
dots_Ns = [10, 12, 14]  # how many dots in the other rectangle
dots_duration = 75 # number of frames                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 

dots_payouts = {
    '1-7': [5, 0.5],
    '3-5': [5, 0.5],
    '1-3': [5, 0.5],
    '5-7': [5, 0.5],
    'practiceDots': [2, 0.2],
    'controlDotsCheat': [2, 0.2],
    'controlDotsAccuracy': [2, 0.2],
    'practiceCS':  [0,0], 
    'practiceExp': [0,0]
}

posRecallText = [0, 6.5]
posRecallGrid = [0, -1]
posTextQAnswer = [0, -2]

textHeight = 0.4  # default text height for instructions
waitFeedback = 0.6  # how long to show feedback (seconds). "ask" will wait for key.
preventDoubleClickTime = 0.5  # a break between "return" presses to prevent accidental double-clicks

# Responses
keysEquation = {'up':1, 'down':0}  # mapping of response keys to meaning (up=True, down=False)
keysRecallFaces = {'left':-1, 'right':1, 'down':recallDimensions[0], 'up':-recallDimensions[0]}
keysRecallLetters = {'left':-1, 'right':1}
keysAns = ['return', 'enter', '\n']
keysDots = ['left', 'right']
keysSelect = ['space', 'spacebar']
keysQuestionnaire = ['F5', 'f5']  # to go straight to questionnaire
keysLockscreen = ['F2', 'f2']  # do not allow subject to continue
keysQuit = ['F9', 'f9']  # quit experiment immediately


instructWelcome = u"""*** Welcome! ***

Thanks for choosing to participate in the study about memory and dot counting. Please read the instructions carefully and follow them. You have a copy of these instructions on a sheet of paper next to you.

In this study you will be asked to memorize letters or faces, complete some simple arithmetic tests, and count dots. You will earn extra money depending on how you respond on the dots task but in order to keep most of these money, you will have to do well on memory and equations. This simply means that you will earn the most from doing your best on all parts of this experiment!

The experiment is completely anonymous. The results of this research may be published, but no individual data will be published.

Press RETURN to continue..."""


instructDots = u"""*** Dots task ***

You will be shown some dots to the left and some dots to the right. Use the left- and right-arrow keys to indicate which side has the most dots. The dots will be shown a limited amount of time, so be quick.

Choosing one side will earn you ten times more money than the other. But note that your task is solely to respond which side has the most dots, even when that side gives you a smaller bonus than the other. Depending on your answers, you can earn between %i kr. (always choosing the low-payout side) and %i kr. (always choosing the high-payout side), assuming that you have perfect performance on the memory and arithmetic. Again, this doesn't really matter since you should only attend to the number of dots.

Press Return to continue..."""

instructDotsControlCheat = u"""*** Dots task ***

Now you will do just the dots task without the memory and arithmetic. As during the first part of the experiment, the payouts are shown on each side but your task is to respond which side has the most dots.

Press RETURN to continue..."""

instructDotsControlAccuracy = u"""*** Dots task ***

Now you will do just the dots task without the memory and arithmetic. Here you will always get the highest payout by choosing the side with the most dots. Note that during this part of the experiment, payouts are not shown since that would reveal the correct answer.

Press RETURN to continue..."""

instructEquations = u"""*** Equations ***

In the upcoming experiment, you will have to check equations like 9/3 + 2 = 5 and see whether it is true, i.e. whether the left hand side and the right hand side are the same number.

Just to brush up your basic math, multiply and divide BEFORE adding or subtracting. So in the equation "6/2 - 3 = 1", first divide so it simplifies to "3 - 3 = 1" and then do the subtraction so it simplifies to "0 = 1". You will see that this equation is wrong.

As another example, '2*4 - 3 = 5' simplifies to '8 - 3 = 5' which simplifies to '5 = 5' and you see now that this equation is correct.
"""

instructCSFaces = u"""*** Instructions for memory and arithmetic ***

You will be shown a face, an equation, a face, an equation etc. 

Equations: Use the keys ARROW UP (=correct) or ARROW DOWN (=incorrect) to answer whether the equations are correct or not. E.g. when shown "2*2+4=8 ?" press ARROW UP because it is correct. When shown "6/3-2=2 ?" press ARROW DOWN because it is incorrect. 

It is very important that you do the arithmetic task as quickly and accurately as possible. You will be given feedback on how accuracte you've been so far in the experiment. It is VERY important that you answer at least 85% of the equations correctly since we would otherwise have to discard your data. Also, if you are too slow, your answer will be discarded and you will not earn money from that trial. What counts as slow will be determined from your other responses, so we cannot give you any specific time here. Doing your best to respond quickly should keep you safely below the time limit.

Faces: In the end, you will have to recall which faces were presented. Use the arrow keys to navigate between 12 options and SPACE to select the faces you saw. Press RETURN when you have finished. You will get points if you select the correct face at the correct location in the sequence. If you forget e.g. what was the first face presented, just choose your best guess.

Press RETURN to start practice..."""

instructCSLetters = u"""*** Instructions for memory and arithmetic ***

You will be shown a letter, an equation, a letter, an equation etc. 

Equations: Use the keys ARROW UP (=correct) or ARROW DOWN (=incorrect) to answer whether the equations are correct or not. E.g. when shown "2*2+4=8 ?" press ARROW UP because it is correct. When shown "6/3-2=2 ?" press ARROW DOWN because it is incorrect. 

It is very important that you do the arithmetic task as quickly and accurately as possible. You will be given feedback on how accuracte you've been so far in the experiment. It is VERY important that you answer at least 85% of the equations correctly since we would otherwise have to discard your data. Also, if you are too slow, your answer will be discarded and you will not earn money from that trial. What counts as slow will be determined from your other responses, so we cannot give you any specific time here. Doing your best to respond quickly should keep you safely below the time limit.

Letters: In the end, you will have to recall which letters were presented. Use the arrow keys to navigate between the options and SPACE to select the letters you saw. Press RETURN when you have finished. You will get points if you select the correct letter at the correct location in the sequence. If you forget e.g. what was the first face presented, just choose your best guess.

Press RETURN to start practice..."""


instructExp = u"""*** Instructions for the real task ***

You will now have to do both the dots task and the memory/arithmetic task. You start with faces/arithmetic but just before the recall, you will be presented with the dots task. After recall you will be given feedback on how well you did. Remember that it is VERY important that you answer at least 85% of the equations correctly for us to use your data but also for you to earn the largest bonus. The better you do on average, the more you'll earn! However, on half of the rounds, the money will be recieved by someone who will participate in this experiment later instead of you.

Some trials will be easy. Others will be very hard. Don't worry, just do your best.

Press RETURN to start practice..."""


instructMixed = u"""*** Experiment begins now! ***

That's it! This is the way the experiment goes. Remember to do your best on the equations, on memorizing and on the dots task.

Press RETURN to start the experiment..."""


instructDebriefing = u"""***Debriefing***

This study was about memory and the influence on monetary incentives on task compliance.

Please go to the counter, where you will state your cpr-number for the payment.

Thank you for your participation!"""

instructReceiver = {
    'self':  u"""The next payout will be for YOU! """,
    'other': u"""The next payout will be for ANOTHER PERSON! """
}


"""
SET UP MODULES AND STIMULI
"""

from psychopy import core, visual, gui, monitors, event, misc
import random
import glob
import numpy as np
import re
import string
from datetime import datetime

# Intro-dialogue. Get subject-id and other variables.
# Save input variables in "V" dictionary (V for "variables")
Vexperimenter = {'id':'', 
                 'stimType':['lettersGrid', 'faces'],
                 'order':['cheat_first', 'accuracy_first'],
                'start_at': ['instructions', 'experiment', 'dots', 'questionnaire']
                 #'level': ['1-7', '1-3', '3-5', '5-7'],
                 #'timeLimit':3600}
                 }
Vexperimenter.update({'level': '1-7'})
if not gui.DlgFromDict(Vexperimenter).OK:
    core.quit()
DIALOGUE = {'age':range(18, 80), 'gender':['male', 'female']}
if not gui.DlgFromDict(DIALOGUE, order=['age', 'gender']).OK:
    core.quit()
DIALOGUE.update(Vexperimenter)

# Translate: repetitions x 6 (dots conditions)
if DIALOGUE['level'] == '1-7':
    spans = {'experiment': [1, 2, 3, 4, 5, 6, 7], 'practiceExp': [2, 6, 4], 'controlCS': range(1, 8), 'practiceCS': [2, 3, 2, 5, 7, 4]}  # span lengths
    repetitions = {'experiment': 1, 'practiceExp': 1, 'practiceCS': 1}  # number of repetitions of each trial
"""
NEEDS UPDATE BASED ON NEW FACTORIAL DOTS! (half the repetitions since dots now adds 12 conditions for each span instead of previous 6)
elif DIALOGUE['level'] == '1-3':
    spans = {'experiment': [1,2,3], 'practiceExp': [3, 1, 2], 'controlCS': [1,2,3], 'practiceCS': [2, 1, 3, 3, 1, 2]}  # span lengths
    repetitions = {'experiment': 8, 'practiceExp': 1, 'practiceCS': 1, 'controlCS': 48}  # number of repetitions of each trial
elif DIALOGUE['level'] == '3-5':
    spans = {'experiment': [3,4,5], 'practiceExp': [5, 3, 4], 'controlCS': [3,4,5], 'practiceCS': [2, 3, 2, 5, 3, 4]}  # span lengths
    repetitions = {'experiment': 5, 'practiceExp': 1, 'practiceCS': 1, 'controlCS': 27}  # number of repetitions of each trial
elif DIALOGUE['level'] == '5-7':
    spans = {'experiment': [5,6,7], 'practiceExp': [6, 5, 7], 'controlCS': [5,6,7], 'practiceCS': [2, 3, 2, 6, 5, 7]}  # span lengths
    repetitions = {'experiment': 3, 'practiceExp': 1, 'practiceCS': 1, 'controlCS': 18}  # number of repetitions of each trial
"""
spans.update({'practiceDots': [1], 'controlDotsCheat': [1], 'controlDotsAccuracy': [1]})
repetitions.update({'practiceCS': 1, 'practiceDots': 6, 'controlDotsCheat': 14, 'controlDotsAccuracy': 14})



# Clocks and time
clock = core.Clock()  # A clock wich will be used throughout the experiment to time events on a trial-per-trial basis (stimuli and reaction times).
clock_block = core.Clock()

# Create psychopy window
monitor = monitors.Monitor('testMonitor', width=monWidth, distance=monDistance)  # Create monitor object from the variables above. This is needed to control size of stimuli in degrees.
monitor.setSizePix(monitorSizePix)
win = visual.Window(monitor=monitor, units='deg', fullscr=True, allowGUI=False, color='black')  # Initiate psychopy Window as the object "win", using the myMon object from last line. Use degree as units!

# Questions stimuli
textInstruct = visual.TextStim(win, height=textHeight, wrapWidth=15)  # Message / question stimulus.
textType = visual.TextStim(win, height=textHeight, wrapWidth=10, pos=posTextQAnswer, color='#5F5')

# Complex Span stimuli for faces
faceStim = visual.ImageStim(win, image='faces/Adam.bmp')  # during encoding
faceStim.size *= faceSizeStim / max(faceStim.size)

recallOption = visual.ImageStim(win, image='faces/Adam.bmp')  # each face during recall
recallOption.size *= recallOptionSize / max(recallOption.size)
recallSelectedCurrent = visual.Rect(win, lineWidth=7, lineColor='red', width=recallOption.size[0], height=recallOption.size[1])  # border around current selected
recallSelectedOverlay = visual.Rect(win, lineWidth=0, fillColor='green', opacity=0.5, width=recallOption.size[0], height=recallOption.size[1])  # green filter over selection
recallSelectedNumber = visual.TextStim(win, color='white', height=0.8)  # indicating sequence during recall

# Complex Span stimulus for letters
letterStim = visual.TextStim(win, height=letterHeight)
letterLabel = visual.TextStim(win, height=0.4)
letterField = visual.Rect(win, lineWidth=0, fillColor='gray', width=letterStim.height * 1.2, height=letterStim.height * 1.2)
letterSelectFrame = visual.Rect(win, lineWidth=7, lineColor='red', width=letterStim.height * 1.2, height=letterStim.height * 1.2)

# Complex Span equation
textEquation = visual.TextStim(win, wrapWidth=1000)
textRecall = visual.TextStim(win, height=textHeight, pos=posRecallText)

# DOTS STIMULI
dot = visual.Circle(win, units='pixels', radius=misc.deg2pix(dots_radius, monitor), fillColor='white', lineColor=None, interpolate=True)
dots_style = {'fieldShape':'sqr', 'fieldSize':(dots_dims[0]*0.9, dots_dims[1]*0.9), 'dotSize':5, 'dotLife':99999, 'speed':0, 'element':dot}
dots_right = visual.DotStim(win, fieldPos=(-dots_dims[0]/2, 0), **dots_style)
dots_left = visual.DotStim(win, fieldPos=(dots_dims[0]/2, 0), **dots_style)

line_style = {'width':dots_dims[0], 'height':dots_dims[1], 'lineWidth':1, 'interpolate':False}
rect_right = visual.Rect(win, pos=(-dots_dims[0]/2, 0), **line_style)
rect_left = visual.Rect(win, pos=(dots_dims[0]/2, 0), **line_style)

dots_money_left = visual.TextStim(win, text='0.00 kr.', pos=(-dots_dims[0]/2, dots_dims[1]*0.6), height=0.4)
dots_money_right = visual.TextStim(win, text='0.00 kr.', pos=(dots_dims[0]/2, dots_dims[1]*0.6), height=0.4)


# A list of images and corresponding coordinates
faceFiles = glob.glob('faces/*.bmp')
gridCoords = []
for y in range(recallDimensions[1] -1, -1, -1):  # from top to bottom
    for x in range(recallDimensions[0]):
        gridCoords += [[x * recallOption.size[0] * spacingFactorRecall, y * recallOption.size[1] * spacingFactorRecall]]
gridCoords = gridCoords - np.mean(gridCoords, axis=0)  # center
gridCoords += posRecallGrid  # shift position



"""
HELPER FUNCTIONS
"""

class csvWriter(object):
    def __init__(self, saveFilePrefix='', saveFolder=''):
        """
        Creates a csv file and appends single rows to it using the csvWriter.write() function.
        Use this function to save trials. Writing is very fast. Around a microsecond.

        :saveFilePrefix: a string to prefix the file with
        :saveFolder: (string/False) if False, uses same directory as the py file

        So you'd do this::
                # In the beginning of your script
                writer = csvWriter('subject 1', 'dataFolder')

                # In the trial-loop
                trial = {'condition': 'fun', 'answer': 'left', 'rt': 0.224}  # your trial
                writer.write(trial)
        
        See lindeloev.net for where csvWriter is maintained and new versions will be published.
        """
        import csv, time

        # Create folder if it doesn't exist
        if saveFolder:
            import os
            saveFolder += '/'
            if not os.path.isdir(saveFolder):
                os.makedirs(saveFolder)

        # Generate self.saveFile and self.writer
        self.saveFile = saveFolder + str(saveFilePrefix) + ' (' + time.strftime('%Y-%m-%d %H-%M-%S', time.localtime()) +').csv'  # Filename for csv. E.g. "myFolder/subj1_cond2 (2013-12-28 09-53-04).csv"
        self.writer = csv.writer(open(self.saveFile, 'wb'), delimiter=';').writerow  # The writer function to csv. It appends a single row to file
        self.headerWritten = False

    def write(self, trial):
        """:trial: a dictionary"""
        if not self.headerWritten:
            self.headerWritten = True
            self.writer(trial.keys())
        self.writer(trial.values())

writerTrials = csvWriter(DIALOGUE['id'] + '_trials', saveFolder=saveFolder) # writer.write(trial) will write individual trials with low latency


def getFactorPair(n):
    """
    This will return a random factor pair > 1 of a number n, with random order of the factors.
    Adapted from agf's answer: http://stackoverflow.com/questions/6800193/what-is-the-most-efficient-way-of-finding-all-the-factors-of-a-number-in-python
    """
    allFactors = list(set((i, n//i) for i in range(2, int(n**0.5) + 1) if n % i == 0))  # the set of all factors of n
    myFactors = list(random.choice(list(allFactors)))  # pick a random pair
    return random.sample(myFactors, 2)  # return the pair in random order

def makeEquation():
    """
    Make an equation like one of the following:
        5 + 2x2 = 9?
        9/3 - 1 = 2?
        3x4 - 4 = 7?
    
    To do: 
     * intercept on different sides of term2. And with minus or plus!
     * check numbers and make them controllable through function keywords.
    """
    # Settings
    boundaries = (0, 9)  # Maximum and minimum number appearing
    maxOperation = 9  # maximum number involved in multiplication/division (result included)
    changes = np.array([-2, -1, 1, 2])  # proposed changes, but they cannot exceed boundaries

    # Prepare for operation term
    nonPrimeBiggestNumbers = [x for x in range(4, maxOperation + 1) if x not in (5, 7, 11, 13, 17, 19, 23, 29, 31, 37)]  # no prime numbers
    biggestNumber = random.choice(nonPrimeBiggestNumbers)
    myFactors = getFactorPair(biggestNumber)
    
    # Operation: multiplication or division
    if random.randint(0, 1):
        operation1, operation2 = myFactors
        term2 = biggestNumber
        operator = 'x'
    else:
        operation2, term2 = myFactors
        operation1 = biggestNumber
        operator = '/'

    # Intercept and true result
    intercept = random.choice([intercept for intercept in range(-9, 9) if intercept != 0 and 0 <= intercept + term2 <= boundaries[1]])  # from -9 to 9 excluding 0 and must give result between 0 and maximum boundary
    result = term2 + intercept
    
    # Shown result: if True
    correct = random.randint(0, 1)
    if correct:
        showResult = result
    # ... or if False
    else:
        legalChanges = changes[(boundaries[0] <= result + changes) * (boundaries[1] >= result + changes)]
        showResult = result + random.choice(legalChanges)  # deviate from result with one of these numbers
    
    # Generate string equation with negative digit...
    if intercept < 0:
        equation = str(operation1) + operator + str(operation2) + '-' + str(-1 * intercept) + '=' + str(showResult) + ' ?'
    else:
        # ... or positive digit first
        if random.randint(0, 1):
            equation = str(intercept) + '+' + str(operation1) + operator + str(operation2) + '=' + str(showResult) + ' ?'
        # ... or positive digit last
        else:
            equation = str(operation1) + operator + str(operation2) + '+' + str(intercept) + '=' + str(showResult) + ' ?'
    
    return equation, correct
        

def makeTrials(condition):
    """ Create list of trials """
    trialList = []  # the purpose if this function is to fill this out
    allStims = []  # contains all possible stims. Whether that's a list of images, letters or...

    # What dots conditions to use. For practice, just pick randomly for each repetition
    import itertools
    if condition[0:8] == 'practice':
        dots_conditions = [(None, None)]  # not factorial, just pick randomly
    elif condition == 'controlDotsCheat':
        dots_conditions = list(itertools.product(dots_Ns, ['cheat']))
    elif condition == 'controlDotsAccuracy':
        dots_conditions = list(itertools.product(dots_Ns, ['accuracy']))
    else:
        dots_conditions = list(itertools.product(dots_Ns, ['cheat', 'cheat', 'cheat', 'accuracy']))
    
    # Dots side
    dots_side_iter = ['left', 'right'] * np.ceil(repetitions[condition]*len(spans[condition])*len(dots_conditions) / 2)
    np.random.shuffle(dots_side_iter)
    dots_side_iter = iter(dots_side_iter)

    # Fill trialList
    for rep in range(repetitions[condition]):  # number of repetitions is specified by condition
        for span in spans[condition]:  # span lengths depend on condition
            for dots_N, dots_incentive in dots_conditions:
                # For practice, pick random dot conditions without doing factorial
                if dots_N is None: dots_N = random.choice(dots_Ns)
                if dots_incentive is None: dots_incentive = random.choice(['cheat', 'accuracy'])
                
                # Figure out which text to show on which side based on dots_side and dots_incentive
                dots_payout = dots_payouts[condition] if condition in ('controlDotsAccuracy', 'controlDotsCheat') else dots_payouts[DIALOGUE['level']]
                dots_side = dots_side_iter.next()
                if dots_side == 'left' and dots_incentive == 'accuracy' or dots_side == 'right' and dots_incentive == 'cheat':
                    dots_money_left = dots_payout[0]
                    dots_money_right = dots_payout[1]
                if dots_side == 'right' and dots_incentive == 'accuracy' or dots_side == 'left' and dots_incentive == 'cheat':
                    dots_money_left = dots_payout[1]
                    dots_money_right = dots_payout[0]
                
                # Start with a pretty empty trial
                trial = {
                    # Complex Span stuff
                    'span': span, 
                    'encode': [], 
                    'equationText': [],
                    'equationCorrect': [],
                    'equationAnss': [],
                    'equationScore': '',
                    'equationScores': [],
                    'equationRateCumulative':'',
                    'equationRTs': [],
                    'equationProportion': '',
                    'recallAns': '',
                    'recallRT': '',
                    'recallRTs': [],
                    'recallIntersection': '',
                    'recallLocation': '',
                    'recallLocationRateCumulative':'',
                    'recallSum': '',
                    'recallProportion': '',
                    
                    # Dots stuff
                    'dotsSide': dots_side,
                    'dotsIncentive': dots_incentive,
                    'dotsN': dots_N,
                    'dotsMoneyLeft': dots_money_left,
                    'dotsMoneyRight': dots_money_right,
                    'dotsRT': '',
                    'dotsAns': '',
                    'dotsCorrect': '',
                    'moneyEarned': '',
                    'moneyEarnedCumulative': '',
                    
                    # General stuff
                    'condition': condition,
                    'pauseRT':'',
		    'utc_start':''
                }
                trial.update(DIALOGUE)
                
                # Then fill in complex span equations and stimuli
                if condition in ('experiment', 'practiceCS', 'practiceExp', 'controlCS'):
                    for i in range(span):
                        # Use all stimuli before loading anew to prevent too much interference from recall to recall.
                        if not len(allStims):
                            if DIALOGUE['stimType'] == 'faces': 
                                allStims = glob.glob('faces/*.bmp')
                            elif DIALOGUE['stimType'] == 'lettersGrid': 
                                allStims = list(string.ascii_uppercase)
                        trial['encode'] += [allStims.pop(random.randint(0, len(allStims) - 1))]  # a random element without replacement
                        equation, correct = makeEquation()
                        trial['equationText'] += [equation]
                        trial['equationCorrect'] += [correct]
                
                # Add this trial to trialList
                trialList += [trial]
    
    
    # Only randomize if this is not an instruction
    if condition[0:8] != 'practice':
        trialList = random.sample(trialList, len(trialList))
    
    # Add non-randomized stuff and return (trial numbers, dotsReciever
    repeats = np.ceil(repetitions[condition]*len(spans[condition])*len(dots_conditions) / (2*6))
    dotsReceiver = np.tile(np.repeat(instructReceiver.keys(), 6), repeats)
    for i, trial in enumerate(trialList):
        trial['no'] = i + 1  # start at 1 instead of 0
        trial['dotsReceiver'] = dotsReceiver[i]
    return trialList

"""
ASK QUESTIONS FUNCTIONS
"""

def eventOnKey(key):
    if key in keysQuit:  # Look at first reponse [0]. Quit everything if quit-key was pressed
        core.quit()
    if key in keysQuestionnaire:
        questionsAndQuit()

def ask(text='', keyList=None):
    """
    Show a text and returns answer (keypress)
    and reaction time. Defaults to no text and keysAns.
    """
    # Draw the TextStims to visual buffer, then show it and reset timing immediately (at stimulus onset)
    textInstruct.text = text
    textInstruct.draw()
    win.flip()
    clock.reset()
    core.wait(preventDoubleClickTime)

    # Halt everything and wait for (first) responses matching the keys given in the Q object.
    if keyList is None: keyList = keysAns
    keyList += keysQuit + keysQuestionnaire
    
    response = event.waitKeys(keyList=keyList, timeStamped=clock)
    eventOnKey(response[0][0])  # exit or go to questions
    
    return response[0][0], response[0][1]  # When answer given, return it.

def askScale(text='', markerStart=4, condition='', labels=[]):
    """ Ask a Likert question """
    textInstruct.text = text
    
    # Ugly to initiate on every call but necessary since RatingScale doesn't currently support setting labels post-initiation
    scaleQ = visual.RatingScale(win, labels=labels, markerStart=markerStart, low=1, high=7, stretch=2, leftKeys='left', rightKeys='right', acceptKeys=keysAns, scale='', showValue=False, showAccept=False, noMouse=True)
    scaleQ.reset()
    win.callOnFlip(core.wait, secs=preventDoubleClickTime)
    while scaleQ.noResponse:
        textInstruct.draw()
        scaleQ.draw()
        win.flip()
        
        # Get other response keys and quit/questionnaire
        otherResponse = event.getKeys(keysQuit + keysQuestionnaire)
        if otherResponse:
            eventOnKey(otherResponse[0])
    
    # Save
    trial = makeTrials('practiceCS')[0]  # data structure to save to
    trial['condition'] = condition
    trial['recallRT'] = scaleQ.getRT()
    trial['recallAns'] = scaleQ.getRating()
    writerTrials.write(trial)
    
    # Cleanup
    del scaleQ

def askFeedback(text='', color=None, wait=0.5):
    """Show feedback. wait="key" will wait for keypress. """
    origColor = textType.color  # set original color
    textType.text = text
    textType.color = color
    textType.draw()
    win.flip()
    if wait == 'key':
        event.waitKeys()
    else:
        core.wait(wait)
    textType.color = origColor  # return to original color

def askType(text='', condition='', corrAns=None, acceptedChars='all', attempts=3, feedbackCorrect='Correct!', feedbackWrong='Wrong!'):
    """ Ask question and collect typed answer. 
    TO DO:
     * if acceptedChars, do not substitute. """
    # Prepare
    currentAttempt = 1
    
    special = {'space': ' ', 'lshift':'', 'rshift':'', 'apostrophe':'\'', 'period': '.', 'minus': '-', 'comma': ',', 'colon': ':', 'exclamation': '!', 'return':'\n', 'question': '?', 'parenleft': '(', 'parenright':')', 'slash':'/', 'num_add': '+', 'num_subtract':'-', 'num_multiply':'*', 'num_divide':'/', 'plus':'+', 'equal':'=', 'percent':'=', 'doublequote':'"', 'backslash':'\\', 'semicolon':';', 'underscore':'_'}
    letters = string.ascii_lowercase + string.ascii_uppercase + u'æøåÆØÅ'
    ints = string.digits
    charSets = {'all': letters + ints + ''.join(special.values()), 'ints': ints, 'letters': letters}
    if acceptedChars in charSets.keys():
        acceptedChars = charSets[acceptedChars]
    trial = makeTrials('practiceDots')[0]  # data structure to save to
    trial['condition'] = condition
    
    # Present question
    textInstruct.text = text
    win.callOnFlip(clock.reset)    # reset clock on next flip
    currentAnswer = ''
    while True:  # wait for answer
        # Show current state
        textType.text = currentAnswer
        textType.draw()
        textInstruct.draw()
        win.flip()
        
        # Get response
        key = event.waitKeys()[0]
        eventOnKey(key)  # exit or go to questions
        if key in special.keys():  # recode special keys/characters
            key = special[key]
        
        if key == 'backspace':
            currentAnswer = currentAnswer[:-1]  # delete last character
        elif key in keysAns and currentAnswer != '':  # an attempt
            # Pressed too early, ignore
            if clock.getTime() < preventDoubleClickTime:
                continue
            
            # If there is no correct answer, just record response
            trial['recallRT'] = clock.getTime()
            trial['recallAns'] = currentAnswer
            #trial = {'id':DIALOGUE['id'], 'condition': condition, 'question': text.replace('\n', ' '), 'answer': currentAnswer, 'rt': clock.getTime()}
            
            if corrAns == None:
                writerTrials.write(trial)
                return
            # If there is a correct answer, give feedback and register attempts
            else:
                if currentAnswer == corrAns:  # correct!
                    writerTrials.write(trial)
                    askFeedback(feedbackCorrect, 'green', wait=waitFeedback)
                    break
                else:  # wrong!
                    if currentAttempt == attempts:  # this was the last attempt, continue
                        writerTrials.write(trial)
                        textInstruct.draw()  # keep question on screen
                        askFeedback(feedbackWrong + ' Press RETURN for next question.', 'red', wait='key')
                        break
                    else:  # still more attempts
                        textInstruct.draw()  # keep question on screen
                        askFeedback('Wrong! Try again.', 'red', wait=waitFeedback)
                        currentAnswer = ''  # reset current answer
                        currentAttempt += 1
        elif key in acceptedChars:  # normal characters
            currentAnswer += key

def experimenterIntervention(trialsLeft=0):
    """ Wait for experimenter and secret key """
    win.color = 'red'
    win.flip()
    
    trialsLeftText = u'\n\n' + str(trialsLeft) + ' trials remaining.' if trialsLeft else ''
    ask(u'The experimenter will come pick you up for a glucose measurement now.\nPlease wait...' + trialsLeftText, keyList=keysLockscreen)
    win.color = 'black'
    win.flip()

"""
RUN FUNCTIONS
"""

allEquationScores = []
def runCSEncode(trial):
    """ Yeah, that's what it does. """
    global allEquationScores
    # Loop through stimuli to be encoded and associated equations
    for i, encode in enumerate(trial['encode']):
        # Prepare equation
        textEquation.text = trial['equationText'][i]
        
        # Prepare encoding stimulus.
        if trial['stimType'] == 'faces':
            stim = faceStim
            stim.image = encode
        elif trial['stimType'] == 'lettersGrid':
            stim = letterStim
            stim.pos = [0, 0]
            stim.text = encode
            
        # Action: Show stimulus
        for frame in range(encodeFrames[trial['stimType']]):
            stim.draw()
            win.flip()
        
        # Present processing task
        textEquation.draw()
        win.flip()
        clock.reset()
        key, rt = event.waitKeys(keyList=keysEquation.keys()+keysQuit+keysQuestionnaire, timeStamped=clock)[0]
        eventOnKey(key)  # exit or go to questions
        trial['equationRTs'] += [rt]  # record reaction time
        trial['equationAnss'] += [keysEquation[key]]
        trial['equationScores'] += [int(keysEquation[key] == trial['equationCorrect'][i])]
    
    allEquationScores += trial['equationScores']
    trial['equationRateCumulative'] = sum(allEquationScores) / len(allEquationScores)
    trial['equationScore'] = sum(trial['equationScores'])
    return trial

allRecallLocationScores = []
def scoreCS(trial):
    """Given trial['encode'] and trial['recallAns'], score trial."""
    global allRecallLocationScores
    intersection = [encode for encode in trial['encode'] if encode in trial['recallAns']]
    location = [encode for i, encode in enumerate(trial['encode']) if encode == trial['recallAns'][i]]  # for every stim presented, check if it corresponds to the answer given.
    
    allRecallLocationScores += [1]*len(location) + [0]*(trial['span'] - len(location))
    trial['recallLocationRateCumulative'] = sum(allRecallLocationScores) / len(allRecallLocationScores)
    trial['recallLocation'] = len(location)
    trial['recallIntersection'] = len(intersection)
    trial['recallSum'] = len(intersection) + len(location)
    trial['recallProportion'] = len(location) / trial['span']
    trial['equationProportion'] = trial['equationScore'] / trial['span']
    
    return trial

def dots_update(dotstim, nDots):
    """ A way to change the number of dots without having to initialize a new stimulus"""
    dotstim.nDots = nDots
    dotstim._dotsDir = [0]*dotstim.nDots
    dotstim._verticesBase = dotstim._newDotsXY(dotstim.nDots)

def dots_draw_background():
    rect_right.draw()
    rect_left.draw() 
    dots_money_left.draw()
    dots_money_right.draw()

def runDots(trial):
    # Prepare stimuli
    if trial['dotsSide'] == 'left':
        dots_update(dots_left, trial['dotsN'])
        dots_update(dots_right, dots_reference)
    elif trial['dotsSide'] == 'right':
        dots_update(dots_right, trial['dotsN'])
        dots_update(dots_left, dots_reference)

    if trial['condition'] == 'controlDotsAccuracy':
        dots_money_left.text = dots_money_right.text = '?.?? kr.'
    else:
        dots_money_left.text = '%.2f kr.' % trial['dotsMoneyLeft']
        dots_money_right.text = '%.2f kr.' % trial['dotsMoneyRight']
    
    # Show and collect response
    win.callOnFlip(clock.reset)
    for frame in xrange(999999):  # loop for a loooong time (breaked on response)
        # Draw stuff in the first frames
        if frame < dots_duration:
            dots_draw_background()
            dots_left.draw()
            dots_right.draw()
            win.flip()
        # Blank screen on next flip
        elif frame == dots_duration:
            dots_draw_background()
            win.flip()
        else:
            core.wait(0.001)  # millisecond intervals
        
        # Listen for key responses and score it no matter what is shown
        response = event.getKeys(keyList=keysDots + keysQuit, timeStamped=clock)
        if response:
            key, rt = response[0]
            eventOnKey(key)
            
            trial['dotsRT'] = rt
            trial['dotsAns'] = key
            trial['dotsCorrect'] = int(trial['dotsAns'] == trial['dotsSide'])
            trial['moneyEarned'] = trial['dotsMoney'+key.capitalize()]  # choose Left or Right
            
            # Animate reward
            choice = dots_money_left if key == 'left' else dots_money_right
            choice.color = 'green'
            for frame in range(30):
                dots_draw_background()                    
                win.flip()
            choice.color = 'white'
            break  # break for-loop

    # Return
    return trial

def runCSRecallGrid(trial):
    """Recall faces"""
    # PREPERATION
    # Create a list "itemOptions of items to show as response options. All of targets + some fill-ins
    if trial['stimType'] == 'faces': 
        allItems = faceFiles
    elif trial['stimType'] == 'lettersGrid': 
        allItems = string.ascii_uppercase
    itemFillin = list(set(allItems) - set(trial['encode']))  # allowable fill-in response options (non-targets)
    itemFillin = random.sample(itemFillin, len(gridCoords) - trial['span'])  # selected fill-in items
    itemOptions = trial['encode'] + itemFillin  # items for this answer
    if trial['stimType'] == 'faces': 
        itemOptions = random.sample(itemOptions, len(itemOptions))  # randomize order
    if trial['stimType'] == 'lettersGrid': 
        itemOptions.sort()  # sort alphabetically
    
    # Create a BufferImageStim "grid" with an array of target and fill-in faces or letters
    for i, item in enumerate(itemOptions):
        if trial['stimType'] == 'faces': 
            recallOption.pos = gridCoords[i]  # set position
            recallOption.image = item
            recallOption.draw()
        elif trial['stimType'] == 'lettersGrid': 
            letterStim.pos = gridCoords[i]
            letterStim.text = item
            letterStim.draw()
    
    textRecall.text = 'Select ' + str(trial['span']) + ' items in their correct position in the presented sequence.\nPress SPACE to select and deselect. Press RETURN to submit answer.'
    textRecall.draw()
    grid = visual.BufferImageStim(win)  # take screenshot of back buffer
    win.clearBuffer()  # clear back buffer
    
    # ACTUAL RECALL
    win.callOnFlip(clock.reset)  # reset clock on next flip
    selectedCurrent = int(len(itemOptions) / 2)  # placeholder for current selected face. Starts in center
    selected = []  # placeholder for current answer
    while True:
        # Draw images
        grid.draw()
        grid.draw()  # because it sometimes doesn't work on first draw on bad graphics hardware!
        
        # Overlay current selection state
        for i, thisSelection in enumerate(selected):
            # Color overlay
            recallSelectedOverlay.pos = gridCoords[thisSelection]
            recallSelectedOverlay.draw()
            
            # Sequence number
            numCoord = gridCoords[thisSelection] + [1, -1] * recallOption.size * 0.3  # location off-center
            recallSelectedNumber.pos = numCoord
            recallSelectedNumber.text = i + 1  # show 1, 2, 3, etc. and not 0, 1, 2
            recallSelectedNumber.draw()
        
        # Border around current selected image
        recallSelectedCurrent.pos = gridCoords[selectedCurrent]
        recallSelectedCurrent.draw()
        
        # Show everything!
        win.flip()
        
        # Wait for response. On every keypress, update the textfield until keysAns is pressed
        response = event.waitKeys(keyList=keysRecallFaces.keys() + keysSelect + keysAns + keysQuit + keysQuestionnaire)
        eventOnKey(response[0])  # exit or go to questions
        # Move current location
        if response[0] in keysRecallFaces.keys():
            if 0 <= selectedCurrent + keysRecallFaces[response[0]] < len(itemOptions):
                selectedCurrent += keysRecallFaces[response[0]]                    
        # Add or remove an element to the current selection
        elif response[0] in keysSelect:
            if selectedCurrent in selected:
                selected.remove(selectedCurrent)
            elif len(selected) < trial['span']:  # only add if series is not filled
                selected.append(selectedCurrent)
        # Answer submitted. Record and score response
        elif response[0] in keysAns and len(selected) == trial['span']:
            trial['recallRT'] = clock.getTime()
            trial['recallAns'] = [itemOptions[index] for index in selected]  # convert indicies to image filenames for the datafile
            
            # Score trial and break out of while loop (finished OSPAN)
            trial = scoreCS(trial)
            break
    
    return trial

moneyCumulative = 0
def runFeedback(trial):
    """ Show feedback on performance"""
    global moneyCumulative
    text_feedback = ''
    # CS feedback
    if trial['condition'] in ('experiment', 'practiceExp', 'practiceCS', 'controlCS'):
        text_feedback += 'You got %i/%i equations and %i/%i items correct in this trial.' %(trial['equationScore'], trial['span'], trial['recallLocation'], trial['span'])
        text_feedback += '\nYou got %i %% equations and %i %% recall correct so far.' %(100*trial['equationRateCumulative'], 100*trial['recallLocationRateCumulative'])
    
    # Money earned is scaled with CS performance in real experiment
    if trial['condition'] in ('experiment'):
        trial['moneyEarned'] = trial['equationRateCumulative'] * trial['recallLocationRateCumulative'] * trial['moneyEarned']
        
    # Dots feedback
    if trial['condition'] in ('experiment', 'controlDots', 'practiceDots'):
        if trial['dotsReceiver'] == 'self':
            moneyCumulative += trial['moneyEarned']
            trial['moneyEarnedCumulative'] = moneyCumulative
            text_feedback += '\n\nYou earned %.2f kr. in this trial and keep them. You now have %.2f kr.' %(trial['moneyEarned'], trial['moneyEarnedCumulative'])
        elif trial['dotsReceiver'] == 'other':
            trial['moneyEarnedCumulative'] = moneyCumulative
            text_feedback += '\n\nYou made %.2f kr. in this trial and gave them to another person.\nYou now have %.2f kr.' %(trial['moneyEarned'], trial['moneyEarnedCumulative'])
    
    if trial['condition'] == 'experiment':
        text_feedback += '\n\n%i%% finished.' %(trial['no'])  # experiment goes to 85%, then control and questionnaire
    text_feedback += '\n\nPress RETURN to start next round...'
    
    key, rt = ask(text_feedback)
    trial['pauseRT'] = rt
    return trial


def saveTrial(trial):
    """ Appends a trial to the csv. 
    But first convert lists to string without quotes, brackets and paths, using regular expression'
    """
    trialSave = dict((k,v) for k,v in trial.items()) # do not alter original trial. Make a shallow copy
    for key in ('equationRTs', 'equationAnss', 'equationScores', 'recallRTs', 'equationText', 'equationCorrect', 'encode', 'recallAns'):
        trialSave[key] = re.sub('\'|\[|\]|faces/', '', str(trialSave[key]))  # remvesn', [, ], and faces/
    
    writerTrials.write(trialSave)  # save trial


"""
FUNCTIONS FOR FLOW CONTROL
"""

def runBlock(condition):    
    """Run a block of trials """

    # Old stuff from the glucose days
    """
    timeLimitPassed = False  # has experimenterInttervention() been shown?
    loops = range(1000) if condition == 'experiment' else range(1)
    for loop in loops:
    """
    clock_block.reset()
    trialList = makeTrials(condition)
    for trialNumber, trial in enumerate(trialList):
        clock.reset()
        trial['utc_start'] = (datetime.now() - datetime.utcfromtimestamp(0)).total_seconds()
        if trial['condition'] in ('experiment', 'practiceExp'):
            ask(instructReceiver[trial['dotsReceiver']])
        
        # Task flow depends on condition
        if trial['condition'] in ('experiment', 'practiceExp'):
            trial = runCSEncode(trial)
            trial = runDots(trial)
            trial = runCSRecallGrid(trial)
        
        elif trial['condition'] in ('controlCS', 'practiceCS'):
            trial = runCSEncode(trial)
            trial = runCSRecallGrid(trial)
            
        elif trial['condition'] in ('controlDots', 'practiceDots'):
            ask(instructReceiver[trial['dotsReceiver']])
            trial = runDots(trial)
        
        # Feedback and break
        trial = runFeedback(trial)
        
        # Save trial
        saveTrial(trial)
        
    """
        # Return of time's up and trials are completed. Continue otherwise.
        if condition == 'experiment' and DIALOGUE['timeLimit'] and not timeLimitPassed:
            if clock_block.getTime() > DIALOGUE['timeLimit']:
                trialsLeft = len(trialList) - trialNumber if loop==0 else 0
                experimenterIntervention(trialsLeft)
                timeLimitPassed = True
                if loop > 1:
                    return
        
    
    #At least 1 loop is finished now so trials are completed. Return if time's up (continue otherwise)
    if DIALOGUE['timeLimit'] and clock_block.getTime() > DIALOGUE['timeLimit']:
        return
    """


def runInstructCS():
    ask(instructEquations)
    if DIALOGUE['stimType'] == 'faces': ask(instructCSFaces)
    elif DIALOGUE['stimType'] == 'lettersGrid': ask(instructCSLetters)
    runBlock('practiceCS')


def runDotsBlock(condition):
    for trial in makeTrials(condition):
        trial = runDots(trial)
        saveTrial(trial)


def questionsAndQuit():
    """ 
    Show quesitons and quit. Meant to be called from anywhere in case something 
    breaks or time is up 
    """
    # Questions
    ask("Almost finished! The memory and dots task is done. Now you should fill in a short questionnaire, before you are done with the experiment.")
    askScale("How did you feel during the task", labels=['Calm', 'Stressed'], condition='qStress')
    askScale("How did you feel during the task?", labels=['Unsatisfied', 'Satisfied'], condition='qSatisfied')
    askScale("How frustrated did you feel during the task?", labels=['Not frustrated', 'Very frustrated'], condition='qFrustration')
    askScale("How difficult was the task?", labels=['Easy', 'Difficult'], condition='qDifficulty')
    askScale("How effortful was the task?", labels=['Not effortful', 'Very effortful'], condition='qEffort')
    askScale("What is your political attitude?", labels=['Left', 'Right'], condition='qAttitude')
    askType("What was your approximate birthweight in grams? (type) 0 means 'no clue'.", condition='qBirthweight', acceptedChars='ints')
    askType("What is your approximate number of Facebook friends? (type) 0 means 'no clue'.", condition='qFriends', acceptedChars='ints')
    askType("What do you think this study is about? Explain briefly (type).", condition='qAbout')
    askType("Do you have any brief comments or suggestions for improvement of this experiment? (type).", condition='qFeedback')
    
    # Debriefing
    ask(instructDebriefing)
    core.quit()

"""
RUN EXPERIMENT
"""
#if DIALOGUE['timeLimit']:
#    experimenterIntervention()
runBlock('experiment')


if DIALOGUE['start_at'] in ('instructions'):
    # Welcome
    ask(instructWelcome)
    
    # Dots practice
    high = dots_payouts[DIALOGUE['level']][0]*6*len(spans['experiment'])*repetitions['experiment'] + dots_payouts['controlDotsCheat'][0]*6*repetitions['controlDotsCheat']
    low  = dots_payouts[DIALOGUE['level']][1]*6*len(spans['experiment'])*repetitions['experiment'] + dots_payouts['controlDotsCheat'][1]*6*repetitions['controlDotsCheat']
    ask(instructDots %(low, high))
    runDotsBlock('practiceDots')
    
    # Complex span practice
    ask(instructEquations)
    if DIALOGUE['stimType'] == 'faces': ask(instructCSFaces)
    elif DIALOGUE['stimType'] == 'lettersGrid': ask(instructCSLetters)
    runBlock('practiceCS')
    
    # Practice experiment
    ask(instructExp)
    runBlock('practiceExp')

if DIALOGUE['start_at'] in ('instructions', 'experiment'):
    # Run experiment!
    ask(instructMixed)
    try:
        runBlock('experiment')
    except MemoryError:
        pass

if DIALOGUE['start_at'] in ('instructions', 'experiment', 'dots'):
    # Dots base rates depending on order
    if DIALOGUE['order'] == 'cheat_first':
        ask(instructDotsControlCheat)
        runDotsBlock('controlDotsCheat')
        ask(instructDotsControlAccuracy)
        runDotsBlock('controlDotsAccuracy')
    elif DIALOGUE['order'] == 'accuracy_first':
        ask(instructDotsControlAccuracy)
        runDotsBlock('controlDotsAccuracy')
        ask(instructDotsControlCheat)
        runDotsBlock('controlDotsCheat')

if DIALOGUE['start_at'] in ('instructions', 'experiment', 'dots', 'questionnaire'):
    # Bye
    questionsAndQuit()
