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

TO DO:
 * show status at red screen

NOTE TO SELF: fix this in the template:
  * keyList=None
  * Lau's comment
"""

"""
SET VARIABLES
"""
# To show to the user
pggAmount = 100  # how much money to maximally invest in a common project?

# General variables
monDistance = 70                 # Distance between subject's eyes and monitor
monWidth = 34.5                  # Width of your monitor in cm
monitorSizePix = [1920, 1080]    # Pixel-dimensions of your monitor
saveFolder = 'data'              # Log is saved to this folder. The folder is created if it does not exist.

# Trials
# See also "spans" seection below the dialogue boxes

# Timing and appearance
glucoseTime = 20 * 60  # how much CS + PGG-self time before glucose measure is taken.

encodeFramesFaces = 120  # for how long should the recall stimuli be displayed?
encodeFramesLetters = 48  # --||-- for letters. 48 frames = 0.8 secs

faceSizeStim = 10
recallOptionSize = 5  # size (deg) of faces images as recall options
spacingFactorRecall = 1.1  # spacing between response images relative to their sizes
recallDimensions = [4, 3]  # [x, y] for faces
recallNumber = 12  # number of letters to choose from (chould be the same as np.prod(recallDImensions))

letterHeight = 1  # height of encoding letters
letterSpacing = 2  # x-distance between recall letters
letterLabelPos = [0, -2]  # y-distance from recall letters to the sequence labels

posRecallText = [0, 6.5]
posRecallGrid = [0, -1]
posTextPGG = [0, -1]
posPGGImage = [0, 5]
pggImageSize = 10
posTextQAnswer = [0, -2]

textHeight = 0.4  # default text height for instructions
maxWaitEquations = 5  # maximum wait time for the equations
waitFeedback = 0.6  # how long to show feedback (seconds). "ask" will wait for key.
preventDoubleClickTime = 0.5  # a break between "return" presses to prevent accidental double-clicks

# Responses
keysEquation = {'up':1, 'down':0}  # mapping of response keys to meaning (up=True, down=False)
keysRecallFaces = {'left':-1, 'right':1, 'down':recallDimensions[0], 'up':-recallDimensions[0]}
keysRecallLetters = {'left':-1, 'right':1}
keysAns = ['return', 'enter']
keysSelect = ['space', 'spacebar']
keysQuestionnaire = ['F5', 'f5']  # to go straight to questionnaire
keysLockscreen = ['F2', 'f2']  # do not allow subject to continue
keysQuit = ['F9', 'f9']  # quit experiment immediately
scaleTicks = 20


instructWelcome = """*** Welcome! ***

Thanks for choosing to participate in the study about memory and interaction. Please read the instructions carefully and follow them. You have a copy of these instructions on a sheet of paper next to you.

In this study you will be asked to memorize letters or faces, complete some simple arithmetic tests, and participate in an interaction task. Consider that your final payment will be determined by your performance as a mean of your results on the Memory and Arithmetic tasks and a randomly chosen round of the Interaction task. So, be sure to perform the best that you can each time!

The experiment is completely anonymous. The results of this research may be published, but no individual data will be published.

At some point towards the end of the experiment the red screen will show again. Just wait for the experimenter to pick you up for a glucose measurement when that happens.

Press RETURN to continue..."""


instructPGG1 = """*** Interaction task example ***
You invest 30 DKK and the other participant invests 70 DKK.
So you and the other participant put a total of 100 DKK in the common pool. It is multiplied by 1.5 and becomes 150 DKK.
You keep the remaining 70 DKK (100 - 30 = 70) and get half of the pool. So in total you get 70 + 150/2 = 145 DKK.

In this case, had you invested 0 DKK (nothing), you'd get 135 DKK (and he/she would get 65 DKK).
If you invested 100 DKK you'd earn 127.5 DKK (and he/she would get 167.5 DKK).

Let's do a few examples so you get a feeling for this. Type in the correct answer to the following four questions.

Press RETURN to start quiz..."""


instructPGG2 = """***Try it***

During the experiment you won't get feedback about the other person's investment. Here's a few practice trials for this game.

Press RETURN to start practice..."""



instructCSFaces = """*** Instructions for memory and arithmetic ***

You will be shown a face, an equation, a face, an equation etc. 

Arithmetic: You should complete an arithmetic task as quickly as possible. If you do not answer within 5 seconds, it will count as wrong answer and the task continues. You should use the keys ARROW UP (=correct) or ARROW DOWN (=incorrect) to answer whether the equations are correct or not. E.g. when shown "2*2+4=8 ?" press ARROW UP because it is correct. When shown "6/3-2=2 ?" press ARROW DOWN because it is incorrect.

Faces: In the end, you will have to recall which faces were presented. Use the arrow keys to navigate between 12 options and SPACE to select the faces you saw. Press RETURN when you have finished. You will get points if you select the correct face at the correct location in the sequence. If you forget e.g. what was the first face presented, just choose your best guess.

Press RETURN to start practice..."""

instructCSLetters = """*** Instructions for memory and arithmetic ***

You will be shown a letter, an equation, a letter, an equation etc. 

Arithmetic: You should complete an arithmetic task as quickly as possible. If you do not answer within 5 seconds, it will count as wrong answer and the task continues. Use the keys ARROW UP (=correct) or ARROW DOWN (=incorrect) to answer whether the equations are correct or not. E.g. when shown "2*2+4=8 ?" press ARROW UP because it is correct. When shown "6/3-2=2 ?" press ARROW DOWN because it is incorrect.

Letters: In the end, you will have to recall which letters were presented. Use the arrow keys to navigate between the options and SPACE to select the letters you saw. Press RETURN when you have finished. You will get points if you select the correct letter at the correct location in the sequence. If you forget e.g. what was the first face presented, just choose your best guess.

Press RETURN to start practice..."""


instructExp = """*** Instructions for the real task ***

You will now have to do both the interaction task and the memory/arithmetic task. You start with faces/arithmetic but just before the recall, you will be presented with the first part of the interaction task. After recall you will be shown the second part of the interaction task.

Your payout from the interaction task depends on your average performance on the arithmetic and on face memory. So the better you do, the more you'll earn!

Some trials will be easy. Others will be very hard. Don't worry, just do your best.

Press RETURN to start practice..."""


instructMixed = """*** Experiment begins now! ***

That's it! This is the way the experiment goes. Remember to pay attention to both faces, equations and maximizing your profit from the interaction task. Later today we will pair you with one of the other participants in this room and calculate your profit. So really you're interacting with one of the other participants in this room!

Press RETURN to start the experiment..."""


instructDebriefing = """***Debriefing***

This study was about memory and interaction.

Please go to the counter, where you will state your cpr-number for the payment.

Thank you for your participation!"""

instructControl = """*** Experiment begins now! ***

That's it! This is the way this part of the experiment goes. You will now complete a number of repetitions of this task. Just keep going until you get further instructions on the screen.

Press RETURN to start..."""


"""
SET UP MODULES AND STIMULI
"""

from psychopy import core, visual, gui, monitors, event
import random
import glob
import numpy as np
import re
import string

# Intro-dialogue. Get subject-id and other variables.
# Save input variables in "V" dictionary (V for "variables")
Vexperimenter = {'id':'', 
                 'stimType':['lettersGrid', 'faces'],
                 'span': ['1-7', '1-3', '3-5', '5-7'],
                 'order':['mixed', 'control', 'controlCSFirst'],
                 'timeLimit':[1200, 1, 0]}
if not gui.DlgFromDict(Vexperimenter).OK:
    core.quit()
DIALOGUE = {'age':range(18, 80), 'gender':['male', 'female']}
if not gui.DlgFromDict(DIALOGUE, order=['age', 'gender']).OK:
    core.quit()
DIALOGUE.update(Vexperimenter)
DIALOGUE['timeLimit'] = int(DIALOGUE['timeLimit'])  # from unicode str to integer

if DIALOGUE['stimType'] == 'lettersGrid': encodeFrames = encodeFramesLetters
elif DIALOGUE['stimType'] == 'faces': encodeFrames = encodeFramesFaces

# Translate 
if DIALOGUE['span'] == '1-7': 
    spans = {'experiment': range(1, 8), 'practiceExp': [2, 3, 5], 'controlCS': range(1, 8), 'practiceCS': [2, 3, 4, 6], 'practicePGG': [1], 'controlPGG': [1]}  # span lengths
    repetitions = {'experiment': 4, 'practiceExp': 1, 'practiceCS': 1, 'controlCS': 4, 'practicePGG': 2, 'controlPGG': 28}  # number of repetitions of each trial
elif DIALOGUE['span'] == '1-3': 
    spans = {'experiment': [1,2,3], 'practiceExp': [2, 1, 3], 'controlCS': [1,2,3], 'practiceCS': [1, 2, 3], 'practicePGG': [1], 'controlPGG': [1]}  # span lengths
    repetitions = {'experiment': 16, 'practiceExp': 1, 'practiceCS': 1, 'controlCS': 16, 'practicePGG': 2, 'controlPGG': 48}  # number of repetitions of each trial
elif DIALOGUE['span'] == '3-5': 
    spans = {'experiment': [3,4,5], 'practiceExp': [4, 3, 5], 'controlCS': [3,4,5], 'practiceCS': [3, 4, 5], 'practicePGG': [1], 'controlPGG': [1]}  # span lengths
    repetitions = {'experiment': 9, 'practiceExp': 1, 'practiceCS': 1, 'controlCS': 9, 'practicePGG': 2, 'controlPGG': 27}  # number of repetitions of each trial
elif DIALOGUE['span'] == '5-7': 
    spans = {'experiment': [5,6,7], 'practiceExp': [6, 5, 7], 'controlCS': [5,6,7], 'practiceCS': [5, 6, 7], 'practicePGG': [1], 'controlPGG': [1]}  # span lengths
    repetitions = {'experiment': 6, 'practiceExp': 1, 'practiceCS': 1, 'controlCS': 6, 'practicePGG': 2, 'controlPGG': 18}  # number of repetitions of each trial

# Clocks and time
clock = core.Clock()  # A clock wich will be used throughout the experiment to time events on a trial-per-trial basis (stimuli and reaction times).


# Create psychopy window
myMonitor = monitors.Monitor('testMonitor', width=monWidth, distance=monDistance)  # Create monitor object from the variables above. This is needed to control size of stimuli in degrees.
myMonitor.setSizePix(monitorSizePix)
win = visual.Window(monitor=myMonitor, units='deg', fullscr=True, allowGUI=False, color='black', waitBlanking=False)  # Initiate psychopy Window as the object "win", using the myMon object from last line. Use degree as units!

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



# PGG stimuli
pggImage = visual.ImageStim(win, image='money.jpg', pos=posPGGImage)
pggImage.size *= pggImageSize / max(pggImage.size)
scalePGG = visual.RatingScale(win, low=0, high=scaleTicks, stretch=2, leftKeys='left', rightKeys='right', acceptKeys=keysAns, scale='', tickMarks=[0, 20], tickHeight=2, labels=['Keep all', 'Invest all'], showValue=False, showAccept=False, noMouse=True)
textPGG = visual.TextStim(win, height=textHeight, wrapWidth=12, pos=posTextPGG)

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

    # Fill trialList    
    for rep in range(repetitions[condition]):  # number of repetitions is specified by condition
        for span in spans[condition]:  # span lengths depend on condition
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
                'equationRTs': [],
                'recallAns': '',
                'recallRT': '',
                'recallRTs': [],
                'recallIntersection': '',
                'recallLocation': '',
                'recallSum': '',
                'recallProportion': '',
                'payoutFactor':'',
                
                # PGG stuff (self and other)
                'pggInvest': '',
                'pggInvestOther': '',
                'pggRT': '',
                'pggRTOther': '',
                'pggStart': '',
                'pggStartOther': '',
                
                # General stuff
                'condition': condition,
                'id': DIALOGUE['id'],
                'age': DIALOGUE['age'],
                'gender': DIALOGUE['gender'],
                'stimType': DIALOGUE['stimType'],
                'order': DIALOGUE['order'],
                'pauseRT':''
            }
            
            # Then fill in complex span equations and stimuli
            if condition in ('experiment', 'practiceCS', 'practiceExp', 'controlCS'):
                for i in range(span):
                    # Use all stimuli before loading anew to prevent too much interference from recall to recall.
                    if not len(allStims):
                        if DIALOGUE['stimType'] == 'faces': allStims = glob.glob('faces/*.bmp')
                        elif DIALOGUE['stimType'] == 'lettersGrid': allStims = list(string.ascii_uppercase)
                    trial['encode'] += [allStims.pop(random.randint(0, len(allStims) - 1))]  # a random element without replacement
                    equation, correct = makeEquation()
                    trial['equationText'] += [equation]
                    trial['equationCorrect'] += [correct]
            
            # Add this trial to trialList
            trialList += [trial]
            
            
    
    # Only randomize if this is not an instruction
    if condition not in ('practiceCS', 'practiceExp'):
        trialList = random.sample(trialList, len(trialList))
    
    # Add trial numbers and return
    for i, trial in enumerate(trialList):
        trial['no'] = i + 1  # start at 1 instead of 0
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

def experimenterIntervention(trialsLeft=0):
    """ Wait for experimenter and secret key """
    win.color = 'red'
    win.flip()
    
    trialsLeftText = '\n\n' + str(trialsLeft) + ' trials remaining.' if trialsLeft else ''
    ask('The experimenter will come pick you up for a glucose measurment now.\nPlease wait...' + trialsLeftText, keyList=keysLockscreen)
    win.color = 'black'
    win.flip()

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
    trial = makeTrials('practicePGG')[0]  # data structure to save to
    trial['condition'] = condition
    trial['recallRT'] = scaleQ.getRT()
    trial['recallAns'] = scaleQ.getRating()
    writerTrials.write(trial)

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
    trial = makeTrials('practicePGG')[0]  # data structure to save to
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
        response = event.waitKeys()
        eventOnKey(response[0])  # exit or go to questions
        if response[0] in special.keys():  # recode special keys/characters
            response[0] = special[response[0]]
        
        if response[0] == 'backspace':
            currentAnswer = currentAnswer[:-1]  # delete last character
        elif response[0] in keysAns and currentAnswer != '':  # an attempt
            # Pressed too early, ignore
            if clock.getTime() < preventDoubleClickTime:
                continue
            # If there is no correct answer, just record response
            trial['recallRT'] = clock.getTime()
            trial['recallAns'] = currentAnswer
            #trial = {'id':DIALOGUE['id'], 'condition': condition, 'question': text.replace('\n', ' '), 'answer': currentAnswer, 'rt': clock.getTime()}
            if corrAns == None:
                writerTrials.write(trial)
                break
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
        elif response[0] in acceptedChars:  # normal characters
            currentAnswer += response[0]


"""
RUN FUNCTIONS
"""

def runCSEncode(trial):
    """ Yeah, that's what it does. """
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
        for frame in range(encodeFrames):
            stim.draw()
            win.flip()
        
        # Present processing task
        textEquation.draw()
        win.flip()
        clock.reset()
        response = event.waitKeys(keyList=keysEquation.keys()+keysQuit+keysQuestionnaire, maxWait=maxWaitEquations)
        if response:  # we got a response! Score trial
            eventOnKey(response[0])  # exit or go to questions
            trial['equationRTs'] += [clock.getTime()]  # record reaction time
            trial['equationAnss'] += [keysEquation[response[0]]]
            trial['equationScores'] += [int(keysEquation[response[0]] == trial['equationCorrect'][i])]
        else:  # Feedback if timeout is reached
            trial['equationRTs'] += ['timeout']
            trial['equationAnss'] += ['timeout']
            trial['equationScores'] += [0]  # scored as wrong answer
            askFeedback('Too slow!', 'red', wait=waitFeedback)
        
    trial['equationScore'] = sum(trial['equationScores'])
    return trial
    
def scoreCS(trial):
    """Given trial['encode'] and trial['recallAns'], score trial."""
    intersection = [encode for encode in trial['encode'] if encode in trial['recallAns']]
    location = [encode for i, encode in enumerate(trial['encode']) if encode == trial['recallAns'][i]]  # for every stim presented, check if it corresponds to the answer given.
    
    trial['recallIntersection'] = len(intersection)
    trial['recallLocation'] = len(location)
    trial['recallSum'] = len(intersection) + len(location)
    trial['recallProportion'] = (len(intersection) + len(location)) / float(2 * trial['span'])
    
    trial['payoutFactor'] = trial['equationScore'] / float(trial['span']) * trial['recallProportion']
    
    return trial    

"""
def runCSRecallType(trial):
    letterCoordsX = (range(trial['span']) - np.mean(range(trial['span']))) * letterSpacing
    
    currentAnswer = [''] * trial['span']  # empty texts for all spans
    selectedCurrent = 0
    win.callOnFlip(clock.reset)
    # Collect responses
    while True:
        # Draw current answer and labels
        for i in range(trial['span']):
            # Label
            letterLabel.pos = [letterCoordsX[i], 0] + np.array(letterLabelPos)
            letterLabel.text = i + 1
            letterLabel.draw()
            
            # Background and letter
            letterField.pos = [letterCoordsX[i], 0]
            letterField.draw()
            
            letterStim.pos = [letterCoordsX[i], 0]
            letterStim.text = currentAnswer[i]
            letterStim.draw()
            
            # Frame
            letterSelectFrame.pos = [letterCoordsX[selectedCurrent], 0]
            letterSelectFrame.draw()
        
        # Show it!
        textRecall.text = 'Write the letters in their correct position in the presented sequence. Press RETURN to submit answer.'
        textRecall.draw()
        win.flip()
        
        # Collect response
        response = event.waitKeys(keyList=list(string.ascii_lowercase) + keysAns + keysRecallLetters.keys() + keysQuit)  # all letters plus these other keys
        if response[0] in keysQuit:
            return  # exit function
        # Move current location
        elif response[0] in keysRecallLetters.keys():
            if 0 <= selectedCurrent + keysRecallLetters[response[0]] < trial['span']:
                selectedCurrent += keysRecallLetters[response[0]]
        # A letter!
        elif response[0] in string.ascii_lowercase:
            currentAnswer[selectedCurrent] = response[0].upper()  # set value, uppercase
            if selectedCurrent < trial['span'] - 1:
                selectedCurrent += 1  # advance when a letter is entered, unless it's the last letter
        # Submit answer without omissions
        elif response[0] in keysAns and '' not in currentAnswer:
            trial['recallRT'] = clock.getTime()
            trial['recallAns'] = currentAnswer
            
            # Score trial and break out of while loop (finished OSPAN)
            trial = scoreCS(trial)
            break
    
    return trial
"""

def runCSRecallGrid(trial):
    """Recall faces"""
    # PREPERATION
    # Create a list "itemOptions of items to show as response options. All of targets + some fill-ins
    if trial['stimType'] == 'faces': allItems = faceFiles
    elif trial['stimType'] == 'lettersGrid': allItems = string.ascii_uppercase
    itemFillin = list(set(allItems) - set(trial['encode']))  # allowable fill-in response options (non-targets)
    itemFillin = random.sample(itemFillin, len(gridCoords) - trial['span'])  # selected fill-in items
    itemOptions = trial['encode'] + itemFillin  # items for this answer
    if trial['stimType'] == 'faces': itemOptions = random.sample(itemOptions, len(itemOptions))  # randomize order
    if trial['stimType'] == 'lettersGrid': itemOptions.sort()  # sort alphabetically
    
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
    selectedCurrent = len(itemOptions) / 2  # placeholder for current selected face. Starts in center
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

def runPGGSelf(trial):
    textPGG.text = 'How much money will you invest in the common project?'
    trial['pggStart'] = random.randint(0, scaleTicks)  # record location of marker
    
    scalePGG.reset()
    scalePGG.setMarkerPos(trial['pggStart'])  # random marker position
    while scalePGG.noResponse:
        pggImage.draw()
        textPGG.draw()
        scalePGG.draw()
        win.flip()

    trial['pggInvest'] = scalePGG.getRating() * pggAmount / scaleTicks    # convert from tick-number to corresponding money-value
    trial['pggRT'] = scalePGG.getRT()
    
    return trial

def runPGGOther(trial):
    trial['pggStartOther'] = random.randint(0, scaleTicks)  # record location of marker
        
    scalePGG.reset()
    scalePGG.setMarkerPos(trial['pggStartOther'])  # random marker position
    while scalePGG.noResponse:
        pggImage.draw()
        textPGG.draw()
        scalePGG.draw()
        win.flip()
        
        # Feedback
        payoutFactor = trial['payoutFactor'] if trial['payoutFactor'] else 1  # this is not set during training. Just use 100 %
        hypotheticalProfit = ((trial['pggInvest'] + scalePGG.getRating()) * 1.5 / 2 + (pggAmount - trial['pggInvest'])) * payoutFactor
        textPGG.text = """How much do you think the other participant invested in the last round? Use the scale to see your possible outcome in this round.
        Your hypothetical profit: %.2f kr.""" %hypotheticalProfit
        
    trial['pggInvestOther'] = scalePGG.getRating() * pggAmount / scaleTicks  # convert from tick-number to corresponding money-value
    trial['pggRTOther'] = scalePGG.getRT()
    
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
    taskTime = 0  # how much time has been spent on tasks
    timeLimitPassed = False  # has experimenterInttervention() been shown?
    
    
    """Run a block of trials """
    if condition == 'experiment': loops = range(1, 1000)
    else: loops = range(1)
    for loop in loops:  # just keep going until we hit a return
        trialList = makeTrials(condition)
        for trialNumber, trial in enumerate(trialList):
            clock.reset()
            ask('press RETURN to start next round...')
            trial['pauseRT'] = clock.getTime()
            
            # Task flow depends on condition
            if trial['condition'] in ('experiment', 'practiceExp'):
                trial = runCSEncode(trial)
                trial = runPGGSelf(trial)
                trial = runCSRecallGrid(trial)
                trial = runPGGOther(trial)
            
            elif trial['condition'] in ('practiceCS', 'controlCS'):
                trial = runCSEncode(trial)
                trial = runCSRecallGrid(trial)
                
            elif trial['condition'] in ('practicePGG', 'controlPGG'):
                trial = runPGGSelf(trial)
                trial = runPGGOther(trial)
                        
            # Save trial or quit this block
            if trial == None:
                return
            else:
                saveTrial(trial)
                
            # Return of time's up and trials are completed. Continue otherwise.
            if condition == 'experiment' and DIALOGUE['timeLimit'] and not timeLimitPassed:
                trial['equationRTs'] = [RT if RT != 'timeout' else maxWaitEquations for RT in trial['equationRTs']]  # replace "timout" with maxWaitEquations just for this purpose. Trials is saved so we can just edit the trial since it won't be used beyond this point
                taskTime += trial['span'] * encodeFrames / 60 + sum(trial['equationRTs']) + trial['recallRT'] + trial['pggRT']  # assuming 60Hz
                if taskTime > DIALOGUE['timeLimit']:
                    trialsLeft = len(trialList) - trialNumber if loop ==1 else 0
                    experimenterIntervention(trialsLeft)
                    timeLimitPassed = True
                    if loop > 1:
                        return
        
        # At least 1 loop is finished now so trials are completed. Return if time's up (continue otherwise)
        if DIALOGUE['timeLimit'] and taskTime > DIALOGUE['timeLimit']:
            return
    

def runInstructPGG():
    """ 
    Instruct the Public Goods Game.
    Require a few different phases so it's nicer to pack it together in a function.
    """
    ask(instructPGG1)
    
    askType("""***Check 1***
    
    if you invest 0 DKK 
    and the other participant invest 0 DKK, 
    how much would you earn? (type)""", corrAns='100', acceptedChars='ints', condition='qCheck1', feedbackWrong='Wrong! Correct answer is 100.\n')
    askType("""***Check 2***
    
    if you invest 0 DKK 
    and the other participant invest 100 DKK, 
    how much would you earn? (type)""", corrAns='175', acceptedChars='ints', condition='qCheck2', feedbackWrong='Wrong! Correct answer is 175.\n')
    askType("""***Check 3***
    
    if you invest 100 DKK 
    and the other participant invest 100 DKK, 
    how much would you earn? (type)""", corrAns='150', acceptedChars='ints', condition='qCheck3', feedbackWrong='Wrong! Correct answer is 150.\n')
    askType("""***Check 4***
    
    if you invest 100 DKK 
    and the other participant invest 0 DKK, 
    how much would you earn? (type)""", corrAns='75', acceptedChars='ints', condition='qCheck4', feedbackWrong='Wrong! Correct answer is 75.\n')
    ask(instructPGG2)
    runBlock('practicePGG')


def runInstructCS():
    if DIALOGUE['stimType'] == 'faces': ask(instructCSFaces)
    elif DIALOGUE['stimType'] == 'lettersGrid': ask(instructCSLetters)
    runBlock('practiceCS')


def questionsAndQuit():
    """ 
    Show quesitons and quit. Meant to be called from anywhere in case something 
    breaks or time is up 
    """
    # Questions
    ask("Almost finished! The memory and interaction task is done. Now you should fill in a short questionnaire, before you are done with the experiment.")
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
if DIALOGUE['timeLimit']:
    experimenterIntervention()
ask(instructWelcome)
  
if DIALOGUE['order'] == 'mixed':
    # Practice PGG, CS and both
    #runInstructPGG()
    #runInstructCS()
    ask(instructExp)
    runBlock('practiceExp')
    
    # Both - experiment!
    ask(instructMixed)
    runBlock('experiment')
    
elif DIALOGUE['order'] == 'control':
    # PGG: instruct, practice, experiment
    runInstructPGG()
    ask(instructControl)
    runBlock('controlPGG')
    
    # CS: instruct, practice, experiment
    runInstructCS()
    ask(instructControl)
    runBlock('controlCS')
elif DIALOGUE['order'] == 'controlCSFirst':
    # CS: instruct, practice, experiment
    runInstructCS()
    ask(instructControl)
    runBlock('controlCS')
    
    # PGG: instruct, practice, experiment
    runInstructPGG()
    ask(instructControl)
    runBlock('controlPGG')
    
questionsAndQuit()