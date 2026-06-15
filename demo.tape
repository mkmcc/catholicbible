# script to make a demo video for the 'bible' CLI
# run `vhs bible.tape`

# basic config
Set Shell zsh

Set Width 900
Set Height 600

Set FontSize 14
Set FontFamily "Menlo"
Set Theme "iTerm2 Solarized Dark"

Set Margin 24
Set MarginFill "#ACD7C5"

Set WindowBar Colorful
Set Padding 18
Set BorderRadius 12

Set TypingSpeed 55ms
Set Framerate 24

Output bible_demo.mp4
Output bible_demo.gif

Env EMACS "/Applications/Emacs.app/Contents/MacOS/Emacs"

##########################################################################

# 1. Show the Help Menu
Type "# Let's check the available options..."
Sleep 1.5s
Ctrl+U

Type './bible -h'

Sleep 500ms
Enter

# Give the viewer time to read the usage instructions
Sleep 4s

# Clear the screen
Ctrl+L
Sleep 500ms

##########################################################################

# 2. Fetch Daniel verses
Type "# Fetch Daniel 7:13-15 in the Knox translation, formatted as LaTeX..."
Sleep 2s
Ctrl+U

Type './bible --emacs=${EMACS} knox Daniel 7 13-15 --format latex'

Sleep 700ms
Enter

# Give the viewer time to read the LaTeX output
Sleep 10s

# Clear the screen
Ctrl+L
Sleep 500ms

##########################################################################

# 3. Fetch Job verses
Type "# Now let's grab a longer passage from Job..."
Sleep 1.5s
Ctrl+U

Type `./bible --emacs=${EMACS} douay_rheims Job 19 23-37 --format text | sed 's/(/\n(/g'`

Sleep 700ms
Enter

# Wait a bit longer for the user to read the larger block of text
Sleep 10s

# Clear the screen
Ctrl+L
Sleep 500ms

##########################################################################

# 4. Fetch Deuteronomy verses
Type "# Now let's grab a poem from Deuteronomy..."
Sleep 1.5s
Ctrl+U

Type `./bible --emacs=${EMACS} esv Deuteronomy 32 8-9`

Sleep 700ms
Enter

# Wait a bit longer for the user to read the larger block of text
Sleep 10s
