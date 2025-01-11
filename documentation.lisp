(in-package #:org.shirakumo.text-draw)

;; toolkit.lisp
(docs:define-docs
  (function wrap
    "Perform line wrapping on a block of text.

Returns a list of text lines.
Wrapping is performed on every Linefeed and Return, and if the current
line exceeds WIDTH number of characters. In the latter case, a line is
wrapped at the last whitespace position if there is one within the
current line, or at the width if there is not. Whitespace excluding
the non-breaking space is trimmed from ends and starts of lines.")
  
  (function alignment
    "Returns the number of characters to insert to achieve the desired alignment.

Returns a cons with the characters on the left and right hand sides of
the line to insert.

ALIGNMENT can be one of :LEFT :RIGHT :MIDDLE :CENTER.")
  
  (function width
    "Returns the width of the text block respecting line feeds.")
  
  (function lines
    "Returns a list of lines from the split text."))

;; styles.lisp
(docs:define-docs
  (function style
    "Returns the text transformed into the given style.

STYLE can be one of:

 :sans
 :bold
 :italic
 :bold-italic
 :serif
 :serif-bold
 :serif-italic
 :serif-bold-italic
 :script
 :script-bold
 :fraktur
 :fraktur-bold
 :monospace
 :doublestruck

Note that by far not all characters support all of these styles. The
text is transformed on a best-effort basis.")

  (function background
    "Return a background character.

TYPE can be one of:
  :TRANSPARENT
  T :WHITE
  :BLACK
  :DARK-GRAY
  :GRAY
  :LIGHT-GRAY")

  (function arrow
    "Return an arrow character.

DIR can be one of 
  :LEFT :WEST
  :RIGHT :EAST
  :UP :NORTH
  :DOWN :SOUTH
  :UP-DOWN :NORTH-SOUTH
  :LEFT-RIGHT :EAST-WEST

TYPE can be one of:
  :HEAD
  :LIGHT-HEAD
  :EMPTY-HEAD
  :TRIANGLE
  :LIGHT
  T :NORMAL
  :HEAVY
  :LARGE
  :VERY-HEAVY
  :DOUBLE
  :CIRCLE
  :FULL-CIRCLE
  :DIAMOND
  :FULL-DIAMOND"))

;; draw.lisp
(docs:define-docs
  (function fill
    "Fill a block with a background pattern.

STREAM should be a format stream designator.

See BACKGROUND")
  
  (function table
    "Format tabular data.

The TABLE should be a list of lists, with each inner list being a row
and each row having the same number of columns.

BORDERS designates whether borders should be drawn around the cells or
not.

PADDING sets the number of spaces to surround each cell's contents by.

STREAM should be a format stream designator.")
  
  (function tree
    "Format a tree structure.

ROOT is the first element at the root. CHILDREN-FUN should be a
function of one argument (a node in the tree) that returns a SEQUENCE
of child nodes for that node.

KEY should be a function that takes a node and whose return value is
used to print the node's entry in the tree.

MAX-DEPTH should be the maximum depth to which the tree is printed.
Entries at a depth below that are abbreviated via three dots. If NIL
or T are passed, no max depth is applied.

STREAM should be a format stream designator.")
  
  (function node
    "Print a flow chart node with the given input and output ports.

Both INPUTs and OUTPUTs should be lists of port entries. Each port
entry should either be the name of the port or a cons consisting of
the name and the value connected at that port.

LABEL if given designates the label in the center of the node to
print.

BACKGROUND designates the background of the node box.

STREAM should be a format stream designator.

See BACKGROUND")
  
  (function box
    "Surrounds the TEXT with a box.

WIDTH designates the width of the box. If NIL or T the box fits the
given text block. If the text is made up of lines that exceed the
width of the box, the text is wrapped automatically.

ALIGN designates how the text block is aligned within the box.

BACKGROUND designates the background of the box.

STREAM should be a format stream designator.

See ALIGNMENT
See BACKGROUND
See WRAP")
  
  (function align
    "Aligns a text block to produce a fixed width block of lines.

ALIGNMENT designates how the text block is aligned within the box.

STREAM should be a format stream designator.

See ALIGNMENT")
  
  (function rows
    "Fuse a couple of parts together via new lines.

Returns a new string with each PART on a line.")
  
  (function progress
    "Prints a simple progress bar filled to PERCENTAGE.

If the percentage is outside of [0,100] then the bar is either
entirely empty or entirely full.

When LABEL is T, some of WIDTH is taken up to print the percentage as
a number label.

STREAM should be a format stream designator.")
  
  (function pad
    "Pads the text block on all sides by SIZE.

SIZE may either be an integer, or a list designating the padding to
add on each side in order of LEFT TOP RIGHT BOTTOM.

BACKGROUND designates the fill character of the padding.

STREAM should be a format stream designator.

See BACKGROUND")
  
  (function check
    "Generates a simple checkbox.

If LABEL is given it is printed next to the checkbox.

STREAM should be a format stream designator.")
  
  (function radio
    "Generates a simple radio button

If LABEL is given it is printed next to the radio button.

STREAM should be a format stream designator.")
  
  (function horizontal-line
    "Generates a horizontally oriented line.

START and END if given designate the arrowheads to draw at either end
of the line.

BEND can be one of :LEFT :START :RIGHT :END :MIDDLE :CENTER and
designates where in the line a bend should be generated when HEIGHT is
greater than one.

STREAM should be a format stream designator.

See ARROW")
  
  (function vertical-line
    "Generates a vertically oriented line.

START and END if given designate the arrowheads to draw at either end
of the line.

BEND can be one of :TOP :START :BOTTOM :END :MIDDLE :CENTER and
designates where in the line a bend should be generated when WIDTH is
greater than one.

STREAM should be a format stream designator.

See ARROW")
  
  (function line
    "Generates a line.

If the width is greater or equal to the height a horizontal line is
generated, otherwise a vertical one.

See HORIZONTAL-LINE
See VERTICAL-LINE")
  
  (function translate
    "Translates the given string block by X and Y.

This is the same as adding padding on the respective sides of the
block.

STREAM should be a format stream designator.")
  
  (function composite
    "Composites two text blocks together.

A-OFFSET and B-OFFSET should be conses of X and Y offsets to apply to
the respective blocks before compositing.

When compositing, B is overlaid on top of A in such a way that any
character in B takes precedence unless that character is a whitespace
character that isn't a non-breaking space.

Effectively this means that the non-breaking space is considered
\"opaque\", and all other white space is \"transparent.\"

STREAM should be a format stream designator.")
  
  (function compose
    "Convenience shorthand to composite shapes together.

Each PART should be a list composed of the X and Y offset and the
string of the block to compose. Eg:

  (compose T
    (0 0 (fill 20 11))
    (6 4 (box \"Hello\")))

STREAM should be a format stream designator."))
