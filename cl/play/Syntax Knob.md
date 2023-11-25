# I Wish I Had a Syntax Knob

(format nil "~a.~a" (owners-name owner) name)

I wish that I had an editor that allowed me to write:

(format nil "~a.~a" <mark>owner's name</mark> name)

and, when I hover the mouse over the highlighted variable, I get a slider or a rotary knob.

![[Syntax Knob Photo.jpg]]

When the knob is set to 0, I get the above.

When the knob is turned to 5, I see:

(format nil "~a.~a" <mark>eh.name</mark> name)

When the knob is turned to 10, I see:
```
(format nil "~a.~a" (slot-value eh 'name) name)
```

When the knob is over-turned to 11, I get Assembler.  If I keep going, I get de Bruijn indexing, I get Forth, I get lambda calculus, etc., etc.

Somewhere - well out of the way - I am allowed to define a table, like

| phrase | synonym |
| -------------- | ----------- |
| owner's name | eh.name |
| *x*.*y* | (slot-value '*y* *x*) |

In the best of all worlds, setting up the table is done via some "easy" UX.  E.g. highlight the phrase, get a popup to enter the synonym (uh, what UX is used for entering generic synonym transforms like "*x*.*y*"?)

# Appendix - attribution 
https://www.flickr.com/photos/dejankrsmanovic/29451540498/

