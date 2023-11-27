# 0D - Zero Dependencies - and Drawware

Convert diagrams in src/*.drawio to executable code.

Parse a given diagram, with given syntax rules, and emit the diagram as JSON.

Inhale the JSON and execute.

# Details

This version uses the Odin language internally.

More languages are expected.

Fundamentally, internal language doesn't matter when designing a solution to a problem.

If you need to optimize the solution (a Big If), then internal language and niggly details do matter.

# Syntax Rules 

# DaS - Diagrams as Syntax

I use the name "DaS" to mean any use of diagrams instead of text for writing programs.

Note that most diagrams include text, but are not exclusively text.

Text is better for expressing *some* things, like equations.

Diagrams are better for expressing *some* things, like control flow.

It is better to draw diagrams in way that layered and structured.  My advice is to follow the Rule of 7, and to build solutions as layers of diagrams within diagrams - Containers down to Leaves. Containers can contain Containers and/or Leaves. Leaves contain code (Currently textual code. In the future, maybe Leaf components will contain StateCharts and Drakon diagrams).

# Drawware

Drawware is software written in diagram form.

Drawware uses DaS.

The language designer(s) makes up the Rules for what symbols may be used and what they mean.

## What are the Differences Between Textual Syntax and Diagrammatic Syntax?
### Stretching, Resizing
  - Syntactic elements can be stretched and resized
	- e.g. a rectangle can be resized, yet, still be recognized as a rectangle
  - vector
  - in contrast, syntactic elements in textual languages tend to be little, fixed-sized bitmaps
  - zooming
	- diagrams can be zoomed, whole diagrams can be resized
	- we see some of this kind of thing in modern text editors - they give a detailed view of the code in one window, and an overall view of most of the code, shrunk down, in another window
		- modern text windows can be zoomed in and out using command+ and command- keys, thinking of code in DaS terms just makes this kind of thing more obvious

### Overlap
  - syntactic elements of DaS can overlap (like windows in windowing systems) 
  - in contrast, syntactic elements of TaS (Text as Syntax) tend to be arranged in grids of non-overlapping cells (in general, characters do not overlap other characters) - a hold-over from the days of EBCDIC and ASCII

### Declarative vs. Sequential
  - most TaS languages (C, Python, Rust, etc.) tend to be read left-to-right, top-to-bottom
  - there are *some* TaS languages that are declarative - PROLOG, HTML, etc. - these imply no reading / execution ordering
  - Lisp has a "recursive syntax" - you read Lisp inside then out
  - sequential syntax is a hold-over from the EE design of CPUs 
	- CPUs are but bits of electronics that sequentially process commands
	- CPUs are meant for single threads (global, mutable, shared stack, global, mutable, shared instruction pointer, global, shared, mutable RAM, etc.) 
		- single-threaded CPUs can be forced to operate in multi-threaded mode, but, this requires extra work and extra code
		- in the 1950's CPUs were expensive and Memory was scarce, hence, it made sense to squeeze blood out of CPUs and trade off programmer-time for cost of CPUs and Memory
		- in 2023, CPUs are no longer expensive (Arduinos, rPIs, etc.) and Memory is abundant, yet, we needlessly continue to use methods, like time-sharing and garbage collection and worrying about memory allocation and we spend copious amounts of programmer time to preserve resources that are no longer in short supply
		
