# 0D - Zero Dependencies - and Drawware

0D compiler compiles diagrams in `src/*.drawio` to code and runs the code.

## Steps
1. Parse diagrams, with given syntax rules, and exhale the diagrams as JSON.
2. Inhale the JSON and execute.

# Details

This version uses the Odin language internally.

More languages are expected.

Fundamentally, internal language doesn't matter when designing a solution to a problem.

If you need to optimize the solution (a Big If), then internal language and niggly details do matter.  Note that optimization (e.g. type checking, etc.) reduces scalability, hence, should be used sparingly.

# Syntax Rules 

This section describes the "syntax" used in our compilable diagrams.

## Overview
In general, the diagrams display a software program as a set of Components interconnected by Connection arrows.

There are two major classes of Component:
- Container
- Leaf.

Containers are drawings that can contain Components and Connections.  Containers can contain other Container components and/or Leaf components.  In `draw.io`, we represent each Container as a separate drawing on a separate *tab* in the `draw.io` editor.

Leaves represent code.  Leaves are represented on diagrams as dark blue rectangles.  The actual code is in other files. In this case, most of the Leaves are in `leaf0d/leaf0d.odin`.  The code must provide two entry points
1. instantiate
2. handle

and, typically, uses the *send* function to create output messages.

Components are *templates* (similar to *classes*).  Components can be instantiated multiple times in a project.  Each instantiation is unique and has a blob of unique storage (state) associated with it (similar to *self* in class-based languages).

Templates have unique names.  Instances do not need to be explicitly named. The underlying 0D engine guarantees that each instance is unique (in a manner similar to *references* to *objects* in class-based languages).

Components had input and output *ports*.  Input ports are drawn as white pills (small rounded rectangles), and output ports are drawn as dark-blue pills.  Each port has a unique name.  The names are scoped within their parent component.  The scope of input port names is unique from the scope of output port names within the same component, i.e. a component can use the same name for an input port as for an output port, but every input name must be different from every other input name and every output name must be different from every other output name within the same component.  Different components can use the same names for ports as other components, for example, many components have input ports named "input".  Those port names do not clash, they are "locally scoped" to their parent components.

Connections within Containers are used to hook output ports to input ports of child components in a 1:many and many:1 manner.

Containers also have special ports called *gates*.  Gates represent the top-level inputs and outputs of a Container.  Input gates are drawn as white rhombuses with a name, and, output gates are drawn as blue rhombuses with a name.

Components can only *send* messages to their own output ports and gates.  This is similar to *input parameter lists* in other programming languages, except that the parameter lists are for *outputs* (the set of input ports are like input parameter lists, the set of output ports are like output parameter lists).  In other programming languages, a function cannot know where a particular input parameter came from.  Likewise, a component cannot know where a particular output will go.

Unlike procedures and functions in most programming languages, inputs to and outputs from a component can happen at any time, in any order.  For example, if a component has two inputs A and B, it might be the case that inputs arrive on port B, and, some time later, inputs arrive on port A, or, the inputs might arrive very closely spaced together in time, or, multiple B inputs might arrive before any A inputs arrive, or, inputs *never* arrive on port A, and so on.

It turned out to be trivial to represent shell commands as Leaf Components.  This repo includes a demo of such components.  We call this VSH - for Visual SHell.  It is possible to draw shell pipelines in `draw.io` and to execute the pipelines.  Combinations beyond simple pipelines are possible to express and are easier to express visually than as pure text.  [*Aside: an outcome of this approach is that it is convenient to visually express component programs that contain relatively heavy concepts.  For example, a parser can be drawn and implemented as a single component with input ports and output ports.  We are considering making A.I. and LLM components.*]
## Visual Overview
### Visual Syntax
- bare component
- Container
- Leaf
- input gate
- output gate 
- input port
- output port
- connection
- VSH component
- comment

#### Notes
- "VSH" means Visual SHell (like a `draw.io` version of */bin/bash*, but simplified).
- *gates* and *ports* are similar, except that *gates* represent inputs and outputs of drawings, where *ports* represent in/out ports of components inside diagrams.
- Containers can contain other Containers or Leaves, Leaves are at the *bottom* and contain nothing but code.
- these diagrams can be found in ../DPL syntax/DPL syntax.drawio.
### Visual Semantics
- down
- up
- across
- through
- fan-out (split)
- fan-in (join)

### Style, Readability
- opacity
- line style
- line thickness

### Idioms
- feedback
- sequential
- parallel
- concurrency
- errors, exceptions

### Low Level Technicalities
- kickoff inject
- handle ()
- send ()
- message copying
- active and idle
- notes

## Visual Syntax
### bare component
![](../DPL%20syntax/DPL%20syntax-bare%20component.drawio.svg)
### Container
![](../DPL%20syntax/DPL%20syntax-Container.drawio.svg)
### Leaf
![](../DPL%20syntax/DPL%20syntax-Leaf.drawio.svg)
### input gate
![](../DPL%20syntax/DPL%20syntax-input%20gate.drawio.svg)
### output gate 
![](../DPL%20syntax/DPL%20syntax-output%20gate.drawio.svg)
### input port
![](../DPL%20syntax/DPL%20syntax-input%20port.drawio.svg)
### output port
![](../DPL%20syntax/DPL%20syntax-output%20port.drawio.svg)
### connection
![](../DPL%20syntax/DPL%20syntax-connection.drawio.svg)
### VSH component
![](../DPL%20syntax/DPL%20syntax-vsh%20component.drawio.svg)
### comment
![](../DPL%20syntax/DPL%20syntax-comment.drawio.svg)

## Visual Semantics
### down
![](../DPL%20syntax/DPL%20syntax-down.drawio.svg)
### up
![](../DPL%20syntax/DPL%20syntax-up.drawio.svg)
### across
![](../DPL%20syntax/DPL%20syntax-across.drawio.svg)
### through
![](../DPL%20syntax/DPL%20syntax-through.drawio.svg)
### fan-out (split)
![](../DPL%20syntax/DPL%20syntax-fan-out%20(split).drawio.svg)
### fan-in (join)
![](../DPL%20syntax/DPL%20syntax-fan-in%20(join).drawio.svg)
![[DPL syntax-fan-in (join).drawio.svg]]

## Style, Readability
### opacity
![](../DPL%20syntax/DPL%20syntax-opacity.drawio.svg)
### line style
![](../DPL%20syntax/DPL%20syntax-line%20style.drawio.svg)
### line thickness
![](../DPL%20syntax/DPL%20syntax-line%20thickness.drawio.svg)

## Idioms
### feedback
![](../DPL%20syntax/DPL%20syntax-feedback.drawio.svg)
### sequential
![](../DPL%20syntax/DPL%20syntax-sequential.drawio.svg)
### parallel
![](../DPL%20syntax/DPL%20syntax-parallel.drawio.svg)
### concurrency
![](../DPL%20syntax/DPL%20syntax-concurrency.drawio.svg)
### errors, exceptions
![](../DPL%20syntax/DPL%20syntax-errrors,%20exceptions.drawio.svg)

## Low Leval Technicalities
### kickoff inject
![](../DPL%20syntax/DPL%20syntax-kickoff%20inject.drawio.svg)
### handle ()
![](../DPL%20syntax/DPL%20syntax-handle().drawio.svg)
### send ()
![](../DPL%20syntax/DPL%20syntax-send().drawio.svg)
### message copying
![](../DPL%20syntax/DPL%20syntax-message%20copying.drawio.svg)

### active and idle
![](../DPL%20syntax/DPL%20syntax-active%20and%20idle.drawio.svg)
### notes
![](../DPL%20syntax/DPL%20syntax-notes.drawio.svg)

# Examples
## Basics
Demo of writing 0D components manually in code, not using a drawing editor.

The demo `demo_basics/main.odin` Defines a Leaf component template named "echo".

The demo defines a Container component called "Top" which contains
1. two instances of the "echo" Leaf component hooked up in a sequential combination.
2. two instances of the "echo" Leaf component hooked up in a parallel combination.

The instances in this demo are given unimaginative names like "10", "11", "20", and, "21".  When you build these things with a drawing compiler, you don't need to give the instances names at all since (x,y) position on a diagram is enough to uniquely identify each component instance.  
## Drawio
This demo `demo_drawio/main.odin` is of drawings compiled to running code.

See `../src/demo_drawio.drawio` for the example drawing used in this demo.

We happen to use `draw.io` for editing diagrams, but, other editors, like Excalidraw, can be considered.  We don't believe that it is worth the effort to build new drawing editors (yet), since the people behind `draw.io` and `Excalidraw` have much more experience in doing so.

The demo basically mimics the functionality of the *Basics* demo, but, using source code in src/demo_vsh.drawio.  This source code uses visual syntax for programming instead of text-only syntax.

The demo runs 3 examples:
1. sequential arrangement of components
2. parallel arrangement of components
3. handling a component that takes a "long time" to finish its work - this would be the way to do fancy I/O or to connect to existing code (but, using VSH is the preferred way for connecting to existing code).  In this demo, the long-running component is faked by making a count-down component.  It doesn't do anything useful, but, it demonstrates how to deal with these kinds of issues.

This demo improves on the hand-built demo by parsing the command line and building code (Odin at this time) from diagrams supplied by the programmer.
## VSH

The demo `demo_vsh/main.odin` uses draw.io diagrams to build a simple, but useless, parallel pipeline that employs several command-line utilities written in some language other than Odin. We guess that the utilities were written in C, but we don't know for sure, nor do we care.  

The utilities are `ps`, `grep` and `wc`.

See `../src/demo_vsh.drawio` for the drawing.

It is customary to use these utilities in a command-line shell, like `/bin/bash` using a shell pipeline.  In this demo, we show that the pipeline can be expressed as a diagram that shows parallel operation of the utilities, in a diagrammatic form.

## Dev0D

This demo is basically the same as the above VSH demo, but the code `demo_dev0d/main.odin` uses and defines extra utility functions that can be used to debug this prototype code and demos.  These utility functions are mostly interesting to developers who still need to work in the text-only paradigm while debugging this Proof of Concept.  The utility functions are not needed by drawware programmers, as it is easier to debug programs using drawings instead of textual code.

## Points of Interest

### Das2json

This version of 0D works in two main passes:
1. It converts a draw.io package of drawings (one .drawio file, containing many possible tabs, each containing a drawing) to JSON.
2. It reads the generated JSON and runs the program (by interpreting the information contained in the JSON file.).

Step (1) uses the code in `das2json/*.odin`, while step (2) uses the code in `odin/*/*.odin`.

It is expected that we will be able to replace step (2) with programs written in different languages, like Python, JavaScript, Common Lisp, etc., but, we haven't done so yet.  Ideally, we might rewrite step (2) in a meta language (currently called 'RT' - recursive text) that can then exhale code in Python, JavaScript, Common Lisp, etc., alleviating programmers from porting step (2) code into the various languages manually.  We haven't done this yet.

### Message and Datum

Message is defined as a *3-tuple*:
1. tag (some sort of id, in the Odin implementation, the id is a *string*)
2. data
3. cause.

Tag is some sort of id. In this Odin implementation, the id is a *string*.

Data is a Datum.

Cause is a *2-tuple*
1. the ISU that is handling the message (an `ė` (spelled `eh` in ASCII))
2. the Message being handled.

`ISU` means Isolated Software Unit.  An ISU is a piece of code with 0 dependency leaks. It is like a procedure with a set of inputs *and* a set of outputs.  It cannot call procedures in other ISUs, it can only send messages to other ISUs.  Message sending is like IPCs in processes, not so-called Message Sending in Smalltalk (which is a just a form of function calling with named parameters).  If an ISU calls functions, those functions must be contained within the same ISU.  If you were to draw a diagram of an ISU, it would be a rectangle (or other closed figure) with input ports and output ports (see the blue rectangles with ports in the above diagrams). The ports are not hard-wired to other parts of the system.  An ISU is totally isolated and self-contained.  See below for a discussion of parameters.

In 0D, we use the name Component to mean ISU.

`Cause` allows programmers to track the *provenance* of Messages.  Since each Message contains its cause, we can track back to the beginning of time, by following back-links.

A Datum is like a simplified object, with (at least) the following fields:
- data:     DatumData - a lump of data only known/accessible/mutable by the functions associated with this kind of Datum
- clone:   impure_function (^Datum) -> ^Datum -- an impure function that makes a deep copy of the given Datum, ensures that this copy is in the heap, and returns a pointer to this new copy
- reclaim:  proc (^Datum) -- a procedure that garbage collects the given Datum, deeply
- repr:     function (^Datum) -> string -- a function that returns a string representing the contents of the Datum
- kind:     function ()       -> string -- returns a string that uniquely identifies the type of the this Datum
- raw:      function (^Datum) -> []byte -- returns a byte array (essentially a fat pointer) to the contiguous bytes that make up the given Datum (a fat pointer is a pair {pointer,length})


# Appendix - Parameters and Return Values

A group of parameters passed to a function in modern languages, is actually just a *single*, contiguous blob of data that is deconstructed into multiple data types.  You can see this in assembler code.  In assembler, we see that values are placed into a array, pointed to be the Stack Pointer.  The parameter list is a heterogenous array (actually a CDR-less List) of various kinds of data.  The receiving procedure immediately deconstructs the bytes in this array and acts like it has several typed data structures in the array.

Likewise, output values are just single, contiguous blobs of data that can be deconstructed into multiple types.

True multiple input parameters, are blobs of data that arrive at *different times* on different ports True multiple output parameters, are blobs of data that are created at different times on different output ports.

A parameter list in conventional programming languages, is just a single input port.  A complete blob of data arrives - all at once, with no time separation - at the input port.  The receiving procedure / function immediately deconstructs the blob into distinct types of data.  The input data, though, all arrives at once grouped together into a homogenous blob of data, hence, it is but a single input, regardless of how it is deconstructed.


