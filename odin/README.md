# 0D - Zero Dependencies - and Drawware

Convert diagrams in src/*.drawio to executable code.

## Steps
1. Parse iagrams, with given syntax rules, and emit the diagram as JSON.
2. Inhale the JSON and execute.

# Details

This version uses the Odin language internally.

More languages are expected.

Fundamentally, internal language doesn't matter when designing a solution to a problem.

If you need to optimize the solution (a Big If), then internal language and niggly details do matter.

# Syntax Rules 
## Overview
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
![[DPL syntax-bare component.drawio.svg]]
### Container
![[DPL syntax-Container.drawio.svg]]
### Leaf
![[DPL syntax-Leaf.drawio.svg]]
### input gate
![[DPL syntax-input gate.drawio.svg]]
### output gate 
![[DPL syntax-output port.drawio.svg]]
### input port
![[DPL syntax-input port.drawio.svg]]
### output port
![[DPL syntax-output port.drawio.svg]]
### connection
![[DPL syntax-connection.drawio.svg]]
### VSH component
![[DPL syntax-vsh component.drawio.svg]]
### comment
![[DPL syntax-comment.drawio.svg]]

## Visual Semantics
### down
![[DPL syntax-down.drawio.svg]]
### up
![[DPL syntax-up.drawio.svg]]
### across
![[DPL syntax-across.drawio.svg]]
### through
![[DPL syntax-through.drawio.svg]]
### fan-out (split)
![[DPL syntax-fan-out (split).drawio.svg]]
### fan-in (join)
![[DPL syntax-fan-in (join).drawio.svg]]

## Style, Readability
### opacity
![[DPL syntax-opacity.drawio.svg]]
### line style
![[DPL syntax-line style.drawio.svg]]
### line thickness
![[DPL syntax-line thickness.drawio.svg]]

## Idioms
### feedback
![[DPL syntax-feedback.drawio.svg]]
### sequential
![[DPL syntax-sequential.drawio.svg]]
### parallel
![[DPL syntax-parallel.drawio.svg]]
### concurrency
![[DPL syntax-concurrency.drawio.svg]]
### errors, exceptions
![[DPL syntax-errrors, exceptions.drawio.svg]]

## Low Leval Technicalities
### kickoff inject
![[DPL syntax-kickoff inject.drawio.svg]]
### handle ()
![[DPL syntax-handle().drawio.svg]]
### send ()
![[DPL syntax-send().drawio.svg]]
### message copying
![[DPL syntax-message copying.drawio.svg]]
### active and idle
![[DPL syntax-active and idle.drawio.svg]]
### notes
![[DPL syntax-notes.drawio.svg]]

# Examples
## Basics
## Drawio
## VSH
## Dev0D
