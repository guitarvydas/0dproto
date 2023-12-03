## Context

fmt.assertf on assertion failure, crashes with bad trap


### Operating System & Odin Version:

MacOS 13.6.1 (22G313)
odin version dev-2023-11:3c021f9c

* Please paste `odin report` output:

## Expected Behavior

My code *should* assert fail and print a message.


## Current Behavior

Instead it crashes. Bad trap.

## Failure Information (for bugs)

The code is in 0d/odin/registry0d.

Note that the fmt.printf on child_decl.name works, but the fmt.assertf crashes...

```
...
container_instantiator :: proc(reg: ^Component_Registry, owner : ^zd.Eh, name_prefix: string, decl: ir.Container_Decl) -> ^zd.Eh {

    ...
    {
        for child_decl in decl.children {
            fmt.printf ("  DEBUG: child_decl %v\n", child_decl)
            fmt.printf ("  DEBUG: child_decl.name %v\n", child_decl.name)
            ...
            fmt.assertf (ok, "\n*** Error: Can't find component %v\n", child_decl.name)
...
```


### Steps to Reproduce

- clone https://github.com/guitarvydas/0d.git
- git checkout bug3
- cd odin # ensure that pwd is ..../0d/odin
lldb agency.bin ../src/agency.drawio
- > run
- > bt

frame #5 calls fmt.assertf, then frame #0 crashes (len=... is suspicious, but looks OK in frame #1)

### Failure Logs

- contact me if more info required paultarvydas@gmail.com
