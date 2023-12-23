// various useful pre-build components
// the components should be loaded-on-demand, but, for now, we'll just EVERYTHING in here...

package lib

import "core:fmt"
import "core:runtime"
import "core:log"
import "core:strings"
import "core:slice"
import "core:os"
import "core:unicode/utf8"

import reg "../registry0d"
import "../process"
import "../../ir"
import zd "../0d"

//////////
// Bang
// create a simple Bang datum
// for use in creating an event, where the data it contains doesn't matter
// like an "edge" in EE
//////////

bang_instantiate :: proc(name: string, owner : ^zd.Eh) -> ^zd.Eh {
    name_with_id := gensym("bang[%d]")
    return zd.make_leaf (name_with_id, owner, nil, bang_handle)
}

bang_handle :: proc(eh: ^zd.Eh, msg: ^zd.Message) {
    zd.send (eh, "output", zd.new_datum_bang (), msg)
}

//////////
// Support for $ (VSH - Visual SHell) components
//////////

stdout_instantiate :: proc(name_prefix: string,name: string, owner : ^zd.Eh) -> ^zd.Eh {
    return zd.make_leaf(name_prefix, name, owner, nil, stdout_handle)
}

stdout_handle :: proc(eh: ^zd.Eh, msg: ^zd.Message) {
    fmt.printf("%#v", msg.datum)
}

process_instantiate :: proc(name_prefix: string, name: string, owner : ^zd.Eh) -> ^zd.Eh {
    command_string := strings.clone(strings.trim_left(name, "$ "))
    command_string_ptr := new_clone(command_string)
    return zd.make_leaf(name_prefix, name, owner, command_string_ptr^, process_handle)
}

process_handle :: proc(eh: ^zd.Eh, msg: ^zd.Message) {
    
    utf8_string :: proc(bytes: []byte) -> (s: string, ok: bool) {
        s = string(bytes)
        ok = utf8.valid_string(s)
        return
    }
    
    send_output :: proc(eh: ^zd.Eh, port: string, output: []byte, causingMsg: ^zd.Message) {
        if len(output) > 0 {
            zd.send (eh, port, zd.new_datum_bytes (output), causingMsg)
        }
    }

    switch msg.port {
    case "input":
        handle := process.process_start(eh.instance_data.(string))
        defer process.process_destroy_handle(handle)

        // write input, wait for finish
        {
	    switch msg.datum.kind () {
	    case "string":
                os.write(handle.input, msg.datum.raw (msg.datum))
	    case "bytes":
                os.write(handle.input, msg.datum.raw (msg.datum))
	    case "bang":
                // OK, no input, just run it
	    case:
                log.errorf("%s: Shell leaf input can handle string, bytes, or bang (got: %v)",
			   eh.name, msg.datum.kind ())
            }
            os.close(handle.input)
            process.process_wait(handle)
        }

        // breaks bootstrap error check, thus, removed line: zd.send_string (eh, "done", Bang{})

        // stdout handling
        {
            stdout, stdout_ok := process.process_read_handle(handle.output)

            // stderr handling
            stderr_untrimmed, stderr_ok := process.process_read_handle(handle.error)
	    stderr : string
            if stderr_ok {
		stderr = strings.trim_right_space(cast(string)stderr_untrimmed)
            }
	    if stdout_ok && stderr_ok {
		// fire only one output port
		// on error, send both, stdout and stderr to the error port
		if len (stderr) > 0 {
                    send_output (eh, "error", stdout, msg)
		    zd.send_string(eh, "error", stderr, msg)
		} else {
                    zd.send_string (eh, "output", transmute(string)stdout, msg)
		}
	    } else {
		// panic - we should never fail to collect stdout and stderr
		// if we come here, then something is deeply wrong with this code
		fmt.assertf (false, "PANIC: failed to retrieve outputs stdout_ok = %v stderr_ok = %v\n", stdout_ok, stderr_ok)
            }
	}
    }
}

collect_process_leaves :: proc(diagram_name: string, leaves: ^[dynamic]reg.Leaf_Instantiator) {
    ref_is_container :: proc(decls: []ir.Container_Decl, name: string) -> bool {
        for d in decls {
            if d.name == name {
                return true
            }
        }
        return false
    }

    decls := reg.json2internal (diagram_name)
    defer delete(decls)

    // TODO(z64): while harmless, this doesn't ignore duplicate process decls yet.

    for decl in decls {
        for child in decl.children {
            if ref_is_container(decls[:], child.name) {
                continue
            }

            if strings.has_prefix(child.name, "$") {
                leaf_instantiate := reg.Leaf_Instantiator {
                    name = child.name,
                    instantiate = process_instantiate,
                }
                append(leaves, leaf_instantiate)
            }
        }
    }
}

////

Command_Instance_Data :: struct {
    buffer : string
}

command_instantiate :: proc(name_prefix: string, name: string, owner : ^zd.Eh) -> ^zd.Eh {
    name_with_id := zd.gensym("command")
    instp := new (Command_Instance_Data)
    return zd.make_leaf (name_prefix, name_with_id, owner, instp^, command_handle)
}

command_handle :: proc(eh: ^zd.Eh, msg: ^zd.Message) {
    inst := eh.instance_data.(Command_Instance_Data)
    switch msg.port {
    case "command":
        inst.buffer = msg.datum.repr (msg.datum)
        received_input := msg.datum.repr (msg.datum)
        captured_output, _ := process.run_command (inst.buffer, received_input)
        zd.send_string (eh, "output", captured_output, msg)
	case:
        fmt.assertf (false, "!!! ERROR: command got an illegal message port %v", msg.port)
    }
}

icommand_instantiate :: proc(name_prefix: string, name: string, owner : ^zd.Eh) -> ^zd.Eh {
    name_with_id := zd.gensym("icommand[%d]")
    instp := new (Command_Instance_Data)
    return zd.make_leaf (name_prefix, name_with_id, owner, instp^, icommand_handle)
}

icommand_handle :: proc(eh: ^zd.Eh, msg: ^zd.Message) {
    inst := eh.instance_data.(Command_Instance_Data)
    switch msg.port {
    case "command":
        inst.buffer = msg.datum.repr (msg.datum)
    case "input":
        received_input := msg.datum.repr (msg.datum)
        captured_output, _ := process.run_command (inst.buffer, received_input)
        zd.send_string (eh, "output", captured_output, msg)
	case:
        fmt.assertf (false, "!!! ERROR: command got an illegal message port %v", msg.port)
    }
}

/////////

probe_instantiate :: proc(name_prefix: string, name: string, owner : ^zd.Eh) -> ^zd.Eh {
    name_with_id := zd.gensym("?")
    return zd.make_leaf (name_prefix, name_with_id, owner, nil, probe_handle)
}

probe_handle :: proc(eh: ^zd.Eh, msg: ^zd.Message) {
    s := msg.datum.repr (msg.datum)
    fmt.eprintf ("probe %v: /%v/ len=%v\n", eh.name, s, len (s))
}

trash_instantiate :: proc(name_prefix: string, name: string, owner : ^zd.Eh) -> ^zd.Eh {
    name_with_id := zd.gensym("trash")
    return zd.make_leaf (name_prefix, name_with_id, owner, nil, trash_handle)
}

trash_handle :: proc(eh: ^zd.Eh, msg: ^zd.Message) {
    // to appease dumped-on-floor checker
}

//////////
// time-ordering
// Deracer
//////////


TwoMessages :: struct {
    first : ^zd.Message,
    second : ^zd.Message
}


Deracer_States :: enum { idle, waitingForFirst, waitingForSecond }

Deracer_Instance_Data :: struct {
    state : Deracer_States,
    buffer : TwoMessages
}

reclaim_Buffers_from_heap :: proc (inst : ^Deracer_Instance_Data) {
    zd.destroy_message (inst.buffer.first)
    zd.destroy_message (inst.buffer.second)
}

deracer_instantiate :: proc(name: string, owner : ^zd.Eh) -> ^zd.Eh {
    name_with_id := gensym ("deracer")
    inst := new (Deracer_Instance_Data) // allocate in the heap
    inst.state = .idle
    eh := zd.make_leaf (name_with_id, owner, inst^, deracer_handle)
    return eh
}

send_first_then_second :: proc (eh : ^zd.Eh, inst: ^Deracer_Instance_Data) {
    zd.forward (eh, "1", inst.buffer.first)
    zd.forward (eh, "2", inst.buffer.second)
    reclaim_Buffers_from_heap (inst)
}

deracer_handle :: proc(eh: ^zd.Eh,  msg: ^zd.Message) {
    inst := &eh.instance_data.(Deracer_Instance_Data)
    switch (inst.state) {
    case .idle:
        switch msg.port {
        case "1":
            inst.buffer.first = msg
            inst.state = .waitingForSecond
        case "2":
            inst.buffer.second = msg
            inst.state = .waitingForFirst
        case:
            fmt.assertf (false, "bad msg.port A for deracer %v\n", msg.port)
        }
    case .waitingForFirst:
        switch msg.port {
        case "1":
            inst.buffer.first = msg
            send_first_then_second (eh, inst)
            inst.state = .idle
        case:
            fmt.assertf (false, "bad msg.port B for deracer %v\n", msg.port)
        }
    case .waitingForSecond:
        switch msg.port {
        case "2":
            inst.buffer.second = msg
            send_first_then_second (eh, inst)
            inst.state = .idle
        case:
            fmt.assertf (false, "bad msg.port C for deracer %v\n", msg.port)
        }
    case:
        fmt.assertf (false, "bad state for deracer %v\n", eh.state)
    }
}

//////////
// type-checking
// null tester
//////////

nulltester_instantiate :: proc(name: string, owner : ^zd.Eh) -> ^zd.Eh {
    name_with_id := gensym("nulltester")
    return zd.make_leaf (name_with_id, owner, nil, nulltester_handle)
}

nulltester_handle :: proc(eh: ^zd.Eh, msg: ^zd.Message) {
    if "" == msg.datum.data.(string) {
	zd.send_string (eh, "null", "", msg)
    } else {
	zd.send_string (eh, "str", msg.datum.data.(string), msg)
    }
}

//////////
// type-checking
// forward input only if it is a string, else send a message on "error"
//////////

ensure_string_datum_instantiate :: proc(name: string, owner : ^zd.Eh) -> ^zd.Eh {
    name_with_id := gensym("Ensure String Datum")
    return zd.make_leaf (name_with_id, owner, nil, ensure_string_datum_handle)
}

ensure_string_datum_handle :: proc(eh: ^zd.Eh, msg: ^zd.Message) {
    switch x in msg.datum.data {
    case string:
	zd.forward (eh, "output", msg)
    case:
	zd.send_string (eh, "error", "ensure: type error (expected a string datum)", msg)
    }
}


//////////
// File I/O
// read, write, fakepipe
// (a fakepipe is a temporary file scribbled onto disk (say /tmp) to fake out unnamed pipes for operating systems that do not support the concept of true pipes)
//////////

low_level_read_text_file_instantiate :: proc(name: string, owner : ^zd.Eh) -> ^zd.Eh {
    name_with_id := gensym("Low Level Read Text File")
    return zd.make_leaf (name_with_id, owner, nil, low_level_read_text_file_handle)
}

low_level_read_text_file_handle :: proc(eh: ^zd.Eh, msg: ^zd.Message) {
    fname := msg.datum.data.(string)
    fd, errnum := os.open (fname)
    if errnum == 0 {
	data, success := os.read_entire_file_from_handle (fd)
	if success {
	    zd.send_string (eh, "str", transmute(string)data, msg)
	} else {
            emsg := fmt.aprintf("read error on file %s", msg.datum.data.(string))
	    zd.send_string (eh, "error", emsg, msg)
	}
    } else {
        emsg := fmt.aprintf("open error on file %s with error code %v", msg.datum.data.(string), errnum)
	zd.send_string (eh, "error", emsg, msg)
    }
}


open_text_file_instantiate :: proc(name: string, owner : ^zd.Eh) -> ^zd.Eh {
    name_with_id := gensym("Open Text File")
    return zd.make_leaf (name_with_id, owner, nil, open_text_file_handle)
}

open_text_file_handle :: proc(eh: ^zd.Eh, msg: ^zd.Message) {
    fd, errnum := os.open (msg.datum.data.(string))
    if errnum == 0 {
	zd.send (eh, "fd", zd.new_datum_handle (fd), msg)
    } else {
        emsg := fmt.aprintf("open error on file %s with error code %v", msg.datum.data.(string), errnum)
	zd.send_string (eh, "error", emsg, msg)
    }
}

read_text_from_fd_instantiate :: proc(name: string, owner : ^zd.Eh) -> ^zd.Eh {
    name_with_id := gensym("Low Level Read Text From FD")
    return zd.make_leaf (name_with_id, owner, nil, low_level_read_text_from_fd_handle)
}

low_level_read_text_from_fd_handle :: proc(eh: ^zd.Eh, msg: ^zd.Message) {
    fd := msg.datum.data.(os.Handle)
    data, success := os.read_entire_file_from_handle (fd)
    if success {
	zd.send_string (eh, "str", transmute(string)data, msg)
    } else {
        emsg := fmt.aprintf("read error on file %s", msg.datum.data.(string))
	zd.send_string (eh, "error", emsg, msg)
    }
}

// syncfilewrite sends a Bang on "done" - this will cause a panic ("message dropped on floor") if the "done" output pin is not
// wired to <something> - wire it to Trash if you don't need it

Syncfilewrite_Data :: struct {
    filename : string
}

syncfilewrite_instantiate :: proc(name: string, owner : ^zd.Eh) -> ^zd.Eh {
    name_with_id := gensym("syncfilewrite")
    instp := new (Syncfilewrite_Data)
    return zd.make_leaf (name_with_id, owner, instp^, syncfilewrite_handle)
}

syncfilewrite_handle :: proc(eh: ^zd.Eh, msg: ^zd.Message) {
    inst := &eh.instance_data.(Syncfilewrite_Data)
    switch msg.port {
    case "filename":
	inst.filename = msg.datum.data.(string)
    case "stdin":
	contents := msg.datum.data.(string)
	ok := os.write_entire_file (inst.filename, transmute([]u8)contents, true)
	if !ok {
	    zd.send_string (eh, "stderr", "write error", msg)
	} else {
	    zd.send (eh, "done", zd.new_datum_bang (), msg)
	}
    }
}

fakepipename_instantiate :: proc(name: string, owner : ^zd.Eh) -> ^zd.Eh {
    name_with_id := gensym("fakepipename")
    return zd.make_leaf (name_with_id, owner, nil, fakepipename_handle)
}

fakepipename_handle :: proc(eh: ^zd.Eh, msg: ^zd.Message) {
    @(static) rand := 0
    rand += 1
    zd.send_string (eh, "output", fmt.aprintf ("/tmp/fakepipename%d", rand), msg)
}


//////////
// Panic
// hard, immediate stop
// strictly, not necessary, but, convenient to use in Proofs of Concept, where ungraceful error handling is OK
//////////

panic_instantiate :: proc(name: string, owner : ^zd.Eh) -> ^zd.Eh {
    name_with_id := gensym("panic")
    return zd.make_leaf (name_with_id, owner, nil, panic_handle)
}

panic_handle :: proc(eh: ^zd.Eh, msg: ^zd.Message) {
    fmt.println ("PANIC: ", msg.datum.data.(string))
    // assert (false, msg.datum.data.(string))
}

//////////
// Concatenate two strings, 1 then 2, and output the concatenated result
// re-orders the strings, so that 1 always precedes 2, regardless of arrival order
//////////

Concat_Instance_Data :: struct {
    buffer : string,
}

empty_string := ""

concat_instantiate :: proc(name: string, owner : ^zd.Eh) -> ^zd.Eh {
    name_with_id := gensym("concat")
    instp := new (Concat_Instance_Data)
    instp.buffer = strings.clone (empty_string)
    return zd.make_leaf (name_with_id, owner, instp^, concat_handle)
}

concat_handle :: proc(eh: ^zd.Eh, msg: ^zd.Message) {
    inst := &eh.instance_data.(Concat_Instance_Data)
    switch (msg.port) {
    case "str":
	delete (inst.buffer)
	inst.buffer = fmt.aprintf ("%s%s", inst.buffer, msg.datum.data.(string))
    case "flush":
	zd.send_string (eh, "str", inst.buffer, msg)
	delete (inst.buffer)
	inst.buffer = ""
    }
}
