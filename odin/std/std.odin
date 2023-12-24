package std

import "core:fmt"
import "core:runtime"
import "core:log"
import "core:strings"
import "core:slice"
import "core:os"
import "core:unicode/utf8"

import reg "../engine/registry0d"
import "../engine/process"
import "../../ir"
import zd "../engine/0d"
import "../std"

///// exported

gensym :: proc (s : string) -> string {
    @(static) counter := 0
    counter += 1
    name_with_id := fmt.aprintf("%s◦%d", s, counter)
    return name_with_id
}

string_constant :: proc (str: string) -> reg.Leaf_Template {
    quoted_name := fmt.aprintf ("'%s'", str)
    return reg.Leaf_Template { name = quoted_name, instantiate = literal_instantiate }
}

initialize :: proc(diagram_name: string) -> [dynamic]reg.Leaf_Instantiator {
    leaves : [dynamic]reg.Leaf_Instantiator = make([dynamic]reg.Leaf_Instantiator)
    collect_process_leaves (diagram_name, &leaves)
    // export standard native leaves
    append(&leaves, reg.Leaf_Instantiator {
        name = "stdout",
        instantiate = std.stdout_instantiate,
    })
    append(&leaves, reg.Leaf_Template { name = "1then2", instantiate = std.deracer_instantiate })
    append(&leaves, reg.Leaf_Template { name = "?", instantiate = std.probe_instantiate })
    append(&leaves, reg.Leaf_Template { name = "trash", instantiate = std.trash_instantiate })

    append(&leaves, reg.Leaf_Template { name = "Low Level Read Text File", instantiate = std.low_level_read_text_file_instantiate })
    append(&leaves, reg.Leaf_Template { name = "Read Text From FD", instantiate = std.read_text_from_fd_instantiate })
    append(&leaves, reg.Leaf_Template { name = "Open Text File", instantiate = std.open_text_file_instantiate })
    append(&leaves, reg.Leaf_Template { name = "Ensure String Datum", instantiate = std.ensure_string_datum_instantiate })

    append(&leaves, reg.Leaf_Template { name = "syncfilewrite", instantiate = std.syncfilewrite_instantiate })
    append(&leaves, reg.Leaf_Template { name = "Bang", instantiate = std.bang_instantiate })
    append(&leaves, reg.Leaf_Template { name = "stringconcat", instantiate = std.stringconcat_instantiate })

    return leaves
}

//// helpers

stdout_instantiate :: proc(name: string, owner : ^zd.Eh) -> ^zd.Eh {
    return zd.make_leaf(name, owner, nil, stdout_handle)
}

stdout_handle :: proc(eh: ^zd.Eh, msg: ^zd.Message) {
    fmt.printf("%#v", msg.datum)
}

process_instantiate :: proc(name: string, owner : ^zd.Eh) -> ^zd.Eh {
    command_string := strings.clone(strings.trim_left(name, "$ "))
    command_string_ptr := new_clone(command_string)
    return zd.make_leaf(name, owner, command_string_ptr^, process_handle)
}

process_handle :: proc(eh: ^zd.Eh, msg: ^zd.Message) {
    
    utf8_string :: proc(bytes: []byte) -> (s: string, ok: bool) {
        s = string(bytes)
        ok = utf8.valid_string(s)
        return
    }
    

    switch msg.port {
    case "input":
	cmd := eh.instance_data.(string)
        handle := process.process_start(cmd)
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
		    fmt.printf ("%v: %v\n", cmd, stderr) // debug
		    zd.send_string(eh, "error", fmt.aprintf ("%v: %v", cmd, stderr), msg)
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
    defer reg.delete_decls (decls)

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

command_instantiate :: proc(name: string, owner : ^zd.Eh) -> ^zd.Eh {
    name_with_id := zd.gensym("command")
    instp := new (Command_Instance_Data)
    return zd.make_leaf (name_with_id, owner, instp^, command_handle)
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

icommand_instantiate :: proc(name: string, owner : ^zd.Eh) -> ^zd.Eh {
    name_with_id := zd.gensym("icommand[%d]")
    instp := new (Command_Instance_Data)
    return zd.make_leaf (name_with_id, owner, instp^, icommand_handle)
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

probe_instantiate :: proc(name: string, owner : ^zd.Eh) -> ^zd.Eh {
    name_with_id := zd.gensym("?")
    return zd.make_leaf (name_with_id, owner, nil, probe_handle)
}

probe_handle :: proc(eh: ^zd.Eh, msg: ^zd.Message) {
    s := msg.datum.repr (msg.datum)
    fmt.eprintf ("probe %v: /%v/ len=%v\n", eh.name, s, len (s))
}

trash_instantiate :: proc(name: string, owner : ^zd.Eh) -> ^zd.Eh {
    name_with_id := zd.gensym("trash")
    return zd.make_leaf (name_with_id, owner, nil, trash_handle)
}

trash_handle :: proc(eh: ^zd.Eh, msg: ^zd.Message) {
    // to appease dumped-on-floor checker
}

////////
literal_instantiate :: proc (name: string, owner : ^zd.Eh) -> ^zd.Eh {
    i := strings.index_rune (name, '\'')
    quoted := name [i+1:(len (name) - 1)]
    name_with_id := gensym(name)
    pstr := string_dup_to_heap (quoted)
    return zd.make_leaf (name_with_id, owner, pstr^, literal_handle)
}

literal_handle :: proc(eh: ^zd.Eh, msg: ^zd.Message) {
    zd.send_string (eh, "output", eh.instance_data.(string), msg)
}

string_dup_to_heap :: proc (s : string) -> ^string{
    heap_s := new (string)
    heap_s^ = strings.clone (s)
    return heap_s
}

////////

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

/////////

////////

low_level_read_text_file_instantiate :: proc(name: string, owner : ^zd.Eh) -> ^zd.Eh {
    name_with_id := gensym("Low Level Read Text File")
    return zd.make_leaf (name_with_id, owner, nil, low_level_read_text_file_handle)
}

low_level_read_text_file_handle :: proc(eh: ^zd.Eh, msg: ^zd.Message) {
    fname := msg.datum.repr (msg.datum)
    fd, errnum := os.open (fname)
    if errnum == 0 {
	data, success := os.read_entire_file_from_handle (fd)
	if success {
	    zd.send_string (eh, "str", transmute(string)data, msg)
	} else {
            emsg := fmt.aprintf("read error on file %s", msg.datum.repr (msg.datum))
	    zd.send_string (eh, "error", emsg, msg)
	}
    } else {
        emsg := fmt.aprintf("open error on file %s with error code %v", msg.datum.repr (msg.datum), errnum)
	zd.send_string (eh, "error", emsg, msg)
    }
}


////////
open_text_file_instantiate :: proc(name: string, owner : ^zd.Eh) -> ^zd.Eh {
    name_with_id := gensym("Open Text File")
    return zd.make_leaf (name_with_id, owner, nil, open_text_file_handle)
}

open_text_file_handle :: proc(eh: ^zd.Eh, msg: ^zd.Message) {
    fd, errnum := os.open (msg.datum.repr (msg.datum))
    if errnum == 0 {
	zd.send (eh, "fd", zd.new_datum_handle (fd), msg)
    } else {
        emsg := fmt.aprintf("open error on file %s with error code %v", msg.datum.repr (msg.datum), errnum)
	zd.send_string (eh, "error", emsg, msg)
    }
}

////////

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
        emsg := fmt.aprintf("read error on file %s", msg.datum.repr (msg.datum))
	zd.send_string (eh, "error", emsg, msg)
    }
}

////////
ensure_string_datum_instantiate :: proc(name: string, owner : ^zd.Eh) -> ^zd.Eh {
    name_with_id := gensym("Ensure String Datum")
    return zd.make_leaf (name_with_id, owner, nil, ensure_string_datum_handle)
}

ensure_string_datum_handle :: proc(eh: ^zd.Eh, msg: ^zd.Message) {
    switch msg.datum.kind () {
    case "string":
	zd.forward (eh, "output", msg)
    case:
	emsg := fmt.aprintf ("*** ensure: type error (expected a string datum) but got %v in %v", msg.datum.kind (), msg)
	zd.send_string (eh, "error", emsg, msg)
    }
}

////////

Syncfilewrite_Data :: struct {
    filename : string
}

// temp copy for bootstrap, sends "done" (error during bootstrap if not wired)

syncfilewrite_instantiate :: proc(name: string, owner : ^zd.Eh) -> ^zd.Eh {
    name_with_id := gensym("syncfilewrite")
    inst := new (Syncfilewrite_Data)
    return zd.make_leaf (name_with_id, owner, inst^, syncfilewrite_handle)
}

syncfilewrite_handle :: proc(eh: ^zd.Eh, msg: ^zd.Message) {
    inst := &eh.instance_data.(Syncfilewrite_Data)
    switch msg.port {
    case "filename":
	inst.filename = msg.datum.repr (msg.datum)
    case "input":
	contents := msg.datum.repr (msg.datum)
	// see .../Odin/core/os.odin/write_entire_file - the following code was stolen from there
	mode: int = 0
	when os.OS == .Linux || os.OS == .Darwin {
		// NOTE(justasd): 644 (owner read, write; group read; others read)
		mode = os.S_IRUSR | os.S_IWUSR | os.S_IRGRP | os.S_IROTH
	}
	fd, open_errnum := os.open (path = inst.filename, flags =  os.O_WRONLY|os.O_CREATE, mode = mode)
	if open_errnum == 0 {
	    numchars, write_errnum := os.write (fd, transmute([]u8)contents)
	    if write_errnum == 0 {
		zd.send (eh, "done", zd.new_datum_bang (), msg)
	    } else {
		zd.send_string (eh, "error", fmt.aprintf("/%v/: %v", inst.filename, write_errnum), msg)
		zd.send_string (eh, "error", "write error", msg)
	    }
	} else {
	    zd.send_string (eh, "error", fmt.aprintf("/%v/: %v", inst.filename, open_errnum), msg)
		zd.send_string (eh, "error", "open error", msg)
	}
    }
}

////////

///
bang_instantiate :: proc(name: string, owner : ^zd.Eh) -> ^zd.Eh {
    name_with_id := gensym("bang[%d]")
    return zd.make_leaf (name_with_id, owner, nil, bang_handle)
}

bang_handle :: proc(eh: ^zd.Eh, msg: ^zd.Message) {
    zd.send (eh, "output", zd.new_datum_bang (), msg)
}

///
StringConcat_Instance_Data :: struct {
    buffer : string
}

stringconcat_instantiate :: proc(name: string, owner : ^zd.Eh) -> ^zd.Eh {
    name_with_id := gensym("stringconcat")
    instp := new (StringConcat_Instance_Data)
    return zd.make_leaf (name_with_id, owner, instp^, stringconcat_handle)
}

stringconcat_handle :: proc(eh: ^zd.Eh, msg: ^zd.Message) {
    inst := &eh.instance_data.(StringConcat_Instance_Data)
    switch msg.port {
    case "1":
	inst.buffer = strings.clone (msg.datum.repr (msg.datum))
    case "2":
	s := strings.clone (msg.datum.repr (msg.datum))
	if 0 == len (inst.buffer) && 0 == len (s) {
	    fmt.printf ("stringconcat %d %d\n", len (inst.buffer), len (s))
	    fmt.assertf (false, "TODO: something is wrong in stringconcat, both strings are 0 length\n")
	}
	concatenated_string : string
	if 0 == len (inst.buffer) {
	    concatenated_string = fmt.aprintf ("%s", s)
	} else if 0 == len (s) {
	    concatenated_string = fmt.aprintf ("%s", inst.buffer)
	} else {
	    concatenated_string = fmt.aprintf ("%s%s", inst.buffer, s)
	}
	zd.send_string (eh, "output", concatenated_string, msg)
     case:
        fmt.assertf (false, "bad msg.port for stringconcat: %v\n", msg.port)
    }
}

////////
