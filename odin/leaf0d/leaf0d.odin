package leaf0d

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

