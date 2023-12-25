package dev0d

import "core:fmt"
import "core:log"
import "core:runtime"

import reg  "../engine/registry0d"
import zd   "../engine/0d"
import std "../std"

main :: proc() {
    diagram_name, main_container_name := std.parse_command_line_args ("<specify on command line>", "main")
    palette := std.initialize_component_palette (diagram_name, components_to_include_in_project)

      //log_level := zd.log_light_handlers // set this to only track handlers in Components
      //log_level := zd.log_full_handlers // set this to only track handlers, in full glory, in Components
      // log_level := runtime.Logger_Level.Info
    log_level := zd.log_all // set this to track everything, equivalent to runtime.Logger_Level.Debug
    fmt.printf ("\n*** starting logger level %v ***\n", log_level)
    context.logger = log.create_console_logger(
	lowest=cast(runtime.Logger_Level)log_level,
        opt={.Level, .Time, .Terminal_Color},
    )

    std.run_demo_debug (&palette, main_container_name, diagram_name, start_function)
}

start_function :: proc (main_container : ^zd.Eh) {
    b := zd.new_datum_bang ()
    msg := zd.make_message("input", b, zd.make_cause (main_container, nil) )
    main_container.handler(main_container, msg)
}

components_to_include_in_project :: proc (leaves: ^[dynamic]reg.Leaf_Template) {
    // examples:
    //    reg.append_leaf (&leaves, reg.Leaf_Template { name = "trash", instantiate = trash_instantiate })
    //    reg.append_leaf (&leaves, string_constant ("rwr.ohm"))
}

