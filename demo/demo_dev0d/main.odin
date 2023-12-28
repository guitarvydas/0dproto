package dev0d

import "core:fmt"
import "core:log"
import "core:runtime"

import zd "../../0d/odin/0d"
import "../../0d/odin/std"

main :: proc() {
    context.logger = std.log (zd.log_all) // see ../../0d/odin/std/lib.odin for other options
    diagram_name, main_container_name := std.parse_command_line_args ("<specify on command line>", "main")
    palette := std.initialize_component_palette (diagram_name, components_to_include_in_project)
    std.run_demo_debug (&palette, main_container_name, diagram_name, start_function)
}

start_function :: proc (main_container : ^zd.Eh) {
    b := zd.new_datum_bang ()
    msg := zd.make_message("input", b, zd.make_cause (main_container, nil) )
    main_container.handler(main_container, msg)
}

components_to_include_in_project :: proc (leaves: ^[dynamic]zd.Leaf_Template) {
    // examples:
    //    zd.append_leaf (leaves, zd.Leaf_Template { name = "trash", instantiate = trash_instantiate })
    //    zd.append_leaf (leaves, std.string_constant ("rwr.ohm"))
}

