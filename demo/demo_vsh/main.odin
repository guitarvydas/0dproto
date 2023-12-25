package vsh

import zd   "../0d"
import std "../std"


main :: proc() {
    diagram_name, main_container_name := std.parse_command_line_args ("<specify on command line>", "main")
    palette := std.initialize_component_palette (diagram_name, components_to_include_in_project)
    std.run_demo (&palette, main_container_name, diagram_name, start_function)
}




start_function :: proc (main_container : ^zd.Eh) {
    b := zd.new_datum_bang ()
    msg := zd.make_message("input", b, zd.make_cause (main_container, nil) )
    main_container.handler(main_container, msg)
}


components_to_include_in_project :: proc (leaves: ^[dynamic]zd.Leaf_Template) {
    // examples:
    //    zd.append_leaf (&leaves, zd.Leaf_Template { name = "trash", instantiate = trash_instantiate })
    //    zd.append_leaf (&leaves, string_constant ("rwr.ohm"))
}
