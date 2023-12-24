package agency

import reg  "../engine/registry0d"
import zd   "../engine/0d"
import std "../std"
import libdev "../libdev0d"


main :: proc() {
    diagram_name, main_container_name := lib.parse_command_line_args ("<specify on command line>", "main")
    palette := lib.initialize_component_palette (diagram_name, components_to_include_in_project)
    lib.run_demo (&palette, main_container_name, diagram_name, start_function)
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



