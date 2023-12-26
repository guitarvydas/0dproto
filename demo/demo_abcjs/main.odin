package abcjs

import zd "../../0d/odin/0d"
import "../../0d/odin/std"

main :: proc() {
    context.logger = std.log (zd.log_light_handlers)
    diagram_name, main_container_name := std.parse_command_line_args ("<specify on command line>", "main")
    palette := std.initialize_component_palette (diagram_name, components_to_include_in_project)
    std.run_demo (&palette, main_container_name, diagram_name, start_function)
}

start_function :: proc (main_container : ^zd.Eh) {
    filename := zd.new_datum_string ("demo/demo_abcjs/test.abc")
    msg := zd.make_message("input", filename, zd.make_cause (main_container, nil) )
    main_container.handler(main_container, msg)
}


components_to_include_in_project :: proc (leaves: ^[dynamic]zd.Leaf_Template) {
    zd.append_leaf (leaves, std.string_constant ("ABC"))
    zd.append_leaf (leaves, std.string_constant ("demo/demo_abcjs/abc.ohm"))
    zd.append_leaf (leaves, std.string_constant ("demo/demo_abcjs/abcjs.rwr"))
    zd.append_leaf (leaves, std.string_constant ("demo/demo_abcjs/null.js"))
    // examples:
    //    zd.append_leaf (leaves, zd.Leaf_Template { name = "trash", instantiate = trash_instantiate })
    //    zd.append_leaf (leaves, std.string_constant ("rwr.ohm"))
}



