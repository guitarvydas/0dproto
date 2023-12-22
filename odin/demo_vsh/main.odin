package vsh

import "core:fmt"
import "core:log"
import "core:runtime"
import "core:strings"
import "core:slice"
import "core:os"
import "core:unicode/utf8"

import reg  "../registry0d"
import zd   "../0d"
import lib "../lib0d"
import      "../debug"


main :: proc() {
    diagram_name, main_container_name := parse_command_line_args ()
    palette := initialize_component_palette (diagram_name)
    run (&palette, main_container_name, diagram_name, start_function)
}




start_function :: proc (main_container : ^zd.Eh) {
    b := zd.new_datum_bang ()
    msg := zd.make_message("input", b, zd.make_cause (main_container, nil) )
    main_container.handler(main_container, msg)
}


////////
project_specific_components :: proc (leaves: ^[dynamic]reg.Leaf_Template) {
    append(leaves, reg.Leaf_Template { name = "?", instantiate = lib.probe_instantiate })
    append(leaves, reg.Leaf_Template { name = "trash", instantiate = lib.trash_instantiate })
}


run :: proc (r : ^reg.Component_Registry, main_container_name : string, diagram_source_file : string, injectfn : #type proc (^zd.Eh)) {
    pregistry := r
    // get entrypoint container
    main_container, ok := reg.get_component_instance(pregistry, "", main_container_name, owner=nil)
    fmt.assertf(
        ok,
        "Couldn't find main container with page name %s in file %s (check tab names, or disable compression?)\n",
        main_container_name,
        diagram_source_file,
    )
    injectfn (main_container)
    print_error_maybe (main_container)
    print_output (main_container)
    fmt.println("\n\n--- done ---")
}


print_output :: proc (main_container : ^zd.Eh) {
    fmt.println("\n\n--- RESULT ---")
    fmt.printf ("...processes that include the name 'vsh' (ps | grep vsh)...\n")
    zd.print_specific_output (main_container, "ps")
    fmt.printf ("...number of processes + 1 (header) (ps | wc -l)...\n")
    zd.print_specific_output (main_container, "processes")
}
print_error_maybe :: proc (main_container : ^zd.Eh) {
    error_port := "error"
    dont_care, found := zd.fetch_first_output (main_container, error_port)
    if found {
	fmt.println("\n\n--- !!! ERRORS !!! ---")
	zd.print_specific_output (main_container, error_port)
    }
}

parse_command_line_args :: proc () -> (diagram_source_file, main_container_name: string) {
    diagram_source_file = slice.get(os.args, 1) or_else "<specify on command line>"
    main_container_name = slice.get(os.args, 2) or_else "main"
    
    if !os.exists(diagram_source_file) {
        fmt.println("Source diagram file", diagram_source_file, "does not exist.")
        os.exit(1)
    }
    return diagram_source_file, main_container_name
}

initialize_component_palette :: proc (diagram_source_file: string) -> (palette: reg.Component_Registry) {
    leaves := make([dynamic]reg.Leaf_Instantiator)

    // set up shell leaves
    lib.collect_process_leaves(diagram_source_file, &leaves)

    // export native leaves
    reg.append_leaf (&leaves, reg.Leaf_Instantiator {
        name = "stdout",
        instantiate = lib.stdout_instantiate,
    })

    project_specific_components (&leaves)

    containers := reg.json2internal (diagram_source_file)
    palette = reg.make_component_registry(leaves[:], containers)
    return palette
}



